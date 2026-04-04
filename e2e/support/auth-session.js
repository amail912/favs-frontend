const DEFAULT_API_BASE = "http://localhost:1234/api";
const DEFAULT_PASSWORD = "StrongPass123!";
const DEFAULT_ADMIN_USERNAME = "admin";
const DEFAULT_ADMIN_PASSWORD = process.env.E2E_ADMIN_PASSWORD || "averystrongpass";
const APPROVAL_RETRY_ATTEMPTS = 10;
const APPROVAL_RETRY_DELAY_MS = 250;
const AUTH_DEBUG = process.env.E2E_AUTH_DEBUG === "1";
const RUN_ID = Date.now().toString(36);
const identityRegistry = new Map();
const MAX_USERNAME_LENGTH = 32;

function sleep(milliseconds) {
  return new Promise(resolve => setTimeout(resolve, milliseconds));
}

function debugLog(message, details) {
  if (!AUTH_DEBUG) {
    return;
  }

  if (details === undefined) {
    console.log(`[auth-session] ${message}`);
    return;
  }

  console.log(`[auth-session] ${message}`, details);
}

function extractMessage(body) {
  if (!body) {
    return "";
  }

  try {
    const parsed = JSON.parse(body);
    return typeof parsed.message === "string" ? parsed.message : body;
  } catch {
    return body;
  }
}

function parseSessionCookie(setCookieHeader) {
  const setCookie = setCookieHeader || "";
  const cookiePair = setCookie.split(";")[0];
  const [cookieName, ...cookieValueParts] = cookiePair.split("=");
  const cookieValue = cookieValueParts.join("=").replace(/^"|"$/g, "");

  if (!cookieName || !cookieValue) {
    throw new Error(`Unable to parse session cookie: ${setCookie}`);
  }

  return { name: cookieName, value: cookieValue };
}

function buildCredentials(options = {}) {
  const apiBase = options.apiBase || DEFAULT_API_BASE;
  const password = options.password || DEFAULT_PASSWORD;
  const username =
    options.username ||
    `${options.usernamePrefix || "e2e"}_${Date.now()}_${Math.floor(Math.random() * 100000)}`;
  return { apiBase, username, password };
}

function identityRegistryKey(kind, reuseKey = kind) {
  return `${kind}:${reuseKey}`;
}

function compactToken(value, fallback) {
  const normalized = `${value || fallback}`
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, "-")
    .replace(/^-+|-+$/g, "");

  return normalized || fallback;
}

function buildRunScopedUsername(kind, reuseKey, usernamePrefix) {
  const prefix = compactToken(usernamePrefix, "e2e").slice(0, 3);
  const kindToken =
    kind === "approved-member"
      ? "am"
      : kind === "pending-member"
        ? "pm"
        : compactToken(kind, "u").slice(0, 2);
  const reuseToken = compactToken(reuseKey, kindToken).slice(0, 10);
  const base = `${prefix}-${kindToken}-${reuseToken}-${RUN_ID}`;

  return base.slice(0, MAX_USERNAME_LENGTH);
}

async function signupUser(options = {}) {
  const { apiBase, username, password } = buildCredentials(options);
  const payload = { username, password };

  const signup = await fetch(`${apiBase}/signup`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(payload)
  });

  debugLog("signupUser response", { username, status: signup.status });
  if (!signup.ok) {
    const body = await signup.clone().text();
    debugLog("signupUser non-ok", { username, status: signup.status, body });
  }
  return { signup, username, password };
}

async function signinUser(options = {}) {
  const { apiBase, username, password } = buildCredentials(options);
  const payload = { username, password };

  const signin = await fetch(`${apiBase}/signin`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(payload)
  });

  if (!signin.ok) {
    const body = await signin.text();
    debugLog("signinUser non-ok", { username, status: signin.status, body });
    throw new Error(`Signin failed with status ${signin.status}`);
  }

  return {
    username,
    password,
    cookie: parseSessionCookie(signin.headers.get("set-cookie"))
  };
}

function buildAdminCredentials(options = {}) {
  return buildCredentials({
    ...options,
    username: options.adminUsername || DEFAULT_ADMIN_USERNAME,
    password: options.adminPassword || DEFAULT_ADMIN_PASSWORD
  });
}

async function ensureUser(options = {}) {
  const { signup, username, password } = await signupUser(options);
  if (signup.ok) {
    return { username, password, alreadyExists: false };
  }

  const body = await signup.text();
  const message = extractMessage(body);

  // Backend returns 400 when a user already exists. Only that case is acceptable here.
  if (signup.status === 400 && message === "Unable to create user") {
    debugLog("ensureUser treating signup 400 as already exists", {
      username,
      status: signup.status,
      message
    });
    return { username, password, alreadyExists: true };
  }

  throw new Error(`Signup failed with status ${signup.status}: ${message || body}`);
}

async function signinAdmin(options = {}) {
  const { apiBase, username, password } = buildAdminCredentials(options);
  return signinUser({ apiBase, username, password });
}

async function ensureAdminUser(options = {}) {
  const { apiBase, username, password } = buildAdminCredentials(options);
  await ensureUser({ apiBase, username, password });
  return { apiBase, username, password };
}

async function fetchPendingSignupsSnapshot(apiBase, adminCookie) {
  const response = await fetch(`${apiBase}/v1/admin/pending-signups`, {
    method: "GET",
    headers: {
      cookie: `${adminCookie.name}=${adminCookie.value}`
    }
  });

  const body = await response.text();
  if (!response.ok) {
    debugLog("pending snapshot non-ok", { status: response.status, body });
  }
  return { status: response.status, body };
}

async function approveUser(options = {}) {
  const { apiBase, username, adminCookie } = options;

  if (!username) {
    throw new Error("approveUser requires a username");
  }

  if (!adminCookie || !adminCookie.name || !adminCookie.value) {
    throw new Error("approveUser requires a valid adminCookie");
  }

  for (let attempt = 1; attempt <= APPROVAL_RETRY_ATTEMPTS; attempt += 1) {
    const approval = await fetch(`${apiBase}/v1/admin/pending-signups/approve`, {
      method: "POST",
      headers: {
        "content-type": "application/json",
        cookie: `${adminCookie.name}=${adminCookie.value}`
      },
      body: JSON.stringify({ username })
    });

    if (approval.ok) {
      debugLog("approveUser success", { username, attempt });
      return;
    }

    const body = await approval.text();
    const isRetryableNotFound = approval.status === 404 && attempt < APPROVAL_RETRY_ATTEMPTS;

    debugLog("approveUser failure", {
      username,
      attempt,
      status: approval.status,
      body
    });

    if (isRetryableNotFound) {
      const snapshot = await fetchPendingSignupsSnapshot(apiBase, adminCookie);
      debugLog("pending snapshot after 404", {
        username,
        attempt,
        snapshotStatus: snapshot.status,
        snapshotBody: snapshot.body
      });
      await sleep(APPROVAL_RETRY_DELAY_MS);
      continue;
    }

    throw new Error(`Approve user failed with status ${approval.status}: ${body}`);
  }
}

async function ensureApprovedUser(options = {}) {
  const { apiBase } = buildCredentials(options);
  const { username, password, alreadyExists } = await ensureUser(options);
  debugLog("ensureApprovedUser after ensureUser", { username, alreadyExists });

  if (alreadyExists) {
    try {
      await signinUser({ apiBase, username, password });
      debugLog("ensureApprovedUser signin succeeded for existing user", { username });
      return { username, password };
    } catch {
      debugLog("ensureApprovedUser signin failed for existing user", { username });
    }
  }

  await ensureAdminUser(options);
  const adminSession = await signinAdmin(options);
  await approveUser({ apiBase, username, adminCookie: adminSession.cookie });
  return { username, password };
}

async function withExistingUser(options = {}) {
  const { apiBase = DEFAULT_API_BASE, kind, reuseKey, username, usernamePrefix, password = DEFAULT_PASSWORD } = options;

  if (!kind) {
    throw new Error("withExistingUser requires a kind");
  }

  const registryKey = identityRegistryKey(kind, reuseKey);
  const cached = identityRegistry.get(registryKey);
  if (cached) {
    debugLog("withExistingUser cache hit", { kind, reuseKey: reuseKey || kind, username: cached.username });
    return cached;
  }

  let identity;

  if (kind === "admin") {
    const adminIdentity = buildAdminCredentials({ apiBase, password });
    try {
      await signinAdmin({ apiBase, adminUsername: adminIdentity.username, adminPassword: adminIdentity.password });
      identity = { username: adminIdentity.username, password: adminIdentity.password };
    } catch {
      await ensureAdminUser({ apiBase, adminUsername: adminIdentity.username, adminPassword: adminIdentity.password });
      identity = { username: adminIdentity.username, password: adminIdentity.password };
    }
  } else {
    const nextReuseKey = reuseKey || kind;
    const nextUsername = username || buildRunScopedUsername(kind, nextReuseKey, usernamePrefix);

    switch (kind) {
      case "approved-member":
        identity = await ensureApprovedUser({ apiBase, username: nextUsername, password });
        break;
      case "pending-member":
        identity = await ensureUser({ apiBase, username: nextUsername, password });
        break;
      default:
        throw new Error(`Unsupported existing user kind: ${kind}`);
    }
  }

  identityRegistry.set(registryKey, identity);
  debugLog("withExistingUser cache store", { kind, reuseKey: reuseKey || kind, username: identity.username });
  return identity;
}

async function withNewUser(options = {}) {
  const { apiBase = DEFAULT_API_BASE, kind, usernamePrefix, password = DEFAULT_PASSWORD } = options;

  if (!kind) {
    throw new Error("withNewUser requires a kind");
  }

  const username = buildRunScopedUsername(kind, `${Date.now()}_${Math.floor(Math.random() * 100000)}`, usernamePrefix);

  switch (kind) {
    case "approved-member":
      return ensureApprovedUser({ apiBase, username, password });
    case "pending-member":
      return ensureUser({ apiBase, username, password });
    default:
      throw new Error(`Unsupported new user kind: ${kind}`);
  }
}

async function signupAndSignin(options = {}) {
  const { username, password } = await ensureApprovedUser(options);
  const signedIn = await signinUser({ ...options, username, password });

  return {
    username: signedIn.username,
    password: signedIn.password,
    cookie: signedIn.cookie
  };
}

module.exports = {
  approveUser,
  ensureAdminUser,
  ensureApprovedUser,
  ensureUser,
  signinAdmin,
  signinUser,
  signupUser,
  signupAndSignin,
  parseSessionCookie,
  withExistingUser,
  withNewUser
};
