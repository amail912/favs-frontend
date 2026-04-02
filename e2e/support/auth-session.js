const DEFAULT_API_BASE = "http://localhost:1234/api";
const DEFAULT_PASSWORD = "StrongPass123!";
const DEFAULT_ADMIN_USERNAME = "admin";
const DEFAULT_ADMIN_PASSWORD = process.env.E2E_ADMIN_PASSWORD || "averystrongpass";

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

async function signupUser(options = {}) {
  const { apiBase, username, password } = buildCredentials(options);
  const payload = { username, password };

  const signup = await fetch(`${apiBase}/signup`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(payload)
  });

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
    return { username, password };
  }

  // Backend returns 400 when user already exists. For test fixtures this is acceptable.
  if (signup.status !== 400) {
    throw new Error(`Signup failed with status ${signup.status}`);
  }

  return { username, password };
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

async function approveUser(options = {}) {
  const { apiBase, username, adminCookie } = options;

  if (!username) {
    throw new Error("approveUser requires a username");
  }

  if (!adminCookie || !adminCookie.name || !adminCookie.value) {
    throw new Error("approveUser requires a valid adminCookie");
  }

  const approval = await fetch(`${apiBase}/v1/admin/pending-signups/approve`, {
    method: "POST",
    headers: {
      "content-type": "application/json",
      cookie: `${adminCookie.name}=${adminCookie.value}`
    },
    body: JSON.stringify({ username })
  });

  if (!approval.ok) {
    const body = await approval.text();
    throw new Error(`Approve user failed with status ${approval.status}: ${body}`);
  }
}

async function ensureApprovedUser(options = {}) {
  const { apiBase } = buildCredentials(options);
  const { username, password } = await ensureUser(options);
  await ensureAdminUser(options);
  const adminSession = await signinAdmin(options);
  await approveUser({ apiBase, username, adminCookie: adminSession.cookie });
  return { username, password };
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
  parseSessionCookie
};
