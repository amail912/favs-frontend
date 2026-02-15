const DEFAULT_API_BASE = "http://localhost:1234/api";
const DEFAULT_PASSWORD = "StrongPass123!";

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

async function signupAndSignin(options = {}) {
  const { username, password } = await ensureUser(options);
  const signedIn = await signinUser({ ...options, username, password });

  return {
    username: signedIn.username,
    password: signedIn.password,
    cookie: signedIn.cookie
  };
}

module.exports = {
  ensureUser,
  signinUser,
  signupUser,
  signupAndSignin,
  parseSessionCookie
};
