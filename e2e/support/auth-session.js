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

async function signupAndSignin(options = {}) {
  const apiBase = options.apiBase || DEFAULT_API_BASE;
  const password = options.password || DEFAULT_PASSWORD;
  const username =
    options.username ||
    `${options.usernamePrefix || "e2e"}_${Date.now()}_${Math.floor(Math.random() * 100000)}`;

  const payload = { username, password };

  const signup = await fetch(`${apiBase}/signup`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(payload)
  });
  if (!signup.ok) {
    throw new Error(`Signup failed with status ${signup.status}`);
  }

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

module.exports = {
  signupAndSignin,
  parseSessionCookie
};
