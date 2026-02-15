const base = require("@playwright/test");
const { signupAndSignin } = require("../support/auth-session");

const API_BASE = process.env.E2E_API_URL || "http://localhost:1234/api";

const test = base.test.extend({
  session: async ({}, use) => {
    const session = await signupAndSignin({ apiBase: API_BASE, usernamePrefix: "e2e" });
    await use(session);
  },
  authenticatedPage: async ({ context, page, baseURL, session }, use) => {
    const hostname = new URL(baseURL).hostname;

    await context.addCookies([
      {
        name: session.cookie.name,
        value: session.cookie.value,
        domain: hostname,
        path: "/",
        httpOnly: true,
        secure: false,
        sameSite: "Lax"
      }
    ]);

    await page.goto("/");
    await use(page);
  }
});

module.exports = {
  test,
  expect: base.expect
};
