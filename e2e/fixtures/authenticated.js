const base = require("@playwright/test");
const { ensureUser, signinUser } = require("../support/auth-session");

const API_BASE = process.env.E2E_API_URL || "http://localhost:1234/api";
const E2E_PASSWORD = process.env.E2E_PASSWORD || "StrongPass123!";

const test = base.test.extend({
  authIdentity: [async ({}, use, testInfo) => {
    const username =
      process.env.E2E_USERNAME ||
      `e2e_worker_${testInfo.workerIndex}_${Date.now()}_${Math.floor(Math.random() * 10000)}`;
    const password = E2E_PASSWORD;
    await ensureUser({ apiBase: API_BASE, username, password });
    await use({ username, password });
  }, { scope: "worker" }],
  session: async ({ authIdentity }, use) => {
    const session = await signinUser({ apiBase: API_BASE, ...authIdentity });
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
