const base = require("@playwright/test");
const { signinUser, withExistingUser } = require("../support/auth-session");

const API_BASE = process.env.E2E_API_URL || "http://localhost:1234/api";
const E2E_PASSWORD = process.env.E2E_PASSWORD || "StrongPass123!";

const test = base.test.extend({
  authIdentity: [async ({}, use, testInfo) => {
    const identity = process.env.E2E_USERNAME
      ? await withExistingUser({
          apiBase: API_BASE,
          kind: "approved-member",
          reuseKey: process.env.E2E_USERNAME,
          username: process.env.E2E_USERNAME,
          password: E2E_PASSWORD
        })
      : await withExistingUser({
          apiBase: API_BASE,
          kind: "approved-member",
          reuseKey: `worker-${testInfo.workerIndex}`,
          password: E2E_PASSWORD
        });
    await use(identity);
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
