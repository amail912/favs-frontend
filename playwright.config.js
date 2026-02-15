const { defineConfig } = require("@playwright/test");

module.exports = defineConfig({
  testDir: "./e2e/tests",
  fullyParallel: false,
  timeout: 30_000,
  expect: {
    timeout: 10_000
  },
  retries: 0,
  workers: 1,
  use: {
    baseURL: process.env.E2E_BASE_URL || "http://localhost:1234",
    trace: "on-first-retry",
    screenshot: "only-on-failure",
    video: "retain-on-failure"
  },
  webServer: {
    command: "npm run start-dev",
    url: "http://localhost:1234",
    reuseExistingServer: true,
    timeout: 30_000
  }
});
