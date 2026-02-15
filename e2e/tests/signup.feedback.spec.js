const { test, expect } = require("@playwright/test");

const STRONG_PASSWORD = "StrongPass123!";

async function gotoSignup(page) {
  await page.goto("/");
  await page.evaluate(() => {
    window.location.hash = "#/signup";
  });
  await expect(page.locator("#signup-username")).toBeVisible();
}

test("signup form shows client-side validation feedback", async ({ page }) => {
  await gotoSignup(page);

  await page.getByRole("button", { name: "Submit" }).click();
  await expect(page.getByText("Username cannot be empty.")).toBeVisible();
  await expect(page.getByText("Password cannot be empty.")).toBeVisible();

  await page.locator("#signup-username").fill("ab");
  await page.locator("#signup-password").fill("short");
  await page.getByRole("button", { name: "Submit" }).click();

  await expect(page.getByText("Username must be at least 3 characters.")).toBeVisible();
  await expect(page.getByText("Password must be at least 12 characters.")).toBeVisible();
});

test("signup form shows backend failure feedback for duplicate username", async ({ page }) => {
  const username = `dup_${Date.now()}_${Math.floor(Math.random() * 100000)}`;

  await page.route("**/api/signup", async route => {
    await route.fulfill({
      status: 400,
      contentType: "text/plain; charset=utf-8",
      body: "Unable to create user"
    });
  });

  await gotoSignup(page);
  await page.locator("#signup-username").fill(username);
  await page.locator("#signup-password").fill(STRONG_PASSWORD);
  await page.getByRole("button", { name: "Submit" }).click();

  await expect(page.getByText("Unable to create user")).toBeVisible();
});
