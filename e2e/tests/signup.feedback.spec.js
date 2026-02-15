const { test, expect } = require("@playwright/test");
const { createAccountButton, signupPasswordInput, signupUsernameInput } = require("../support/ui");

const STRONG_PASSWORD = "StrongPass123!";

async function gotoSignup(page) {
  await page.goto("/signup");
  await expect(signupUsernameInput(page)).toBeVisible();
}

test("signup form shows client-side validation feedback", async ({ page }) => {
  await gotoSignup(page);

  await createAccountButton(page).click();
  await expect(page.getByText("Username cannot be empty.")).toBeVisible();
  await expect(page.getByText("Password cannot be empty.")).toBeVisible();

  await signupUsernameInput(page).fill("ab");
  await signupPasswordInput(page).fill("short");
  await createAccountButton(page).click();

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
  await signupUsernameInput(page).fill(username);
  await signupPasswordInput(page).fill(STRONG_PASSWORD);
  await createAccountButton(page).click();

  await expect(page.getByText("Unable to create user")).toBeVisible();
});
