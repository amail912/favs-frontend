const { test, expect } = require("../fixtures/authenticated");

test("checklists lifecycle: create and delete", async ({ authenticatedPage: page }) => {
  const checklistName = `Checklist ${Date.now()}`;

  await page.locator("a.nav-link", { hasText: "Checklists" }).click();
  await page.getByRole("button", { name: "+" }).click();

  const nameInput = page.locator("input.name-input").first();
  await expect(nameInput).toBeVisible();
  await nameInput.click();
  await page.keyboard.press("Control+A");
  await page.keyboard.type(checklistName, { delay: 10 });
  await page.locator("h1", { hasText: "FAVS" }).click();

  const row = page.locator("li", { has: page.locator(`h2:has-text("${checklistName}")`) }).first();
  await expect(row).toBeVisible();

  await row.locator("button.btn-outline-danger").first().click({ force: true });
  await expect(row).toHaveCount(0);
});
