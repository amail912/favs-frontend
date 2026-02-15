const { test, expect } = require("../fixtures/authenticated");
const { appTitle, checklistsTab, createButton } = require("../support/ui");

test("checklists lifecycle: create and delete", async ({ authenticatedPage: page }) => {
  const checklistName = `Checklist ${Date.now()}`;

  await checklistsTab(page).click();
  await createButton(page).click();

  const nameInput = page.locator("input.name-input").first();
  await expect(nameInput).toBeVisible();
  await nameInput.click();
  await page.keyboard.press("Control+A");
  await page.keyboard.type(checklistName, { delay: 10 });
  await appTitle(page).click();

  const row = page.locator("li", { has: page.locator(`h2:has-text("${checklistName}")`) }).first();
  await expect(row).toBeVisible();

  await row.locator("button.btn-outline-danger").first().click({ force: true });
  await expect(row).toHaveCount(0);
});
