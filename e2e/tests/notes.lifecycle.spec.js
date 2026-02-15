const { test, expect } = require("../fixtures/authenticated");

test("notes lifecycle: create, edit, delete", async ({ authenticatedPage: page }) => {
  const title = `Note ${Date.now()}`;
  const content = `Content ${Date.now()}`;

  await page.locator("a.nav-link", { hasText: "Notes" }).click();
  await page.getByRole("button", { name: "+" }).click();

  const titleInput = page.locator("input.title-input").first();
  await expect(titleInput).toBeVisible();
  await titleInput.click();
  await page.keyboard.press("Control+A");
  await page.keyboard.type(title, { delay: 10 });
  await page.locator("h1", { hasText: "FAVS" }).click();

  const row = page.locator("li", { has: page.locator(`h2:has-text("${title}")`) }).first();
  await expect(row).toBeVisible();

  await row.locator("section div").first().click();
  const contentInput = row.locator("textarea.content-input").first();
  await expect(contentInput).toBeVisible();
  await contentInput.click();
  await page.keyboard.press("Control+A");
  await page.keyboard.type(content, { delay: 10 });
  await page.locator("h1", { hasText: "FAVS" }).click();

  await expect(row.locator(`text=${content}`).first()).toBeVisible();

  await row.locator("button.btn-outline-danger").first().click({ force: true });
  await expect(row).toHaveCount(0);
});
