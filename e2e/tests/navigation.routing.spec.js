const base = require("@playwright/test");
const { test: authTest, expect: authExpect } = require("../fixtures/authenticated");

const { test, expect } = base;

test("root route redirects to /notes", async ({ page }) => {
  await page.goto("/");
  await expect(page).toHaveURL(/\/notes$/);
  await expect(page.getByRole("heading", { name: "FAVS" })).toBeVisible();
});

test("unknown route displays styled 404 page", async ({ page }) => {
  await page.goto("/this-route-does-not-exist");
  await expect(page.getByText("404")).toBeVisible();
  await expect(page.getByText("Page introuvable")).toBeVisible();
  await expect(page.getByRole("button", { name: "Aller aux notes" })).toBeVisible();
});

authTest("navigation tabs update browser URL", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await page.locator("a.nav-link", { hasText: "Checklists" }).click();
  await authExpect(page).toHaveURL(/\/checklists$/);

  await page.locator("a.nav-link", { hasText: "Notes" }).click();
  await authExpect(page).toHaveURL(/\/notes$/);
});
