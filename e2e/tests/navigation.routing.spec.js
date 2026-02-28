const base = require("@playwright/test");
const { test: authTest, expect: authExpect } = require("../fixtures/authenticated");
const {
  checklistsTab,
  notesTab,
  signupMenuButton,
  signupUsernameInput,
  signoutMenuButton
} = require("../support/ui");

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

test("unauthenticated user can navigate to signup from auth menu", async ({ page }) => {
  await page.goto("/notes");
  await expect(signupMenuButton(page)).toBeVisible();
  await signupMenuButton(page).click();
  await expect(page).toHaveURL(/\/signup$/);
  await expect(signupUsernameInput(page)).toBeVisible();
});

authTest("navigation tabs update browser URL", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await checklistsTab(page).click();
  await authExpect(page).toHaveURL(/\/checklists$/);

  await notesTab(page).click();
  await authExpect(page).toHaveURL(/\/notes$/);
});

authTest("authenticated user can sign out from auth menu", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await authExpect(signoutMenuButton(page)).toBeVisible();
  await signoutMenuButton(page).click();
  await authExpect(page).toHaveURL(/\/signin$/);
  await authExpect(signupMenuButton(page)).toBeVisible();
});
