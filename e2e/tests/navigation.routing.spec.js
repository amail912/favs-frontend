const base = require("@playwright/test");
const { test: authTest, expect: authExpect } = require("../fixtures/authenticated");
const {
  adminTab,
  checklistsTab,
  connectedIdentity,
  notesTab,
  signupMenuButton,
  signupUsernameInput,
  signoutMenuButton
} = require("../support/ui");
const { signinUser, withExistingUser } = require("../support/auth-session");

const { test, expect } = base;

async function signInAsAdmin(context, page, baseURL) {
  const adminIdentity = await withExistingUser({ kind: "admin" });
  const session = await signinUser(adminIdentity);
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
}

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
  await expect(connectedIdentity(page)).toHaveCount(0);
  await expect(signupMenuButton(page)).toBeVisible();
  await signupMenuButton(page).click();
  await expect(page).toHaveURL(/\/signup$/);
  await expect(connectedIdentity(page)).toHaveCount(0);
  await expect(signupUsernameInput(page)).toBeVisible();
});

authTest("navigation tabs update browser URL", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await authExpect(connectedIdentity(page)).toHaveText(/Connecté: /);
  await checklistsTab(page).click();
  await authExpect(page).toHaveURL(/\/checklists$/);
  await authExpect(connectedIdentity(page)).toHaveText(/Connecté: /);

  await notesTab(page).click();
  await authExpect(page).toHaveURL(/\/notes$/);
  await authExpect(connectedIdentity(page)).toHaveText(/Connecté: /);
});

authTest("authenticated user can sign out from auth menu", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await authExpect(connectedIdentity(page)).toHaveText(/Connecté: /);
  await authExpect(signoutMenuButton(page)).toBeVisible();
  await signoutMenuButton(page).click();
  await authExpect(page).toHaveURL(/\/signin$/);
  await authExpect(connectedIdentity(page)).toHaveCount(0);
  await authExpect(signupMenuButton(page)).toBeVisible();
});

authTest("authenticated shell shows the connected username", async ({ authenticatedPage: page, authIdentity }) => {
  await page.goto("/notes");
  await authExpect(connectedIdentity(page)).toHaveText(`Connecté: ${authIdentity.username}`);
});

authTest("approved non-admin user cannot discover or open the admin route", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await authExpect(adminTab(page)).toHaveCount(0);

  await page.goto("/admin");
  await authExpect(page.getByText("404")).toBeVisible();
  await authExpect(page.getByText("Page introuvable")).toBeVisible();
});

test("admin user sees admin navigation and can open admin page", async ({ context, page, baseURL }) => {
  await signInAsAdmin(context, page, baseURL);

  await expect(adminTab(page)).toBeVisible();
  await adminTab(page).click();
  await expect(page).toHaveURL(/\/admin$/);
  await expect(page.getByRole("heading", { name: "Administration utilisateurs" })).toBeVisible();
});

test("admin user keeps access to /admin after reload", async ({ context, page, baseURL }) => {
  await signInAsAdmin(context, page, baseURL);

  await page.goto("/admin");
  await expect(page.getByRole("heading", { name: "Administration utilisateurs" })).toBeVisible();

  await page.reload();
  await expect(page).toHaveURL(/\/admin$/);
  await expect(page.getByRole("heading", { name: "Administration utilisateurs" })).toBeVisible();
});
