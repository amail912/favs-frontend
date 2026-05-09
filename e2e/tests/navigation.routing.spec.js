const base = require("@playwright/test");
const { test: authTest, expect: authExpect } = require("../fixtures/authenticated");
const {
  adminTab,
  calendarTab,
  checklistsTab,
  connectedIdentity,
  financeCreateButton,
  financeReportsTab,
  financeShell,
  financeTab,
  financeTransactionsTab,
  lateItemsChip,
  lateItemsLoadMore,
  lateItemsRows,
  lateItemsSheet,
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
  await attachSessionCookie(context, baseURL, session);
  await page.goto("/");
}

async function attachSessionCookie(context, baseURL, session) {
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
}

async function authenticateAdmin(context, baseURL) {
  const adminIdentity = await withExistingUser({ kind: "admin" });
  const session = await signinUser(adminIdentity);
  await attachSessionCookie(context, baseURL, session);
}

function buildLateTaskItems(count) {
  return Array.from({ length: count }, (_, index) => {
    const minute = `${index % 60}`.padStart(2, "0");
    return {
      id: `late-${index + 1}`,
      type: "BLOC_PLANIFIE",
      titre: `Late task ${index + 1}`,
      fenetre_debut: `2000-03-01T08:${minute}`,
      fenetre_fin: `2000-03-01T09:${minute}`,
      statut: "TODO",
      categorie: "Reminder"
    };
  });
}

async function mockLateItemsRoute(page, items) {
  await page.route("**/api/v1/calendar-items**", async route => {
    if (route.request().method() === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify(items)
      });
      return;
    }
    await route.continue();
  });
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
  await authExpect(financeShell(page)).toHaveCount(0);
  await checklistsTab(page).click();
  await authExpect(page).toHaveURL(/\/checklists$/);
  await authExpect(connectedIdentity(page)).toHaveText(/Connecté: /);

  await notesTab(page).click();
  await authExpect(page).toHaveURL(/\/notes$/);
  await authExpect(connectedIdentity(page)).toHaveText(/Connecté: /);
});

authTest("authenticated user sees finance navigation and lands on transactions", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await authExpect(financeTab(page)).toBeVisible();
  await financeTab(page).click();
  await authExpect(page).toHaveURL(/\/finance\/transactions$/);
  await authExpect(financeShell(page)).toBeVisible();
  await authExpect(financeTransactionsTab(page)).toHaveClass(/active/);
  await authExpect(financeReportsTab(page)).toBeVisible();
  await authExpect(financeCreateButton(page)).toBeVisible();
  await authExpect(page.locator(".finance-route-placeholder")).toContainText("Finance Transactions");
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

authTest("late-items reminder chip stays hidden when no late item exists", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await authExpect(lateItemsChip(page)).toHaveCount(0);
});

test("calendar route with a missing targeted item remains usable", async ({ context, page, baseURL }) => {
  await authenticateAdmin(context, baseURL);
  await mockLateItemsRoute(page, buildLateTaskItems(2));

  await page.goto("/calendar?day=2000-03-01&item=item-does-not-exist");
  await expect(page.locator(".calendar-view-date-trigger")).toBeVisible();
  await expect(page.locator(".calendar-item-actions-sheet")).toHaveCount(0);

  await page.getByRole("button", { name: "Semaine" }).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await page.getByRole("button", { name: "Jour" }).click();
  await expect(page).toHaveURL(/\/calendar\?day=/);
});

authTest("approved non-admin user cannot discover or open the admin route", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await authExpect(adminTab(page)).toHaveCount(0);

  await page.goto("/admin");
  await authExpect(page.getByText("404")).toBeVisible();
  await authExpect(page.getByText("Page introuvable")).toBeVisible();
});

test("unauthenticated user cannot open finance routes", async ({ page }) => {
  await page.goto("/finance/transactions");
  await expect(page.getByText("404")).toBeVisible();
  await expect(page.getByText("Page introuvable")).toBeVisible();
  await expect(financeShell(page)).toHaveCount(0);
});

authTest("authenticated direct /finance entry canonicalizes to /finance/transactions", async ({ authenticatedPage: page }) => {
  await page.goto("/finance");
  await authExpect(page).toHaveURL(/\/finance\/transactions$/);
  await authExpect(financeShell(page)).toBeVisible();
  await authExpect(financeTransactionsTab(page)).toHaveClass(/active/);
  await authExpect(financeCreateButton(page)).toBeVisible();
  await authExpect(page.locator(".finance-route-placeholder")).toContainText("Transactions");
});

authTest("authenticated finance route stays accessible after reload", async ({ authenticatedPage: page }) => {
  await page.goto("/finance/reports");
  await authExpect(financeShell(page)).toBeVisible();
  await authExpect(financeReportsTab(page)).toHaveClass(/active/);
  await authExpect(financeCreateButton(page)).toHaveCount(0);
  await authExpect(page.locator(".finance-route-placeholder")).toContainText("Reports");
  await page.reload();
  await authExpect(page).toHaveURL(/\/finance\/reports$/);
  await authExpect(financeReportsTab(page)).toHaveClass(/active/);
  await authExpect(financeCreateButton(page)).toHaveCount(0);
  await authExpect(page.locator(".finance-route-placeholder")).toContainText("Reports");
});

authTest("finance local navigation switches between transactions and reports", async ({ authenticatedPage: page }) => {
  await page.goto("/finance/transactions");
  await authExpect(financeTransactionsTab(page)).toHaveClass(/active/);
  await financeReportsTab(page).click();
  await authExpect(page).toHaveURL(/\/finance\/reports$/);
  await authExpect(financeReportsTab(page)).toHaveClass(/active/);
  await authExpect(financeCreateButton(page)).toHaveCount(0);
  await financeTransactionsTab(page).click();
  await authExpect(page).toHaveURL(/\/finance\/transactions$/);
  await authExpect(financeTransactionsTab(page)).toHaveClass(/active/);
  await authExpect(financeCreateButton(page)).toBeVisible();
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
