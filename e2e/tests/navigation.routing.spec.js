const base = require("@playwright/test");
const { test: authTest, expect: authExpect } = require("../fixtures/authenticated");
const {
  adminTab,
  calendarTab,
  checklistsTab,
  connectedIdentity,
  financeCreateChooser,
  financeCreateExpenseAction,
  financeCreateIncomeAction,
  financeCreateOverlay,
  financeCreateButton,
  financeDetailOverlay,
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

function buildFinanceTransactions(count) {
  return Array.from({ length: count }, (_, index) => {
    const minute = `${index % 60}`.padStart(2, "0");
    return {
      id: `tx-${index + 1}`,
      direction: index % 2 === 0 ? "sent" : "received",
      accountId: "acc-1",
      amount: 10 + index,
      occurredAt: `2026-05-10T09:${minute}:00Z`,
      recordedAt: `2026-05-10T09:${minute}:05Z`,
      transfer: index === 1 ? { linkedTransactionId: "tx-99", linkType: "transfer" } : null,
      category: index === 2 ? null : { id: "cat-1" },
      splits: index === 0 ? [{ amount: 3, category: "cat-1" }] : [],
      notes: index === 2 ? [{ id: "note-1", text: "memo" }] : [],
      adjustment: index === 3 ? { kind: "balance-snapshot" } : null
    };
  });
}

async function mockFinanceLedgerRoutes(page, transactions) {
  await page.route("**/api/v1/finance/accounts**", async route => {
    if (route.request().method() === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify([{ id: "acc-1", name: "Primary account", status: "active" }])
      });
      return;
    }
    await route.continue();
  });

  await page.route("**/api/v1/finance/transactions**", async route => {
    if (route.request().method() === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify(transactions)
      });
      return;
    }
    await route.continue();
  });

  await page.route("**/api/v1/finance/categories**", async route => {
    if (route.request().method() === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify([
          { id: "personal.clothing", name: "Clothing", parentId: null, owner: "system", selectable: true },
          { id: "internal.root", name: "Root", parentId: null, owner: "system", selectable: false }
        ])
      });
      return;
    }
    await route.continue();
  });

  await page.route("**/api/v1/finance/transactions/*/categorize", async route => {
    if (route.request().method() === "POST") {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify({})
      });
      return;
    }
    await route.continue();
  });

  await page.route("**/api/v1/finance/transactions/*/notes", async route => {
    if (route.request().method() === "POST") {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify({ id: "note-new", text: "added note" })
      });
      return;
    }
    await route.continue();
  });

  await page.route("**/api/v1/finance/transactions/*/notes/*", async route => {
    if (route.request().method() === "PUT") {
      const body = JSON.parse(route.request().postData() || "{}");
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify({ id: "note-new", text: body.text || "" })
      });
      return;
    }
    if (route.request().method() === "DELETE") {
      await route.fulfill({
        status: 200,
        contentType: "text/plain; charset=utf-8",
        body: ""
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
  await authExpect(page.locator(".finance-ledger-workspace")).toBeVisible();
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
  await authExpect(page.locator(".finance-ledger-workspace")).toBeVisible();
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

authTest("finance transactions filters update URL and survive reload/back-forward", async ({ authenticatedPage: page }) => {
  await page.goto("/finance/transactions");

  await page.locator(".finance-ledger-filter-from").fill("2026-05-01T00:00:00Z");
  await page.locator(".finance-ledger-filter-to").fill("2026-05-31T23:59:59Z");
  await page.locator(".finance-ledger-apply").click();
  await authExpect(page).toHaveURL(/\/finance\/transactions\?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z$/);

  await page.reload();
  await authExpect(page).toHaveURL(/\/finance\/transactions\?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z$/);

  await page.locator(".finance-ledger-filter-from").fill("");
  await page.locator(".finance-ledger-apply").click();
  await authExpect(page).toHaveURL(/\/finance\/transactions\?to=2026-05-31T23:59:59Z$/);

  await page.goBack();
  await authExpect(page).toHaveURL(/\/finance\/transactions\?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z$/);
});

authTest("finance create overlay preserves route and browser back closes it", async ({ authenticatedPage: page }) => {
  await page.goto("/notes");
  await financeTab(page).click();
  await authExpect(page).toHaveURL(/\/finance\/transactions$/);

  await financeCreateButton(page).click();
  await authExpect(financeCreateChooser(page)).toBeVisible();
  await financeCreateExpenseAction(page).click();
  await authExpect(financeCreateOverlay(page)).toBeVisible();
  await authExpect(page).toHaveURL(/\/finance\/transactions$/);

  await page.goBack();
  await authExpect(financeCreateOverlay(page)).toHaveCount(0);
  await authExpect(page).toHaveURL(/\/finance\/transactions$/);

  await financeCreateButton(page).click();
  await authExpect(financeCreateChooser(page)).toBeVisible();
  await financeCreateExpenseAction(page).click();
  await authExpect(financeCreateOverlay(page)).toBeVisible();
  await page.getByRole("button", { name: "Fermer" }).click();
  await authExpect(financeCreateOverlay(page)).toHaveCount(0);

  await page.goBack();
  await authExpect(page).toHaveURL(/\/notes$/);
});

authTest("finance create overlay success closes without changing route", async ({ authenticatedPage: page }) => {
  await page.goto("/finance/transactions");
  await financeCreateButton(page).click();
  await authExpect(financeCreateChooser(page)).toBeVisible();
  await financeCreateIncomeAction(page).click();
  await authExpect(financeCreateOverlay(page)).toBeVisible();

  await page.locator(".finance-create-overlay__amount-input").fill("42");
  await page.getByRole("button", { name: "Save transaction" }).click();
  await authExpect(financeCreateOverlay(page)).toBeVisible();
  await authExpect(page.locator(".finance-create-overlay__amount-input")).toHaveValue("");
  await authExpect(page).toHaveURL(/\/finance\/transactions$/);
  await authExpect(financeTransactionsTab(page)).toHaveClass(/active/);
});

authTest("finance create chooser forwards direction and ledger context", async ({ authenticatedPage: page }) => {
  await page.goto("/finance/transactions?accountId=acc-1&from=2026-05-20T09:30:00Z");
  await financeCreateButton(page).click();
  await authExpect(financeCreateChooser(page)).toBeVisible();
  await financeCreateExpenseAction(page).click();
  await authExpect(financeCreateOverlay(page)).toContainText("Direction: sent");
  await authExpect(page.locator(".finance-create-overlay__account-input")).toHaveValue("acc-1");
  await authExpect(page.locator(".finance-create-overlay__occurred-at-input")).toHaveValue("2026-05-20T12:00");
});

authTest("finance create mobile success closes overlay and shows transient toast", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.goto("/finance/transactions");
  await financeCreateButton(page).click();
  await authExpect(financeCreateChooser(page)).toBeVisible();
  await financeCreateExpenseAction(page).click();
  await authExpect(financeCreateOverlay(page)).toBeVisible();

  await page.locator(".finance-create-overlay__amount-input").fill("21");
  await page.getByRole("button", { name: "Save transaction" }).click();
  await authExpect(financeCreateOverlay(page)).toHaveCount(0);
  await authExpect(page.locator(".app-toast")).toContainText("Transaction saved.");
});

authTest("finance ledger row opens detail overlay and preserves route and scroll on close", async ({ authenticatedPage: page }) => {
  const transactions = buildFinanceTransactions(80);
  await mockFinanceLedgerRoutes(page, transactions);

  await page.goto("/finance/transactions?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z");
  await authExpect(page.locator(".finance-ledger-row")).toHaveCount(80);
  await authExpect(page.locator(".finance-ledger-row__facts")).toContainText("split");
  await authExpect(page.locator(".finance-ledger-row__facts")).toContainText("transfer");
  await authExpect(page.locator(".finance-ledger-row__facts")).toContainText("note");
  await authExpect(page.locator(".finance-ledger-row__facts")).toContainText("adjustment");

  await page.evaluate(() => window.scrollTo(0, 1200));
  const beforeOpenScroll = await page.evaluate(() => Math.trunc(window.scrollY));

  await page.locator(".finance-ledger-row").first().click();
  await authExpect(financeDetailOverlay(page)).toBeVisible();
  await authExpect(financeDetailOverlay(page)).toContainText("Core facts");
  await authExpect(financeDetailOverlay(page)).toContainText("Id: tx-1");
  await authExpect(financeDetailOverlay(page)).toContainText("Direction: sent");
  await authExpect(financeDetailOverlay(page)).toContainText("Account: Primary account");
  await authExpect(financeDetailOverlay(page)).toContainText("Categorization");
  await authExpect(financeDetailOverlay(page)).toContainText("cat-1: 3");
  await authExpect(page).toHaveURL(/\/finance\/transactions\?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z$/);

  await page.getByRole("button", { name: "Fermer" }).click();
  await authExpect(financeDetailOverlay(page)).toHaveCount(0);
  await authExpect(page).toHaveURL(/\/finance\/transactions\?from=2026-05-01T00:00:00Z&to=2026-05-31T23:59:59Z$/);
  const afterCloseScroll = await page.evaluate(() => Math.trunc(window.scrollY));
  expect(Math.abs(afterCloseScroll - beforeOpenScroll)).toBeLessThanOrEqual(2);
});

authTest("finance detail supports category and note management", async ({ authenticatedPage: page }) => {
  const transactions = buildFinanceTransactions(8);
  await mockFinanceLedgerRoutes(page, transactions);

  await page.goto("/finance/transactions");
  await authExpect(page.locator(".finance-ledger-row")).toHaveCount(8);

  await page.locator(".finance-ledger-row").first().click();
  await authExpect(financeDetailOverlay(page)).toContainText("Split transactions");
  await authExpect(page.locator(".finance-detail-overlay__category-submit")).toHaveCount(0);
  await page.getByRole("button", { name: "Fermer" }).click();

  await page.locator(".finance-ledger-row").nth(1).click();
  await authExpect(financeDetailOverlay(page)).toBeVisible();

  await page.locator(".finance-detail-overlay__category-select").selectOption("personal.clothing");
  await page.locator(".finance-detail-overlay__category-submit").click();

  await page.locator(".finance-detail-overlay__new-note-input").fill("added note");
  await page.locator(".finance-detail-overlay__new-note-submit").click();
  await authExpect(financeDetailOverlay(page)).toContainText("#note-new: added note");

  await page.locator(".finance-detail-overlay__note-edit").first().click();
  await page.locator(".finance-detail-overlay__edit-note-input").fill("edited note");
  await page.locator(".finance-detail-overlay__edit-note-save").click();
  await authExpect(financeDetailOverlay(page)).toContainText("edited note");

  await page.locator(".finance-detail-overlay__note-delete").first().click();
  await authExpect(financeDetailOverlay(page)).not.toContainText("edited note");
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
