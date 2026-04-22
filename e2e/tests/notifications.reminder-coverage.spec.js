const { test, expect } = require("../fixtures/authenticated");
const {
  calendarTab,
  checklistsTab,
  lateItemsChip,
  lateItemsLoadMore,
  lateItemsRows,
  lateItemsSheet,
  notesTab
} = require("../support/ui");

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

async function mockLateItemsApi(page, options = {}) {
  const state = {
    items: options.items ? options.items.map(item => ({ ...item })) : [],
    updateCalls: []
  };

  await page.route("**/api/v1/calendar-items**", async route => {
    const request = route.request();
    const method = request.method();
    const url = request.url();

    if (method === "GET") {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify(state.items)
      });
      return;
    }

    if (method === "POST" && url.endsWith("/api/v1/calendar-items")) {
      const payload = request.postDataJSON();
      const itemId = payload?.id || "";
      state.updateCalls.push({ itemId, payload });

      if (typeof options.onUpdate === "function") {
        const outcome = options.onUpdate({ itemId, payload, state });
        if (outcome && outcome.status) {
          await route.fulfill({
            status: outcome.status,
            contentType: "application/json; charset=utf-8",
            body: outcome.body || "{}"
          });
          return;
        }
      }

      state.items = state.items.map(item => {
        if (item.id !== itemId) return item;
        return {
          ...item,
          ...payload
        };
      });
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify(state.items.find(item => item.id === itemId) || payload || {})
      });
      return;
    }

    await route.continue();
  });

  return state;
}

test("late reminders: chip/sheet stay coherent across tabs and pagination", async ({ authenticatedPage: page }) => {
  const mockedItems = buildLateTaskItems(55);
  await mockLateItemsApi(page, { items: mockedItems });

  await page.goto("/notes");
  await expect(lateItemsChip(page)).toBeVisible();
  await expect(lateItemsChip(page).locator(".app-late-items-chip__count")).toHaveText("55");

  await checklistsTab(page).click();
  await expect(page).toHaveURL(/\/checklists$/);
  await expect(lateItemsChip(page)).toBeVisible();

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar/);
  await expect(lateItemsChip(page)).toBeVisible();

  await notesTab(page).click();
  await expect(page).toHaveURL(/\/notes$/);
  await lateItemsChip(page).click();

  await expect(lateItemsSheet(page)).toBeVisible();
  await expect(lateItemsRows(page)).toHaveCount(50);
  await expect(lateItemsLoadMore(page)).toBeVisible();

  await lateItemsLoadMore(page).click();
  await expect(lateItemsRows(page)).toHaveCount(55);
  await expect(lateItemsLoadMore(page)).toHaveCount(0);
});

test("late reminders: selecting a row opens targeted calendar item actions", async ({ authenticatedPage: page }) => {
  const targetItem = {
    id: "late-target-1",
    type: "BLOC_PLANIFIE",
    titre: "Targeted late task",
    fenetre_debut: "2000-03-01T08:00",
    fenetre_fin: "2000-03-01T09:00",
    statut: "TODO",
    categorie: "Reminder"
  };
  await mockLateItemsApi(page, { items: [ targetItem ] });

  await page.goto("/notes");
  await lateItemsChip(page).click();
  await expect(lateItemsSheet(page)).toBeVisible();

  await lateItemsRows(page).first().locator(".app-late-items-row__navigate").click();

  await expect(page).toHaveURL(/\/calendar\?day=2000-03-01&item=late-target-1$/);
  await expect(page.locator(".calendar-item-actions-sheet")).toBeVisible();
});

test("late reminders: quick-complete success keeps sheet open and refreshes count", async ({ authenticatedPage: page }) => {
  const mockedItems = buildLateTaskItems(2);
  const state = await mockLateItemsApi(page, { items: mockedItems });

  await page.goto("/notes");
  await lateItemsChip(page).click();
  await expect(lateItemsRows(page)).toHaveCount(2);

  const firstRow = lateItemsRows(page).first();
  await firstRow.locator(".app-late-items-row__quick-complete").click();

  await expect(page.locator(".app-late-items-row__quick-complete-input")).toHaveValue("60");
  await page.locator(".app-late-items-row__quick-complete-confirm").click();

  await expect.poll(() => state.updateCalls.length).toBe(1);
  await expect(lateItemsSheet(page)).toBeVisible();
  await expect(lateItemsChip(page).locator(".app-late-items-chip__count")).toHaveText("1");
  await expect(lateItemsRows(page)).toHaveCount(1);
});

test("late reminders: quick-complete failure shows inline feedback and retry succeeds", async ({ authenticatedPage: page }) => {
  const mockedItems = buildLateTaskItems(1);
  let attempt = 0;
  await mockLateItemsApi(page, {
    items: mockedItems,
    onUpdate: ({ itemId, state }) => {
      attempt += 1;
      if (attempt === 1) {
        return { status: 500, body: "{\"error\":\"boom\"}" };
      }
      state.items = state.items.map(item => {
        if (item.id !== itemId) return item;
        return { ...item, statut: "DONE" };
      });
      return { status: 200, body: JSON.stringify(state.items.find(item => item.id === itemId) || {}) };
    }
  });

  await page.goto("/notes");
  await lateItemsChip(page).click();
  await expect(lateItemsRows(page)).toHaveCount(1);
  await lateItemsRows(page).first().locator(".app-late-items-row__quick-complete").click();

  await page.locator(".app-late-items-row__quick-complete-confirm").click();
  await expect(page.locator(".app-late-items-row__quick-complete-error")).toBeVisible();
  await expect(lateItemsRows(page)).toHaveCount(1);

  await page.locator(".app-late-items-row__quick-complete-confirm").click();
  await expect(lateItemsSheet(page)).toBeVisible();
  await expect(page.locator(".app-late-items-sheet__empty")).toBeVisible();
  await expect(lateItemsChip(page)).toHaveCount(0);
});

test("late reminders: quick-complete keeps item when response remains late", async ({ authenticatedPage: page }) => {
  const mockedItems = buildLateTaskItems(1);
  await mockLateItemsApi(page, {
    items: mockedItems,
    onUpdate: ({ itemId, state, payload }) => {
      state.items = state.items.map(item => {
        if (item.id !== itemId) return item;
        return {
          ...item,
          ...payload,
          titre: "Still late task",
          statut: "TODO"
        };
      });
      return {
        status: 200,
        body: JSON.stringify(state.items[0])
      };
    }
  });

  await page.goto("/notes");
  await lateItemsChip(page).click();
  await expect(lateItemsRows(page)).toHaveCount(1);

  await lateItemsRows(page).first().locator(".app-late-items-row__quick-complete").click();
  await page.locator(".app-late-items-row__quick-complete-confirm").click();

  await expect(lateItemsSheet(page)).toBeVisible();
  await expect(lateItemsRows(page)).toHaveCount(1);
  await expect(lateItemsRows(page).first().locator(".app-late-items-row__title")).toHaveText("Still late task");
  await expect(lateItemsChip(page).locator(".app-late-items-chip__count")).toHaveText("1");
});
