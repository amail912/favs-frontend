const { test, expect } = require("../fixtures/authenticated");
const { ensureUser } = require("../support/auth-session");
const { calendarTab, appTitle } = require("../support/ui");

const API_BASE = process.env.E2E_API_URL || "http://localhost:1234/api";
const E2E_PASSWORD = process.env.E2E_PASSWORD || "StrongPass123!";
const AUTH_USERNAME_STORAGE_KEY = "favs.auth.username";

function toLocalDatetimeInput(date) {
  const pad = value => `${value}`.padStart(2, "0");
  return [
    date.getFullYear(),
    pad(date.getMonth() + 1),
    pad(date.getDate())
  ].join("-") + "T" + [pad(date.getHours()), pad(date.getMinutes())].join(":");
}

function toLocalDateInput(date) {
  const pad = value => `${value}`.padStart(2, "0");
  return [
    date.getFullYear(),
    pad(date.getMonth() + 1),
    pad(date.getDate())
  ].join("-");
}

function toFrenchDayLabel(date) {
  const currentYear = new Date().getFullYear();
  return new Intl.DateTimeFormat("fr-FR", {
    weekday: "short",
    day: "numeric",
    month: "short",
    ...(date.getFullYear() !== currentYear ? { year: "numeric" } : {})
  }).format(date);
}

function isolatedFutureDate(baseOffsetDays) {
  const dayOffset = baseOffsetDays + (Date.now() % 300);
  return new Date(Date.now() + dayOffset * 24 * 60 * 60 * 1000);
}

async function openCreateModal(page) {
  await page.getByRole("button", { name: "Nouvel item" }).click();
  const modal = page.locator(".app-modal__dialog", { hasText: "Créer un item" });
  await expect(modal).toBeVisible();
  return modal;
}

async function setCalendarViewDate(page, value) {
  await page.locator(".calendar-view-date").evaluate((input, nextValue) => {
    input.value = nextValue;
    input.dispatchEvent(new Event("input", { bubbles: true }));
    input.dispatchEvent(new Event("change", { bubbles: true }));
  }, value);
}

async function setTextInputValue(locator, value) {
  await locator.evaluate((input, nextValue) => {
    input.value = nextValue;
    input.dispatchEvent(new Event("input", { bubbles: true }));
    input.dispatchEvent(new Event("change", { bubbles: true }));
  }, value);
}

function defaultSharedPresenceTrips(focusDate) {
  return [
    {
      windowStart: `${focusDate}T08:00`,
      windowEnd: `${focusDate}T09:00`,
      departurePlaceId: "Paris",
      arrivalPlaceId: "St Clair"
    }
  ];
}

async function mockSharedPresenceRoute(page, focusDate, sharedUsers) {
  const groups = Array.isArray(sharedUsers)
    ? sharedUsers.map(group => ({
        username: group.username,
        trips: group.trips || defaultSharedPresenceTrips(focusDate)
      }))
    : [
        {
          username: sharedUsers,
          trips: defaultSharedPresenceTrips(focusDate)
        }
      ];

  await page.route("**/api/v1/trip-sharing/period-trips**", async route => {
    const url = new URL(route.request().url());
    if (url.searchParams.get("start") === `${focusDate}T00:00`) {
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify(groups)
      });
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: "[]"
    });
  });
}

async function seedCuePreferenceOwner(page, username) {
  const cueStorageKey = `favs.calendar.presence-cues.${username}`;
  await page.evaluate(({ authKey, cueKey, nextUsername }) => {
    window.localStorage.setItem(authKey, nextUsername);
    window.localStorage.removeItem(cueKey);
  }, { authKey: AUTH_USERNAME_STORAGE_KEY, cueKey: cueStorageKey, nextUsername: username });
}

async function dragCalendarItemToMinute(page, title, targetMinuteOfDay) {
  const source = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();
  const grid = page.locator(".calendar-calendar-items");

  await expect(source).toBeVisible();
  await expect(grid).toBeVisible();

  const box = await grid.boundingBox();
  if (!box) {
    throw new Error("Missing day grid bounding box");
  }

  const targetY = box.y + (box.height * targetMinuteOfDay) / 1440 + 1;
  const dataTransfer = await page.evaluateHandle(() => new DataTransfer());

  await source.dispatchEvent("dragstart", { dataTransfer });
  await grid.dispatchEvent("dragenter", { dataTransfer, clientY: targetY });
  await grid.dispatchEvent("dragover", { dataTransfer, clientY: targetY });
  await grid.dispatchEvent("drop", { dataTransfer, clientY: targetY });
  await source.dispatchEvent("dragend", { dataTransfer });
}

async function dispatchTouch(locator, type, x, y) {
  await locator.evaluate((element, payload) => {
    const touch = new Touch({
      identifier: 1,
      target: element,
      clientX: payload.x,
      clientY: payload.y,
      pageX: payload.x,
      pageY: payload.y,
      screenX: payload.x,
      screenY: payload.y
    });
    const activeTouches = payload.type === "touchend" || payload.type === "touchcancel" ? [] : [touch];
    element.dispatchEvent(new TouchEvent(payload.type, {
      bubbles: true,
      cancelable: true,
      composed: true,
      touches: activeTouches,
      targetTouches: activeTouches,
      changedTouches: [touch]
    }));
  }, { type, x, y });
}

async function longPressDragCalendarItemToMinute(page, title, targetMinuteOfDay) {
  const sourceCard = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();
  const touchSurface = sourceCard.locator(".calendar-calendar-content").first();
  const grid = page.locator(".calendar-calendar-items");

  await expect(sourceCard).toBeVisible();
  await expect(touchSurface).toBeVisible();
  await expect(grid).toBeVisible();

  const [sourceBox, gridBox] = await Promise.all([
    touchSurface.boundingBox(),
    grid.boundingBox()
  ]);

  if (!sourceBox || !gridBox) {
    throw new Error("Missing mobile drag bounding boxes");
  }

  const sourceX = sourceBox.x + sourceBox.width / 2;
  const sourceY = sourceBox.y + Math.min(2, sourceBox.height / 4);
  const targetY = gridBox.y + (gridBox.height * targetMinuteOfDay) / 1440 + 1;

  await dispatchTouch(touchSurface, "touchstart", sourceX, sourceY);
  await page.waitForTimeout(500);
  await expect(page.locator(".calendar-calendar-drag-preview")).toBeVisible();

  await dispatchTouch(grid, "touchmove", sourceX, targetY);

  await dispatchTouch(grid, "touchend", sourceX, targetY);
  await expect(page.locator(".calendar-calendar-drag-preview")).toHaveCount(0);
}

test("calendar integration: create task in day view", async ({ authenticatedPage: page }) => {
  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Task ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const modal = await openCreateModal(page);
  await modal.getByPlaceholder("Titre").fill(title);
  await modal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await modal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));
  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );
  const listResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "GET"
  );

  await modal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const listResponse = await listResponsePromise;
  expect(listResponse.ok()).toBeTruthy();

  const listBody = await listResponse.json();
  const titles = Array.isArray(listBody) ? listBody.map(item => item.titre || "") : [];
  expect(titles).toContain(title);

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await appTitle(page).click();
});

test("calendar share panel: add and remove a shared user", async ({ authenticatedPage: page }, testInfo) => {
  const sharedUsername = `e2e_shared_${testInfo.workerIndex}_${Date.now()}`;

  await ensureUser({ apiBase: API_BASE, username: sharedUsername, password: E2E_PASSWORD });

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const sharePanel = page.locator(".calendar-share-panel").filter({ hasText: "Qui peut voir mes trajets" });
  await expect(sharePanel).toBeVisible();

  const addButton = sharePanel.getByRole("button", { name: "Ajouter" });
  await expect(addButton).toBeDisabled();

  await setTextInputValue(sharePanel.getByPlaceholder("Nom d'utilisateur"), sharedUsername);
  await expect(addButton).toBeEnabled();

  const addResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/trip-sharing/shares") &&
      response.request().method() === "POST"
  );
  const addListResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/trip-sharing/shares") &&
      response.request().method() === "GET"
  );

  await addButton.click();
  expect((await addResponsePromise).ok()).toBeTruthy();
  expect((await addListResponsePromise).ok()).toBeTruthy();

  const shareRow = sharePanel.locator(".calendar-share-panel__item", { hasText: sharedUsername }).first();
  await expect(shareRow).toBeVisible();

  const deleteResponsePromise = page.waitForResponse(
    response =>
      response.url().includes(`/api/v1/trip-sharing/shares/${sharedUsername}`) &&
      response.request().method() === "DELETE"
  );
  const deleteListResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/trip-sharing/shares") &&
      response.request().method() === "GET"
  );

  await shareRow.getByRole("button", { name: "Retirer" }).click();
  expect((await deleteResponsePromise).ok()).toBeTruthy();
  expect((await deleteListResponsePromise).ok()).toBeTruthy();
  await expect(shareRow).toHaveCount(0);
});

test("calendar subscription panel: add and remove a subscribed user", async ({ authenticatedPage: page }, testInfo) => {
  const subscribedUsername = `e2e_subscription_${testInfo.workerIndex}_${Date.now()}`;

  await ensureUser({ apiBase: API_BASE, username: subscribedUsername, password: E2E_PASSWORD });

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const subscriptionPanel = page.locator(".calendar-share-panel").filter({ hasText: "Utilisateurs suivis" });
  await expect(subscriptionPanel).toBeVisible();
  await expect(subscriptionPanel).toContainText("Vous ne verrez les trajets d'une personne que si elle vous les partage aussi.");

  const addButton = subscriptionPanel.getByRole("button", { name: "Ajouter" });
  await expect(addButton).toBeDisabled();

  await setTextInputValue(subscriptionPanel.getByPlaceholder("Nom d'utilisateur"), subscribedUsername);
  await expect(addButton).toBeEnabled();

  const addResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/trip-sharing/subscriptions") &&
      response.request().method() === "POST"
  );
  const addListResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/trip-sharing/subscriptions") &&
      response.request().method() === "GET"
  );

  await addButton.click();
  expect((await addResponsePromise).ok()).toBeTruthy();
  expect((await addListResponsePromise).ok()).toBeTruthy();

  const subscriptionRow = subscriptionPanel.locator(".calendar-share-panel__item", { hasText: subscribedUsername }).first();
  await expect(subscriptionRow).toBeVisible();

  const deleteResponsePromise = page.waitForResponse(
    response =>
      response.url().includes(`/api/v1/trip-sharing/subscriptions/${subscribedUsername}`) &&
      response.request().method() === "DELETE"
  );
  const deleteListResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/trip-sharing/subscriptions") &&
      response.request().method() === "GET"
  );

  await subscriptionRow.getByRole("button", { name: "Retirer" }).click();
  expect((await deleteResponsePromise).ok()).toBeTruthy();
  expect((await deleteListResponsePromise).ok()).toBeTruthy();
  await expect(subscriptionRow).toHaveCount(0);
});

test("calendar day rail: shows shared presence without owner items", async ({ authenticatedPage: page }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2330));
  const sharedUsername = "shared-rail-user";

  await mockSharedPresenceRoute(page, focusDate, sharedUsername);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const periodTripsResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/trip-sharing/period-trips") &&
      response.request().method() === "GET"
  );

  await setCalendarViewDate(page, focusDate);
  expect((await periodTripsResponsePromise).ok()).toBeTruthy();

  await expect(page.locator(".calendar-empty")).toHaveCount(0);
  await expect(page.locator(".calendar-calendar-card")).toHaveCount(0);
  await expect(page.locator(".calendar-presence-rail")).toBeVisible();

  const sharedSegments = page.locator(`.calendar-presence-rail__segment[data-username="${sharedUsername}"]`);
  await expect(sharedSegments).toHaveCount(3);
  await expect(page.locator(`.calendar-presence-rail__segment--unknown[data-username="${sharedUsername}"]`)).toHaveCount(1);
  await expect(page.locator(`.calendar-presence-rail__segment--transit[data-username="${sharedUsername}"]`)).toHaveCount(1);
  await expect(page.locator(`.calendar-presence-rail__segment--place[data-username="${sharedUsername}"]`)).toHaveCount(1);
});

test("calendar day rail: keeps three shared users directly visible without overflow", async ({ authenticatedPage: page }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2335));
  const sharedUsers = [
    { username: "shared-visible-alice" },
    { username: "shared-visible-bob" },
    { username: "shared-visible-carol" }
  ];

  await mockSharedPresenceRoute(page, focusDate, sharedUsers);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  await expect(page.locator(".calendar-presence-overflow")).toHaveCount(0);
  for (const sharedUser of sharedUsers) {
    await expect(page.locator(`.calendar-presence-rail__segment[data-username="${sharedUser.username}"]`)).toHaveCount(3);
  }
});

test("calendar day rail: desktop overflow shows hidden shared users", async ({ authenticatedPage: page }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2338));
  const sharedUsers = [
    { username: "shared-overflow-alice" },
    { username: "shared-overflow-bob" },
    { username: "shared-overflow-carol" },
    { username: "shared-overflow-dave" }
  ];

  await mockSharedPresenceRoute(page, focusDate, sharedUsers);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const overflowButton = page.locator(".calendar-presence-overflow");
  await expect(overflowButton).toBeVisible();
  await expect(overflowButton).toHaveText("+1");

  await expect(page.locator('.calendar-presence-rail__segment[data-username="shared-overflow-alice"]')).toHaveCount(3);
  await expect(page.locator('.calendar-presence-rail__segment[data-username="shared-overflow-bob"]')).toHaveCount(3);
  await expect(page.locator('.calendar-presence-rail__segment[data-username="shared-overflow-carol"]')).toHaveCount(3);
  await expect(page.locator('.calendar-presence-rail__segment[data-username="shared-overflow-dave"]')).toHaveCount(0);

  await overflowButton.click();

  const sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await expect(sheet).toContainText("shared-overflow-dave");
  await expect(sheet).toContainText("Trajet: Paris → St Clair");
  await expect(sheet).toContainText("Lieu: St Clair");
});

test("calendar day rail: hover and focus reveal presence inspection on desktop", async ({ authenticatedPage: page }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2340));
  const sharedUsername = "shared-inspection-user";

  await mockSharedPresenceRoute(page, focusDate, sharedUsername);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const unknownSegment = page.locator(`.calendar-presence-rail__segment--unknown[data-username="${sharedUsername}"]`).first();
  const transitSegment = page.locator(`.calendar-presence-rail__segment--transit[data-username="${sharedUsername}"]`).first();
  const placeSegment = page.locator(`.calendar-presence-rail__segment--place[data-username="${sharedUsername}"]`).first();
  const inspection = page.locator(".calendar-presence-inspection");

  await unknownSegment.hover();
  await expect(inspection).toBeVisible();
  await expect(inspection).toContainText(sharedUsername);
  await expect(inspection).toContainText("Lieu inconnu");
  await expect(inspection).toContainText("De 00:00 à 08:00");

  await transitSegment.focus();
  await expect(inspection).toContainText("Trajet: Paris → St Clair");
  await expect(inspection).toContainText("De 08:00 à 09:00");

  await placeSegment.hover();
  await expect(inspection).toContainText("Lieu: St Clair");
  await expect(inspection).toContainText("De 09:00 à 24:00");
});

test("calendar day rail: desktop hover keeps inspection open while interacting with cue editor", async ({ authenticatedPage: page, authIdentity }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2342));
  const sharedUsername = "shared-hover-panel-user";

  await mockSharedPresenceRoute(page, focusDate, sharedUsername);
  await seedCuePreferenceOwner(page, authIdentity.username);
  await page.reload();

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const placeSegment = page.locator(`.calendar-presence-rail__segment--place[data-username="${sharedUsername}"]`).first();
  const inspection = page.locator(".calendar-presence-inspection");

  await placeSegment.hover();
  await expect(inspection).toBeVisible();
  await inspection.hover();
  await inspection.locator('[data-cue-color-token="amber"]').click();

  await expect(inspection).toBeVisible();
  await expect(placeSegment).toHaveClass(/calendar-presence-rail__segment--color-amber/);
});

test("calendar day rail: desktop hover inspection closes after leaving segment and panel", async ({ authenticatedPage: page }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2343));
  const sharedUsername = "shared-hover-close-user";

  await mockSharedPresenceRoute(page, focusDate, sharedUsername);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const placeSegment = page.locator(`.calendar-presence-rail__segment--place[data-username="${sharedUsername}"]`).first();
  const inspection = page.locator(".calendar-presence-inspection");
  const outsideTarget = page.locator(".calendar-calendar-title").first();

  await placeSegment.hover();
  await expect(inspection).toBeVisible();
  await inspection.hover();
  await outsideTarget.hover();

  await expect(inspection).toHaveCount(0);
});

test("calendar day rail: desktop cue personalization updates immediately and persists after reload", async ({ authenticatedPage: page, authIdentity }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2345));
  const sharedUsername = "shared-color-user";

  await mockSharedPresenceRoute(page, focusDate, sharedUsername);
  await seedCuePreferenceOwner(page, authIdentity.username);
  await page.reload();

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const placeSegment = page.locator(`.calendar-presence-rail__segment--place[data-username="${sharedUsername}"]`).first();
  await placeSegment.click();

  const inspection = page.locator(".calendar-presence-inspection");
  await expect(inspection).toBeVisible();
  await inspection.locator('[data-cue-color-token="green"]').click();

  await expect(placeSegment).toHaveClass(/calendar-presence-rail__segment--color-green/);

  await page.reload();
  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  await expect(page.locator(`.calendar-presence-rail__segment--place[data-username="${sharedUsername}"]`).first()).toHaveClass(/calendar-presence-rail__segment--color-green/);
});

test("calendar day rail: tap reveals presence inspection on mobile", async ({ authenticatedPage: page }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2350));
  const sharedUsername = "shared-mobile-user";

  await page.setViewportSize({ width: 390, height: 844 });
  await mockSharedPresenceRoute(page, focusDate, sharedUsername);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const transitSegment = page.locator(`.calendar-presence-rail__segment--transit[data-username="${sharedUsername}"]`).first();
  await transitSegment.click();

  const sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await expect(sheet).toContainText(sharedUsername);
  await expect(sheet).toContainText("Trajet: Paris → St Clair");
  await expect(sheet).toContainText("De 08:00 à 09:00");
});

test("calendar day rail: mobile overflow opens hidden shared users sheet", async ({ authenticatedPage: page }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2352));
  const sharedUsers = [
    { username: "shared-mobile-overflow-alice" },
    { username: "shared-mobile-overflow-bob" },
    { username: "shared-mobile-overflow-carol" },
    { username: "shared-mobile-overflow-dave" }
  ];

  await page.setViewportSize({ width: 390, height: 844 });
  await mockSharedPresenceRoute(page, focusDate, sharedUsers);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const overflowButton = page.locator(".calendar-presence-overflow");
  await expect(overflowButton).toBeVisible();
  await expect(overflowButton).toHaveText("+1");

  await overflowButton.click();

  const sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await expect(sheet).toContainText("shared-mobile-overflow-dave");
  await expect(sheet).toContainText("Lieu inconnu");
  await expect(sheet).toContainText("Lieu: St Clair");
});

test("calendar day rail: mobile inspection sheet edits place cue colors", async ({ authenticatedPage: page, authIdentity }) => {
  const focusDate = toLocalDateInput(isolatedFutureDate(2355));
  const sharedUsername = "shared-mobile-color-user";

  await page.setViewportSize({ width: 390, height: 844 });
  await mockSharedPresenceRoute(page, focusDate, sharedUsername);
  await seedCuePreferenceOwner(page, authIdentity.username);
  await page.reload();

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const placeSegment = page.locator(`.calendar-presence-rail__segment--place[data-username="${sharedUsername}"]`).first();
  await placeSegment.click();

  const sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await sheet.locator('[data-cue-color-token="violet"]').click();

  await expect(placeSegment).toHaveClass(/calendar-presence-rail__segment--color-violet/);
});

test("calendar integration: create item on a later day", async ({ authenticatedPage: page }) => {
  const tomorrow = new Date(Date.now() + 24 * 60 * 60 * 1000);
  const focusDate = toLocalDateInput(tomorrow);

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  await setCalendarViewDate(page, focusDate);

  const start = new Date(`${focusDate}T10:00`);
  const end = new Date(`${focusDate}T11:00`);
  const title = `Future ${Date.now()}`;

  const modal = await openCreateModal(page);
  await modal.getByPlaceholder("Titre").fill(title);
  await modal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await modal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );
  const listResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "GET"
  );

  await modal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const listResponse = await listResponsePromise;
  expect(listResponse.ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();

  await page.getByRole("button", { name: "Semaine" }).click();
  await page.getByRole("button", { name: "Jour" }).click();
  await setCalendarViewDate(page, focusDate);

  await expect(row).toBeVisible();
  await expect(page.locator("[data-day-focus-item='true']", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  })).toBeVisible();
});

test("calendar desktop: drag and drop keeps the focused date and dropped time", async ({ authenticatedPage: page }) => {
  const targetDate = isolatedFutureDate(2130);
  const focusDate = toLocalDateInput(targetDate);
  const todayDate = toLocalDateInput(new Date());
  const title = `Drag desktop ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(`${focusDate}T09:00`);
  await createModal.getByPlaceholder("Fin").fill(`${focusDate}T10:00`);

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  expect((await createResponsePromise).ok()).toBeTruthy();

  const updateResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );
  const refreshResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "GET"
  );

  await dragCalendarItemToMinute(page, title, 14 * 60 + 30);

  expect((await updateResponsePromise).ok()).toBeTruthy();
  expect((await refreshResponsePromise).ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();
  await expect(row).toBeVisible();
  await expect(row.locator(".calendar-calendar-item-time")).toHaveText("14:30 → 15:30");

  await page.getByRole("button", { name: "Semaine" }).click();
  await page.getByRole("button", { name: "Jour" }).click();
  await setCalendarViewDate(page, focusDate);
  await expect(row).toBeVisible();
  await expect(row.locator(".calendar-calendar-item-time")).toHaveText("14:30 → 15:30");

  await setCalendarViewDate(page, todayDate);
  await expect(page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  })).toHaveCount(0);
});

test("calendar: create modal resets on cancel", async ({ authenticatedPage: page }) => {
  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  let modal = await openCreateModal(page);
  await modal.getByPlaceholder("Titre").fill("Temp");
  await modal.getByRole("button", { name: "Annuler" }).click();
  await expect(page.locator(".app-modal__dialog")).toHaveCount(0);

  modal = await openCreateModal(page);
  await expect(modal.getByPlaceholder("Titre")).toHaveValue("");
});

test("calendar: create fab stays visible across views", async ({ authenticatedPage: page }) => {
  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const fab = page.getByRole("button", { name: "Nouvel item" });
  await expect(fab).toBeVisible();

  await page.getByRole("button", { name: "Semaine" }).click();
  await expect(fab).toBeVisible();

  await page.getByRole("button", { name: "Mois" }).click();
  await expect(fab).toBeVisible();
});

test("calendar integration: edit item via modal", async ({ authenticatedPage: page }) => {
  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Task ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await createModal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await row.scrollIntoViewIfNeeded();

  await row.locator("..").getByRole("button", { name: "Editer" }).click({ force: true });
  const modal = page.locator(".app-modal__dialog");
  await expect(modal.getByText("Modifier l'item")).toBeVisible();

  const updatedTitle = `${title} (modifie)`;
  await modal.getByPlaceholder("Titre").fill(updatedTitle);
  await modal.getByPlaceholder("Ex: 30").fill("35");

  const updateResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );
  const listResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "GET"
  );

  await modal.getByRole("button", { name: "Valider" }).click();
  const updateResponse = await updateResponsePromise;
  expect(updateResponse.ok()).toBeTruthy();

  const listResponse = await listResponsePromise;
  expect(listResponse.ok()).toBeTruthy();

  const listBody = await listResponse.json();
  const titles = Array.isArray(listBody) ? listBody.map(item => item.titre || "") : [];
  expect(titles).toContain(updatedTitle);
});

test("calendar integration: escape closes edit modal", async ({ authenticatedPage: page }) => {
  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Task ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await createModal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await row.locator("..").getByRole("button", { name: "Editer" }).click({ force: true });

  const modal = page.locator(".app-modal__dialog");
  await expect(modal.getByText("Modifier l'item")).toBeVisible();

  await modal.locator(".app-modal__focus").focus();
  await page.keyboard.press("Escape");
  await expect(modal).toHaveCount(0);
});

test("calendar desktop: edit button is visible in timeline", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 1280, height: 800 });

  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Desktop edit ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await createModal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await expect(row.locator("..").getByRole("button", { name: "Editer" })).toBeVisible();
});

test("calendar desktop: double click does not open edit modal", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 1280, height: 800 });
  await page.waitForFunction(() => window.innerWidth >= 1024);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Desktop ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await createModal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await expect(row.locator("..").getByRole("button", { name: "Editer" })).toBeVisible();
  const box = await row.boundingBox();
  if (!box) {
    throw new Error("Missing bounding box for calendar item");
  }
  await row.dblclick({ position: { x: box.width / 2, y: box.height / 2 }, force: true });
  await expect(page.locator(".app-modal__dialog")).toHaveCount(0);
});

test.describe("calendar mobile touch", () => {
  test.use({ hasTouch: true });

test("calendar mobile: overlapping items render as a stacked deck", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => window.innerWidth <= 768);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const targetDate = isolatedFutureDate(30);
  const focusDate = toLocalDateInput(targetDate);
  const firstTitle = `Mobile overlap A ${Date.now()}`;
  const secondTitle = `Mobile overlap B ${Date.now()}`;
  const startHour = 6 + (Math.floor(Date.now() / 60000) % 12);
  const startLabel = `${String(startHour).padStart(2, "0")}:00`;
  const overlapLabel = `${String(startHour).padStart(2, "0")}:15`;
  const endLabel = `${String(startHour + 1).padStart(2, "0")}:00`;
  const overlapEndLabel = `${String(startHour + 1).padStart(2, "0")}:15`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const firstModal = await openCreateModal(page);
  await firstModal.getByPlaceholder("Titre").fill(firstTitle);
  await firstModal.getByPlaceholder("Début").fill(`${focusDate}T${startLabel}`);
  await firstModal.getByPlaceholder("Fin").fill(`${focusDate}T${endLabel}`);
  let createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );
  await firstModal.getByRole("button", { name: "Valider" }).click();
  expect((await createResponsePromise).ok()).toBeTruthy();

  const secondModal = await openCreateModal(page);
  await secondModal.getByPlaceholder("Titre").fill(secondTitle);
  await secondModal.getByPlaceholder("Début").fill(`${focusDate}T${overlapLabel}`);
  await secondModal.getByPlaceholder("Fin").fill(`${focusDate}T${overlapEndLabel}`);
  createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );
  await secondModal.getByRole("button", { name: "Valider" }).click();
  expect((await createResponsePromise).ok()).toBeTruthy();

  const stack = page.locator(".calendar-calendar-stack", {
    has: page.locator(".calendar-calendar-item-title", { hasText: firstTitle })
  }).first();
  await expect(stack).toBeVisible();
  await expect
    .poll(async () => await stack.locator(".calendar-calendar-card--shadow").count())
    .toBeGreaterThanOrEqual(1);
  await expect(stack.locator(".calendar-calendar-item-title", { hasText: firstTitle })).toBeVisible();

  const [stackBox, gridBox] = await Promise.all([
    stack.boundingBox(),
    page.locator(".calendar-calendar-grid").boundingBox()
  ]);

  if (!stackBox || !gridBox) {
    throw new Error("Missing mobile overlap layout metrics");
  }

  expect(stackBox.width).toBeGreaterThan(gridBox.width * 0.7);
});

test("calendar mobile: hidden cards communicate different start and end times", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => window.innerWidth <= 768);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const targetDate = isolatedFutureDate(330);
  const focusDate = toLocalDateInput(targetDate);
  const topTitle = `Mobile geometry top ${Date.now()}`;
  const hiddenEarlyTitle = `Mobile geometry hidden early ${Date.now()}`;
  const hiddenLateTitle = `Mobile geometry hidden late ${Date.now()}`;
  const startHour = 6 + (Math.floor(Date.now() / 60000) % 12);
  const hour = String(startHour).padStart(2, "0");
  const nextHour = String(startHour + 1).padStart(2, "0");

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const createItems = [
    { title: topTitle, start: `${focusDate}T${hour}:00`, end: `${focusDate}T${hour}:45` },
    { title: hiddenEarlyTitle, start: `${focusDate}T${hour}:10`, end: `${focusDate}T${nextHour}:10` },
    { title: hiddenLateTitle, start: `${focusDate}T${hour}:20`, end: `${focusDate}T${hour}:50` }
  ];

  for (const item of createItems) {
    const modal = await openCreateModal(page);
    await modal.getByPlaceholder("Titre").fill(item.title);
    await modal.getByPlaceholder("Début").fill(item.start);
    await modal.getByPlaceholder("Fin").fill(item.end);
    const createResponsePromise = page.waitForResponse(
      response =>
        response.url().includes("/api/v1/calendar-items") &&
        response.request().method() === "POST"
    );
    await modal.getByRole("button", { name: "Valider" }).click();
    expect((await createResponsePromise).ok()).toBeTruthy();
  }

  const stack = page.locator(".calendar-calendar-stack", {
    has: page.locator(".calendar-calendar-item-title", { hasText: topTitle })
  }).first();
  await expect(stack).toBeVisible();
  await expect(stack.locator(".calendar-calendar-card--shadow")).toHaveCount(2);

  const shadows = stack.locator(".calendar-calendar-card--shadow");
  const firstShadowBox = await shadows.nth(0).boundingBox();
  const secondShadowBox = await shadows.nth(1).boundingBox();
  const topCardBox = await stack.locator(".calendar-calendar-stack-top").boundingBox();

  if (!firstShadowBox || !secondShadowBox || !topCardBox) {
    throw new Error("Missing mobile overlap geometry metrics");
  }

  expect(firstShadowBox.y).toBeGreaterThan(topCardBox.y);
  expect(secondShadowBox.y).toBeGreaterThan(firstShadowBox.y);
  expect(firstShadowBox.height).toBeGreaterThan(secondShadowBox.height);
  expect(firstShadowBox.y + firstShadowBox.height).toBeGreaterThan(topCardBox.y + topCardBox.height);
});

test("calendar mobile: overlap summary opens a bottom sheet in chronological order", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => window.innerWidth <= 768);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const targetDate = isolatedFutureDate(630);
  const focusDate = toLocalDateInput(targetDate);
  const firstTitle = `Mobile sheet A ${Date.now()}`;
  const secondTitle = `Mobile sheet B ${Date.now()}`;
  const thirdTitle = `Mobile sheet C ${Date.now()}`;
  const startHour = 6 + (Math.floor(Date.now() / 60000) % 12);
  const hour = String(startHour).padStart(2, "0");
  const nextHour = String(startHour + 1).padStart(2, "0");

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const createItems = [
    { title: firstTitle, start: `${focusDate}T${hour}:00`, end: `${focusDate}T${hour}:45` },
    { title: secondTitle, start: `${focusDate}T${hour}:10`, end: `${focusDate}T${nextHour}:00` },
    { title: thirdTitle, start: `${focusDate}T${hour}:20`, end: `${focusDate}T${hour}:50` }
  ];

  for (const item of createItems) {
    const modal = await openCreateModal(page);
    await modal.getByPlaceholder("Titre").fill(item.title);
    await modal.getByPlaceholder("Début").fill(item.start);
    await modal.getByPlaceholder("Fin").fill(item.end);
    const createResponsePromise = page.waitForResponse(
      response =>
        response.url().includes("/api/v1/calendar-items") &&
        response.request().method() === "POST"
    );
    await modal.getByRole("button", { name: "Valider" }).click();
    expect((await createResponsePromise).ok()).toBeTruthy();
  }

  const stack = page.locator(".calendar-calendar-stack", {
    has: page.locator(".calendar-calendar-item-title", { hasText: firstTitle })
  }).first();
  await expect(stack).toBeVisible();

  const summaryButton = stack.getByRole("button", { name: /Afficher 2 elements superposes/ });
  await expect(summaryButton).toBeVisible();
  await summaryButton.click();

  const sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await expect(sheet.getByText("Elements superposes")).toBeVisible();

  const items = sheet.locator(".calendar-overlap-sheet__item");
  await expect(items).toHaveCount(3);
  await expect(items.nth(0)).toContainText(firstTitle);
  await expect(items.nth(1)).toContainText(secondTitle);
  await expect(items.nth(2)).toContainText(thirdTitle);

  await sheet.getByRole("button", { name: "Fermer" }).click();
  await expect(page.locator(".app-bottom-sheet__dialog")).toHaveCount(0);
});

test("calendar mobile: overlap summary tap and long-press drag both work", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => window.innerWidth <= 768);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const targetDate = isolatedFutureDate(780);
  const focusDate = toLocalDateInput(targetDate);
  const firstTitle = `Mobile tap drag A ${Date.now()}`;
  const secondTitle = `Mobile tap drag B ${Date.now()}`;
  const thirdTitle = `Mobile tap drag C ${Date.now()}`;
  const dragTitle = `Mobile tap drag item ${Date.now()}`;
  const startHour = 6 + (Math.floor(Date.now() / 60000) % 12);
  const hour = String(startHour).padStart(2, "0");
  const nextHour = String(startHour + 1).padStart(2, "0");
  const dragHour = String(startHour + 3).padStart(2, "0");
  const dragNextHour = String(startHour + 4).padStart(2, "0");

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const createItems = [
    { title: firstTitle, start: `${focusDate}T${hour}:00`, end: `${focusDate}T${hour}:45` },
    { title: secondTitle, start: `${focusDate}T${hour}:10`, end: `${focusDate}T${nextHour}:00` },
    { title: thirdTitle, start: `${focusDate}T${hour}:20`, end: `${focusDate}T${hour}:50` },
    { title: dragTitle, start: `${focusDate}T${dragHour}:00`, end: `${focusDate}T${dragNextHour}:00` }
  ];

  for (const item of createItems) {
    const modal = await openCreateModal(page);
    await modal.getByPlaceholder("Titre").fill(item.title);
    await modal.getByPlaceholder("Début").fill(item.start);
    await modal.getByPlaceholder("Fin").fill(item.end);
    const createResponsePromise = page.waitForResponse(
      response =>
        response.url().includes("/api/v1/calendar-items") &&
        response.request().method() === "POST"
    );
    await modal.getByRole("button", { name: "Valider" }).click();
    expect((await createResponsePromise).ok()).toBeTruthy();
  }

  const stack = page.locator(".calendar-calendar-stack", {
    has: page.locator(".calendar-calendar-item-title", { hasText: firstTitle })
  }).first();
  await expect(stack).toBeVisible();

  const summaryButton = stack.getByRole("button", { name: /Afficher 2 elements superposes/ });
  await expect(summaryButton).toBeVisible();
  await summaryButton.click();

  const sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await sheet.getByRole("button", { name: "Fermer" }).click();
  await expect(page.locator(".app-bottom-sheet__dialog")).toHaveCount(0);

  const updateResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );
  const refreshResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "GET"
  );

  await longPressDragCalendarItemToMinute(page, dragTitle, (startHour + 5) * 60 + 30);

  expect((await updateResponsePromise).ok()).toBeTruthy();
  expect((await refreshResponsePromise).ok()).toBeTruthy();

  const draggedRow = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: dragTitle })
  }).first();
  await expect(draggedRow).toBeVisible();
  await expect(draggedRow.locator(".calendar-calendar-item-time")).toHaveText(`${String(startHour + 5).padStart(2, "0")}:30 → ${String(startHour + 6).padStart(2, "0")}:30`);
});

test("calendar mobile: selecting a hidden overlap item promotes it until leaving Day view", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => window.innerWidth <= 768);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const targetDate = isolatedFutureDate(930);
  const focusDate = toLocalDateInput(targetDate);
  const otherDate = toLocalDateInput(isolatedFutureDate(1230));
  const firstTitle = `Mobile promote A ${Date.now()}`;
  const secondTitle = `Mobile promote B ${Date.now()}`;
  const thirdTitle = `Mobile promote C ${Date.now()}`;
  const startHour = 6 + (Math.floor(Date.now() / 60000) % 12);
  const hour = String(startHour).padStart(2, "0");
  const nextHour = String(startHour + 1).padStart(2, "0");

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const createItems = [
    { title: firstTitle, start: `${focusDate}T${hour}:00`, end: `${focusDate}T${hour}:45` },
    { title: secondTitle, start: `${focusDate}T${hour}:10`, end: `${focusDate}T${nextHour}:00` },
    { title: thirdTitle, start: `${focusDate}T${hour}:20`, end: `${focusDate}T${hour}:50` }
  ];

  for (const item of createItems) {
    const modal = await openCreateModal(page);
    await modal.getByPlaceholder("Titre").fill(item.title);
    await modal.getByPlaceholder("Début").fill(item.start);
    await modal.getByPlaceholder("Fin").fill(item.end);
    const createResponsePromise = page.waitForResponse(
      response =>
        response.url().includes("/api/v1/calendar-items") &&
        response.request().method() === "POST"
    );
    await modal.getByRole("button", { name: "Valider" }).click();
    expect((await createResponsePromise).ok()).toBeTruthy();
  }

  const defaultStack = page.locator(".calendar-calendar-stack", {
    has: page.locator(".calendar-calendar-item-title", { hasText: firstTitle })
  }).first();
  await expect(defaultStack).toBeVisible();

  await defaultStack.getByRole("button", { name: /Afficher 2 elements superposes/ }).click();

  let sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await expect(sheet.locator(".calendar-overlap-sheet__item--active")).toContainText(firstTitle);
  await expect(sheet.locator(".calendar-overlap-sheet__item--active")).toBeDisabled();

  const promotedRow = sheet.locator(".calendar-overlap-sheet__item", { hasText: thirdTitle });
  await expect(promotedRow).toBeEnabled();
  await promotedRow.click();
  await expect(page.locator(".app-bottom-sheet__dialog")).toHaveCount(0);

  const promotedStack = page.locator(".calendar-calendar-stack", {
    has: page.locator(".calendar-calendar-stack-top .calendar-calendar-item-title", { hasText: thirdTitle })
  }).first();
  await expect(promotedStack).toBeVisible();
  await expect(promotedStack.locator(".calendar-calendar-stack-top .calendar-calendar-item-title", { hasText: thirdTitle })).toBeVisible();

  await setCalendarViewDate(page, otherDate);
  await setCalendarViewDate(page, focusDate);
  await expect(promotedStack.locator(".calendar-calendar-stack-top .calendar-calendar-item-title", { hasText: thirdTitle })).toBeVisible();

  await promotedStack.getByRole("button", { name: /Afficher 2 elements superposes/ }).click();
  sheet = page.locator(".app-bottom-sheet__dialog");
  await expect(sheet).toBeVisible();
  await expect(sheet.locator(".calendar-overlap-sheet__item--active")).toContainText(thirdTitle);
  await expect(sheet.locator(".calendar-overlap-sheet__item--active")).toBeDisabled();
  await sheet.getByRole("button", { name: "Fermer" }).click();

  await page.getByRole("button", { name: "Semaine" }).click();
  await page.getByRole("button", { name: "Jour" }).click();

  const resetStack = page.locator(".calendar-calendar-stack", {
    has: page.locator(".calendar-calendar-stack-top .calendar-calendar-item-title", { hasText: firstTitle })
  }).first();
  await expect(resetStack).toBeVisible();
});

test("calendar mobile: localized date chip stays aligned with the Day header", async ({ authenticatedPage: page }) => {
    await page.setViewportSize({ width: 390, height: 844 });

  const tomorrow = new Date(Date.now() + 24 * 60 * 60 * 1000);
  const focusDate = toLocalDateInput(tomorrow);
  const expectedLabel = toFrenchDayLabel(tomorrow);
  const start = new Date(`${focusDate}T10:00`);
  const end = new Date(`${focusDate}T11:00`);
  const title = `Mobile date ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await createModal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  await setCalendarViewDate(page, focusDate);

  const trigger = page.locator(".calendar-view-date-trigger");
  await expect(trigger).toBeVisible();
  await expect(trigger).toHaveText(expectedLabel);
  await expect(trigger).not.toHaveText(focusDate);
  await expect(page.locator(".calendar-calendar-title")).toHaveText(expectedLabel);

  await trigger.click();
  await expect(page.locator(".calendar-view-date")).toBeFocused();
});

test("calendar mobile: double tap does not open edit modal", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => window.innerWidth <= 768);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const targetDate = isolatedFutureDate(1530);
  const focusDate = toLocalDateInput(targetDate);
  const start = new Date(`${focusDate}T10:00`);
  const end = new Date(`${focusDate}T11:00`);
  const title = `Mobile tap ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await createModal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await row.scrollIntoViewIfNeeded();

  const box = await row.boundingBox();
  if (!box) {
    throw new Error("Missing bounding box for calendar item");
  }

  const x = box.x + box.width / 2;
  const y = box.y + box.height / 2;
  await page.touchscreen.tap(x, y);
  await page.waitForTimeout(120);
  await page.touchscreen.tap(x, y);

  await expect(page.getByText("Modifier l'item")).toHaveCount(0);
});
});

test("calendar mobile actions: secondary actions stay reachable across views", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const actionsButton = page.getByRole("button", { name: "Actions" });

  await expect(actionsButton).toBeVisible();

  await page.getByRole("button", { name: "Semaine" }).click();
  await expect(actionsButton).toBeVisible();

  await page.getByRole("button", { name: "Mois" }).click();
  await expect(actionsButton).toBeVisible();

  await actionsButton.click();
  await expect(page.getByRole("button", { name: "Partage trajets" })).toBeVisible();
  await expect(page.getByRole("button", { name: "Import CSV" })).toBeVisible();
  await expect(page.getByRole("button", { name: "Import ICS" })).toBeVisible();
  await expect(page.getByRole("button", { name: "Export" })).toBeVisible();
  await expect(page.getByRole("button", { name: "Templates" })).toHaveCount(0);
  await expect(page.getByRole("button", { name: "Filtres" })).toHaveCount(0);
});

test("calendar mobile: edit button is not rendered", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });
  await page.waitForFunction(() => window.innerWidth <= 768);
  await page.evaluate(() => window.dispatchEvent(new Event("resize")));
  await page.reload();

  const targetDate = isolatedFutureDate(1830);
  const focusDate = toLocalDateInput(targetDate);
  const start = new Date(`${focusDate}T10:00`);
  const end = new Date(`${focusDate}T11:00`);
  const title = `Mobile edit ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);
  await setCalendarViewDate(page, focusDate);

  const createModal = await openCreateModal(page);
  await createModal.getByPlaceholder("Titre").fill(title);
  await createModal.getByPlaceholder("Début").fill(toLocalDatetimeInput(start));
  await createModal.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));

  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await createModal.getByRole("button", { name: "Valider" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const row = page.locator(".calendar-calendar-card", {
    has: page.locator(".calendar-calendar-item-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await expect(page.getByRole("button", { name: "Editer" })).toHaveCount(0);
});
