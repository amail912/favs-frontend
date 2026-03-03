const { test, expect } = require("../fixtures/authenticated");
const { calendarTab, appTitle } = require("../support/ui");

function toLocalDatetimeInput(date) {
  const pad = value => `${value}`.padStart(2, "0");
  return [
    date.getFullYear(),
    pad(date.getMonth() + 1),
    pad(date.getDate())
  ].join("-") + "T" + [pad(date.getHours()), pad(date.getMinutes())].join(":");
}

async function openCreateModal(page) {
  await page.getByRole("button", { name: "Nouvel item" }).click();
  const modal = page.locator(".app-modal__dialog", { hasText: "Créer un item" });
  await expect(modal).toBeVisible();
  return modal;
}

test("calendar integration: create intention then planify", async ({ authenticatedPage: page }) => {
  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Intention ${Date.now()}`;

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
  await expect(row.getByRole("button", { name: "Planifier" })).toBeVisible();
  await expect(row.getByRole("button", { name: "Valider" })).toHaveCount(0);

  await row.getByRole("button", { name: "Planifier" }).click();
  await expect(row).toBeVisible();
  await appTitle(page).click();
});

test("calendar integration: create scheduled block via modal toggle", async ({ authenticatedPage: page }) => {
  const now = new Date();
  const start = new Date(now.getTime() + 3 * 60 * 60 * 1000);
  const end = new Date(now.getTime() + 4 * 60 * 60 * 1000);
  const title = `Planifié ${Date.now()}`;

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const modal = await openCreateModal(page);
  await modal.getByRole("button", { name: "Bloc planifié" }).click();
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
  await expect(row.getByRole("button", { name: "Valider" })).toBeVisible();
  await expect(row.getByRole("button", { name: "Planifier" })).toHaveCount(0);
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
  const title = `Intention ${Date.now()}`;

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
  const title = `Intention ${Date.now()}`;

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
  await row.dblclick({ position: { x: 20, y: 20 }, force: true });
  await expect(page.locator(".app-modal__dialog")).toHaveCount(0);
});

test("calendar mobile: double tap opens edit modal", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });

  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Mobile ${Date.now()}`;

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
  await row.dblclick({ force: true });
  await expect(page.getByText("Modifier l'item")).toBeVisible();
});

test.describe("calendar mobile touch", () => {
  test.use({ hasTouch: true });

  test("calendar mobile: touch double tap opens edit modal", async ({ authenticatedPage: page }) => {
    await page.setViewportSize({ width: 390, height: 844 });

  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Mobile tap ${Date.now()}`;

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

  const box = await row.boundingBox();
  if (!box) {
    throw new Error("Missing bounding box for calendar item");
  }

  const x = box.x + box.width / 2;
  const y = box.y + box.height / 2;
  await page.touchscreen.tap(x, y);
  await page.waitForTimeout(120);
  await page.touchscreen.tap(x, y);

    await expect(page.getByText("Modifier l'item")).toBeVisible();
  });
});

test("calendar mobile tools: filters and tools stay visible across views", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });

  await calendarTab(page).click();
  await expect(page).toHaveURL(/\/calendar$/);

  const filtersButton = page.getByRole("button", { name: "Filtres" });
  const toolsButton = page.getByRole("button", { name: "Outils" });

  await expect(filtersButton).toBeVisible();
  await expect(toolsButton).toBeVisible();

  await page.getByRole("button", { name: "Semaine" }).click();
  await expect(filtersButton).toBeVisible();
  await expect(toolsButton).toBeVisible();

  await page.getByRole("button", { name: "Mois" }).click();
  await expect(filtersButton).toBeVisible();
  await expect(toolsButton).toBeVisible();
});

test("calendar mobile: edit button is not rendered", async ({ authenticatedPage: page }) => {
  await page.setViewportSize({ width: 390, height: 844 });

  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Mobile edit ${Date.now()}`;

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
  await expect(page.getByRole("button", { name: "Editer" })).toHaveCount(0);
});
