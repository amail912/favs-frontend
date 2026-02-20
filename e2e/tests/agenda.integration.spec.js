const { test, expect } = require("../fixtures/authenticated");
const { agendaTab, appTitle } = require("../support/ui");

function toLocalDatetimeInput(date) {
  const pad = value => `${value}`.padStart(2, "0");
  return [
    date.getFullYear(),
    pad(date.getMonth() + 1),
    pad(date.getDate())
  ].join("-") + "T" + [pad(date.getHours()), pad(date.getMinutes())].join(":");
}

test("agenda integration: create intention then validate", async ({ authenticatedPage: page }) => {
  const now = new Date();
  const start = new Date(now.getTime() + 60 * 60 * 1000);
  const end = new Date(now.getTime() + 2 * 60 * 60 * 1000);
  const title = `Intention ${Date.now()}`;

  await agendaTab(page).click();
  await expect(page).toHaveURL(/\/agenda$/);

  await page.getByPlaceholder("Titre de l'intention").fill(title);
  await page.getByPlaceholder("Debut").fill(toLocalDatetimeInput(start));
  await page.getByPlaceholder("Fin").fill(toLocalDatetimeInput(end));
  const createResponsePromise = page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "POST"
  );

  await page.getByRole("button", { name: "Creer l'intention" }).click();
  const createResponse = await createResponsePromise;
  expect(createResponse.ok()).toBeTruthy();

  const listResponse = await page.waitForResponse(
    response =>
      response.url().includes("/api/v1/calendar-items") &&
      response.request().method() === "GET"
  );
  expect(listResponse.ok()).toBeTruthy();

  const listBody = await listResponse.json();
  const titles = Array.isArray(listBody) ? listBody.map(item => item.titre || "") : [];
  expect(titles).toContain(title);

  const row = page.locator(".agenda-card", {
    has: page.locator(".agenda-card-title", { hasText: title })
  }).first();

  await expect(row).toBeVisible();
  await expect(row.getByRole("button", { name: "Valider" })).toBeVisible();
  await expect(row.getByRole("button", { name: "Planifier" })).toBeVisible();

  await row.getByRole("button", { name: "Valider" }).click();
  const validationPanel = page.locator(".agenda-validation-panel");
  await expect(validationPanel).toBeVisible();

  await validationPanel.getByPlaceholder("Duree reelle (minutes)").fill("30");
  await validationPanel.getByRole("button", { name: "Confirmer" }).click();

  await expect(validationPanel).toHaveCount(0);
  await expect(row).toBeVisible();
  await appTitle(page).click();
});
