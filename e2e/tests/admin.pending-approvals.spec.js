const { test, expect } = require("@playwright/test");
const { signinUser, withExistingUser } = require("../support/auth-session");

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
}

function pendingRow(page, username) {
  return page.locator(".admin-pending-row", { hasText: username });
}

async function mockPendingApprovalsFlow(page, { username, action }) {
  let pendingSignups = [{ username }];

  await page.route("**/api/v1/admin/pending-signups", async route => {
    if (route.request().method() !== "GET") {
      await route.continue();
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: JSON.stringify(pendingSignups)
    });
  });

  if (action === "approve") {
    await page.route("**/api/v1/admin/pending-signups/approve", async route => {
      pendingSignups = [];
      await route.fulfill({
        status: 200,
        contentType: "application/json; charset=utf-8",
        body: "{}"
      });
    });
    return;
  }

  await page.route(`**/api/v1/admin/pending-signups/${username}`, async route => {
    pendingSignups = [];
    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: "{}"
    });
  });
}

test("admin approves a pending account and sees it disappear", async ({ context, page, baseURL }) => {
  const username = "pending-approve-user";
  await signInAsAdmin(context, page, baseURL);
  await mockPendingApprovalsFlow(page, { username, action: "approve" });

  await page.goto("/admin");
  await expect(pendingRow(page, username)).toBeVisible();
  await pendingRow(page, username).getByRole("button", { name: "Approuver" }).click();
  await expect(pendingRow(page, username)).toHaveCount(0);
});

test("admin deletes a pending account and sees it disappear", async ({ context, page, baseURL }) => {
  const username = "pending-delete-user";
  await signInAsAdmin(context, page, baseURL);
  await mockPendingApprovalsFlow(page, { username, action: "delete" });

  await page.goto("/admin");
  await expect(pendingRow(page, username)).toBeVisible();
  await pendingRow(page, username).getByRole("button", { name: "Supprimer" }).click();
  await expect(pendingRow(page, username)).toHaveCount(0);
});

test("admin page shows an explicit empty state when there are no pending accounts", async ({ context, page, baseURL }) => {
  await signInAsAdmin(context, page, baseURL);

  await page.route("**/api/v1/admin/pending-signups", async route => {
    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: "[]"
    });
  });

  await page.goto("/admin");
  await expect(page.getByText("Aucun compte en attente.")).toBeVisible();
});

test("admin page shows actionable feedback and retry when pending loading fails", async ({ context, page, baseURL }) => {
  await signInAsAdmin(context, page, baseURL);

  let callCount = 0;
  await page.route("**/api/v1/admin/pending-signups", async route => {
    callCount += 1;
    if (callCount === 1) {
      await route.fulfill({
        status: 500,
        contentType: "application/json; charset=utf-8",
        body: JSON.stringify({ message: "Unable to process authentication" })
      });
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: JSON.stringify([{ username: "retry-user" }])
    });
  });

  await page.goto("/admin");
  await expect(page.getByText("Unable to process authentication")).toBeVisible();
  await page.getByRole("button", { name: "Reessayer" }).click();
  await expect(pendingRow(page, "retry-user")).toBeVisible();
});
