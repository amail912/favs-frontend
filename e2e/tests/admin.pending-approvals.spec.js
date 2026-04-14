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

function approvedRow(page, username) {
  return page.locator(".admin-approved-row", { hasText: username });
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

async function mockEmptyPendingApprovals(page) {
  await page.route("**/api/v1/admin/pending-signups", async route => {
    if (route.request().method() !== "GET") {
      await route.continue();
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: "[]"
    });
  });
}

async function mockApprovedUsersFlow(page, { users, deleteUsername, deleteStatus = 200, deleteBody = "{}" }) {
  let approvedUsers = users;

  await page.route("**/api/v1/admin/users", async route => {
    if (route.request().method() !== "GET") {
      await route.continue();
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: JSON.stringify(approvedUsers)
    });
  });

  if (!deleteUsername) {
    return;
  }

  await page.route(`**/api/v1/admin/users/${deleteUsername}`, async route => {
    if (deleteStatus >= 200 && deleteStatus < 300) {
      approvedUsers = approvedUsers.filter(user => user.username !== deleteUsername);
    }

    await route.fulfill({
      status: deleteStatus,
      contentType: "application/json; charset=utf-8",
      body: deleteBody
    });
  });
}

async function mockEmptyApprovedUsers(page) {
  await page.route("**/api/v1/admin/users", async route => {
    if (route.request().method() !== "GET") {
      await route.continue();
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: "[]"
    });
  });
}

test("admin approves a pending account and sees it disappear", async ({ context, page, baseURL }) => {
  const username = "pending-approve-user";
  await signInAsAdmin(context, page, baseURL);
  await mockPendingApprovalsFlow(page, { username, action: "approve" });
  await mockEmptyApprovedUsers(page);

  await page.goto("/admin");
  await expect(pendingRow(page, username)).toBeVisible();
  await pendingRow(page, username).getByRole("button", { name: "Approuver" }).click();
  await expect(pendingRow(page, username)).toHaveCount(0);
});

test("admin deletes a pending account and sees it disappear", async ({ context, page, baseURL }) => {
  const username = "pending-delete-user";
  await signInAsAdmin(context, page, baseURL);
  await mockPendingApprovalsFlow(page, { username, action: "delete" });
  await mockEmptyApprovedUsers(page);

  await page.goto("/admin");
  await expect(pendingRow(page, username)).toBeVisible();
  await pendingRow(page, username).getByRole("button", { name: "Supprimer" }).click();
  await expect(pendingRow(page, username)).toHaveCount(0);
});

test("admin page shows an explicit empty state when there are no pending accounts", async ({ context, page, baseURL }) => {
  await signInAsAdmin(context, page, baseURL);
  await mockEmptyApprovedUsers(page);

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
  await mockEmptyApprovedUsers(page);

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

test("admin cancels approved-user deletion and keeps the user visible", async ({ context, page, baseURL }) => {
  const users = [
    { username: "admin", roles: ["admin"], approved: true },
    { username: "existing-user", roles: ["member"], approved: true }
  ];

  await signInAsAdmin(context, page, baseURL);
  await mockEmptyPendingApprovals(page);
  await mockApprovedUsersFlow(page, { users, deleteUsername: "existing-user" });

  await page.goto("/admin");
  await expect(approvedRow(page, "existing-user")).toBeVisible();
  await approvedRow(page, "existing-user").getByRole("button", { name: "Supprimer" }).click();

  const modal = page.locator(".admin-delete-confirm-modal .app-modal__dialog");
  await expect(modal).toBeVisible();
  await modal.getByRole("button", { name: "Annuler" }).click();
  await expect(page.locator(".admin-delete-confirm-modal")).toHaveCount(0);
  await expect(approvedRow(page, "existing-user")).toBeVisible();
});

test("admin modal: browser back closes delete confirmation without validating", async ({ context, page, baseURL }) => {
  const users = [
    { username: "admin", roles: ["admin"], approved: true },
    { username: "existing-user", roles: ["member"], approved: true }
  ];
  let deleteRequestCount = 0;

  await signInAsAdmin(context, page, baseURL);
  await mockEmptyPendingApprovals(page);

  await page.route("**/api/v1/admin/users", async route => {
    if (route.request().method() !== "GET") {
      await route.continue();
      return;
    }

    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: JSON.stringify(users)
    });
  });

  await page.route("**/api/v1/admin/users/existing-user", async route => {
    deleteRequestCount += 1;
    await route.fulfill({
      status: 200,
      contentType: "application/json; charset=utf-8",
      body: "{}"
    });
  });

  await page.goto("/admin");
  await expect(approvedRow(page, "existing-user")).toBeVisible();
  await approvedRow(page, "existing-user").getByRole("button", { name: "Supprimer" }).click();

  const modal = page.locator(".admin-delete-confirm-modal .app-modal__dialog");
  await expect(modal).toBeVisible();

  await page.goBack();

  await expect(page.locator(".admin-delete-confirm-modal")).toHaveCount(0);
  await expect(approvedRow(page, "existing-user")).toBeVisible();
  expect(deleteRequestCount).toBe(0);
});

test("admin confirms approved-user deletion and sees the user disappear", async ({ context, page, baseURL }) => {
  const users = [
    { username: "admin", roles: ["admin"], approved: true },
    { username: "existing-user", roles: ["member"], approved: true }
  ];

  await signInAsAdmin(context, page, baseURL);
  await mockEmptyPendingApprovals(page);
  await mockApprovedUsersFlow(page, { users, deleteUsername: "existing-user" });

  await page.goto("/admin");
  await approvedRow(page, "existing-user").getByRole("button", { name: "Supprimer" }).click();
  await page.locator(".admin-delete-confirm-modal .app-modal__validate").click();
  await expect(approvedRow(page, "existing-user")).toHaveCount(0);
});

test("admin page keeps the current admin visible but not deletable", async ({ context, page, baseURL }) => {
  const users = [
    { username: "admin", roles: ["admin"], approved: true },
    { username: "existing-user", roles: ["member"], approved: true }
  ];

  await signInAsAdmin(context, page, baseURL);
  await mockEmptyPendingApprovals(page);
  await mockApprovedUsersFlow(page, { users });

  await page.goto("/admin");
  await expect(approvedRow(page, "admin")).toBeVisible();
  await expect(approvedRow(page, "admin").getByRole("button", { name: "Supprimer" })).toBeDisabled();
  await expect(approvedRow(page, "admin").getByText("Suppression indisponible pour votre compte.")).toBeVisible();
});

test("admin page shows actionable feedback and retry when approved-users loading fails", async ({ context, page, baseURL }) => {
  await signInAsAdmin(context, page, baseURL);
  await mockEmptyPendingApprovals(page);

  let callCount = 0;
  await page.route("**/api/v1/admin/users", async route => {
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
      body: JSON.stringify([{ username: "existing-user", roles: ["member"], approved: true }])
    });
  });

  await page.goto("/admin");
  await expect(page.locator(".admin-approved-error")).toContainText("Unable to process authentication");
  await page.locator(".admin-approved-error").getByRole("button", { name: "Reessayer" }).click();
  await expect(approvedRow(page, "existing-user")).toBeVisible();
});

test("admin page surfaces delete conflicts without leaving the page", async ({ context, page, baseURL }) => {
  const users = [
    { username: "admin", roles: ["admin"], approved: true },
    { username: "bootstrap-admin", roles: ["admin"], approved: true }
  ];

  await signInAsAdmin(context, page, baseURL);
  await mockEmptyPendingApprovals(page);
  await mockApprovedUsersFlow(page, {
    users,
    deleteUsername: "bootstrap-admin",
    deleteStatus: 409,
    deleteBody: JSON.stringify({ message: "Cannot delete bootstrap admin" })
  });

  await page.goto("/admin");
  await approvedRow(page, "bootstrap-admin").getByRole("button", { name: "Supprimer" }).click();
  await page.locator(".admin-delete-confirm-modal .app-modal__validate").click();
  await expect(page.locator(".admin-approved-action-feedback")).toContainText("Cannot delete bootstrap admin");
  await expect(approvedRow(page, "bootstrap-admin")).toBeVisible();
});
