function notesTab(page) {
  return page.locator("a.nav-link", { hasText: "Notes" });
}

function checklistsTab(page) {
  return page.locator("a.nav-link", { hasText: "Checklists" });
}

function calendarTab(page) {
  return page.locator("a.nav-link", { hasText: "Calendar" });
}

function adminTab(page) {
  return page.locator("a.nav-link", { hasText: "Admin" });
}

function signupMenuButton(page) {
  return page.getByRole("button", { name: "Signup" });
}

function signoutMenuButton(page) {
  return page.getByRole("button", { name: "Se deconnecter" });
}

function connectedIdentity(page) {
  return page.locator(".auth-menu__identity");
}

function lateItemsChip(page) {
  return page.locator(".app-late-items-chip");
}

function lateItemsSheet(page) {
  return page.locator(".app-late-items-sheet");
}

function lateItemsRows(page) {
  return page.locator(".app-late-items-row");
}

function lateItemsLoadMore(page) {
  return page.locator(".app-late-items-load-more");
}

function createButton(page) {
  return page.getByRole("button", { name: "+" });
}

function createAccountButton(page) {
  return page.getByRole("button", { name: "Create account" });
}

function signInButton(page) {
  return page.getByRole("button", { name: "Sign in" });
}

function signupUsernameInput(page) {
  return page.locator("#signup-username");
}

function signupPasswordInput(page) {
  return page.locator("#signup-password");
}

function signinUsernameInput(page) {
  return page.locator("#signin-username");
}

function signinPasswordInput(page) {
  return page.locator("#signin-password");
}

function appTitle(page) {
  return page.locator("h1", { hasText: "FAVS" });
}

module.exports = {
  adminTab,
  calendarTab,
  appTitle,
  checklistsTab,
  connectedIdentity,
  lateItemsChip,
  lateItemsSheet,
  lateItemsRows,
  lateItemsLoadMore,
  createAccountButton,
  createButton,
  notesTab,
  signInButton,
  signinPasswordInput,
  signupMenuButton,
  signupPasswordInput,
  signupUsernameInput,
  signinUsernameInput,
  signoutMenuButton
};
