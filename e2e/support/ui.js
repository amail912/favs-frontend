function notesTab(page) {
  return page.locator("a.nav-link", { hasText: "Notes" });
}

function checklistsTab(page) {
  return page.locator("a.nav-link", { hasText: "Checklists" });
}

function signupMenuButton(page) {
  return page.getByRole("button", { name: "Signup" });
}

function signoutMenuButton(page) {
  return page.getByRole("button", { name: "Se deconnecter" });
}

function createButton(page) {
  return page.getByRole("button", { name: "+" });
}

function createAccountButton(page) {
  return page.getByRole("button", { name: "Create account" });
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

function appTitle(page) {
  return page.locator("h1", { hasText: "FAVS" });
}

module.exports = {
  appTitle,
  checklistsTab,
  createAccountButton,
  createButton,
  notesTab,
  signupMenuButton,
  signupPasswordInput,
  signupUsernameInput,
  signinUsernameInput,
  signoutMenuButton
};
