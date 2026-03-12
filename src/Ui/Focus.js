export const openDateInputPicker = element => () => {
  if (!element) {
    return false;
  }

  const input = element;

  if (typeof input.focus === "function") {
    input.focus();
  }

  if (typeof input.showPicker === "function") {
    input.showPicker();
    return true;
  }

  if (typeof input.click === "function") {
    input.click();
  }

  return true;
};
