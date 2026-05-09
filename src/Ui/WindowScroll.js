export const getWindowScrollY = () =>
  Math.max(0, Math.trunc(window.scrollY || window.pageYOffset || 0));

export const setWindowScrollY = y => () => {
  window.scrollTo(0, Math.max(0, y));
};
