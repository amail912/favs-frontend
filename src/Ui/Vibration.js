export const vibrateIfAvailable = (ms) => () => {
  if (typeof navigator !== "undefined" && navigator.vibrate) {
    navigator.vibrate(ms);
  }
};
