export const getItem = key => () => {
  try {
    if (typeof window === "undefined" || !window.localStorage) {
      return "";
    }

    return window.localStorage.getItem(key) || "";
  } catch (error) {
    return "";
  }
};

export const setItem = key => value => () => {
  try {
    if (typeof window === "undefined" || !window.localStorage) {
      return;
    }

    window.localStorage.setItem(key, value);
  } catch (error) {
    return;
  }
};

export const removeItem = key => () => {
  try {
    if (typeof window === "undefined" || !window.localStorage) {
      return;
    }

    window.localStorage.removeItem(key);
  } catch (error) {
    return;
  }
};
