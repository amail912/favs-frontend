export const scrollElementIntoView = element => () => {
  if (!element || typeof element.scrollIntoView !== "function") {
    return;
  }

  element.scrollIntoView({
    block: "start",
    inline: "nearest",
    behavior: "auto"
  });
};

export const viewportVisibleHeight = () => {
  if (typeof window === "undefined") {
    return 0;
  }
  if (window.visualViewport && typeof window.visualViewport.height === "number") {
    return window.visualViewport.height;
  }
  return window.innerHeight;
};
