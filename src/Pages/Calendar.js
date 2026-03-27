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
