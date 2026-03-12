export const currentLocalDate = (() => {
  const date = new Date();
  const pad = value => `${value}`.padStart(2, "0");
  return [
    date.getFullYear(),
    pad(date.getMonth() + 1),
    pad(date.getDate())
  ].join("-");
})();

export const formatFrenchDayLabelImpl = includeYear => raw => {
  const date = new Date(`${raw}T12:00:00`);
  if (Number.isNaN(date.getTime())) {
    return raw;
  }

  const formatter = new Intl.DateTimeFormat("fr-FR", {
    weekday: "short",
    day: "numeric",
    month: "short",
    ...(includeYear ? { year: "numeric" } : {})
  });

  return formatter.format(date);
};
