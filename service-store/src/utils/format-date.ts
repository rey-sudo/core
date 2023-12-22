export const formatDate = (date: any) => {
  if (!date) return "Unassigned Date";

  const dateValue = new Date(date);

  return dateValue.toUTCString();
};
