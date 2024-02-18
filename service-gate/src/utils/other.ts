function stringToTimestamp(date: string) {
  return date.replace("T", " ").replace("Z", "");
}


export { stringToTimestamp }