function stringToTimestamp(date: string) {
  return date.replace("T", " ").replace("Z", "");
}

function getNetPrice(originalPrice: number, discountPercent: number): number {
  let discountMount = originalPrice * (discountPercent / 100);

  let discountPrice = originalPrice - discountMount;

  return Math.floor(discountPrice);
}

export { stringToTimestamp, getNetPrice };
