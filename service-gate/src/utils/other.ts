function stringToTimestamp(date: string) {
  return date.replace("T", " ").replace("Z", "");
}

function getContractPrice(
  productPrice: number,
  discountPercent: number,
  productUnits: number
): number {
  let discountMount = productPrice * (discountPercent / 100);

  let discountPrice = productPrice - discountMount;

  let totalPrice = discountPrice * productUnits;

  return Math.floor(totalPrice);
}

function getContractCollateral(
  productCollateral: number,
  productUnits: number
): number {
  let totalCollateral = productCollateral * productUnits;

  return Math.floor(totalCollateral);
}

export { stringToTimestamp, getContractPrice, getContractCollateral };
