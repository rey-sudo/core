function getStockStatus(stock: number): string {
  if (stock < 1) {
    return "out";
  }

  if (stock < 10) {
    return "low";
  }

  if (stock < 20) {
    return "stock";
  }

  return "stock";
}

export { getStockStatus };
