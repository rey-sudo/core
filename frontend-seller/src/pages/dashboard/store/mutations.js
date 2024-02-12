const createProduct = (state, data) => {
  state.productData.push(data);
};

const getProducts = (state, data) => {
  state.productData.push(...data);
};

export { createProduct, getProducts };
