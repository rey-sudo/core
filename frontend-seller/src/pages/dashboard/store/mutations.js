const createProduct = (state, data) => {
  state.productData.push(data);
};

const getProducts = (state, data) => {
  state.productData.length = 0;
  state.productData.push(...data);

  console.log(state.productData);
};

export { createProduct, getProducts };
