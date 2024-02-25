const createProduct = (state, data) => {
  state.productsData.push(data);
};

const getProducts = (state, data) => {
  state.productsData.length = 0;
  state.productsData.push(...data);
};

const getSlots = (state, data) => {
  state.slotsData.length = 0;
  state.slotsData.push(...data);
};



export { createProduct, getProducts, getSlots };
