
const commit__getAllProducts = (state, data) => {
  state.allProducts = data;
};

const getProduct = (state, data) => {
  state.productData = data;
};


export {
  commit__getAllProducts,
  getProduct
};
