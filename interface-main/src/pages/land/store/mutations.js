const commit__viewPaymentModal = (state, data) => {
  state.viewPaymentModal = data;
};

const commit__getProductData = (state, data) => {
  state.productData = data;
};

const commit__createOrder = (state, data) => {
  state.orderData = data;
};


export {
  commit__viewPaymentModal,
  commit__getProductData,
  commit__createOrder,
};
