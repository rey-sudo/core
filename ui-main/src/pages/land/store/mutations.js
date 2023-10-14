const commit__viewPaymentModal = (state, data) => {
  state.viewPaymentModal = data;
};


const commit__getProductData = (state, data) => {
  state.productData = data;
};


export { commit__viewPaymentModal, commit__getProductData };
