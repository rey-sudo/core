const getter__productData = (state) => {
  return state.productData;
};

const getter__orderData = (state) => {
  return state.orderData;
};

const getter__viewPaymentModal = (state) => {
  return state.viewPaymentModal;
};

export { getter__productData, getter__viewPaymentModal, getter__orderData };
