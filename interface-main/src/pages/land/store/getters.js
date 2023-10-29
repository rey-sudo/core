const getter__productData = (state) => {
  return state.productData;
};

const getter__orderData = (state) => {
  return state.orderData;
};

const getter__viewPaymentModal = (state) => {
  return state.viewPaymentModal;
};

const getter__viewPaymentModalMobile = (state) => {
  return state.viewPaymentModalMobile;
};

export { getter__productData, getter__viewPaymentModal, getter__orderData, getter__viewPaymentModalMobile };
