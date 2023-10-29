import axiosApi from "@/api/axios-api";

const action__viewPaymentModal = async ({ commit }, params) => {
  commit("commit__viewPaymentModal", params);
};

const action__viewPaymentModalMobile = async ({ commit }, params) => {
  commit("commit__viewPaymentModalMobile", params);
};

const action__getProductData = async ({ commit }, params) => {
  try {
    const response = await axiosApi.get("/api/store/get-product", {
      params: params,
    });

    commit("commit__getProductData", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const action__createOrder = async ({ commit }, params) => {
  try {
    const response = await axiosApi.post("/api/store/create-order", params);

    commit("commit__createOrder", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export {
  action__viewPaymentModal,
  action__getProductData,
  action__createOrder,
  action__viewPaymentModalMobile,
};
