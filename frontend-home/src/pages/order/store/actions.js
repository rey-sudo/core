import axiosApi from "@/api/axios-api";

const action__getOrderData = async ({ commit }, params) => {
  try {
    const response = await axiosApi.get("/api/store/get-order", {
      params: params,
    });

    commit("commit__getOrderData", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export { action__getOrderData };
