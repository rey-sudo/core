import axiosAPI from "@/api/axios-api";

const createProduct = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.post("/api/product/create-product", params);

    console.log(response);

    commit("createProduct", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const getProducts = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.post("/api/product/get-products", params);

    console.log(response);

    commit("getProducts", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export { createProduct, getProducts };
