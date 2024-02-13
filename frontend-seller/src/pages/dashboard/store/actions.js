import axiosAPI from "@/api/axios-api";

const createProduct = async (_, params) => {
  try {
    const response = await axiosAPI.post("/api/product/create-product", params);

    //commit("createProduct", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const getProducts = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.post("/api/product/get-products", params);

    commit("getProducts", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export { createProduct, getProducts };
