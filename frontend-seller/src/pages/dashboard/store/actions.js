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

const startEndpoint = async (_, params) => {
  try {
    const response = await axiosAPI.post("/api/gate/start-endpoint", params);

    console.log(response);
    //commit("startEndpoint", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const setupLucid = async ({ commit }, data) => {
  commit("setupLucid", data);
};

const createSlot = async (_, params) => {
  try {
    const response = await axiosAPI.post("/api/gate/create-slot", params);

    //commit("createSlot", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const getProducts = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.get("/api/product/get-products", params);

    commit("getProducts", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const getSlots = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.get("/api/gate/get-slots", params);

    commit("getSlots", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export { startEndpoint, setupLucid, createProduct, getProducts, createSlot, getSlots };
