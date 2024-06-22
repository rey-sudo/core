import axiosAPI from "@/api/axios-api";

const currentSeller = async ({ commit }) => {
  try {
    const response = await axiosAPI.get("/api/seller/current-seller");

    console.log(response.data);

    commit("currentSeller", response.data.sellerData);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { success: false, response: error.response.data };
  }
};

const loginSeller = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.post("/api/seller/login-seller", params);

    console.log(response);

    commit("userData", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const setupWallet = async ({ commit }, params) => {
  commit("setupWallet", params);
};
export { currentSeller, loginSeller, setupWallet };
