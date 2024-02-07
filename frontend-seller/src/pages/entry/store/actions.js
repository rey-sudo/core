import axiosAPI from "@/api/axios-api";

const createUser = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.post("/api/seller/create-seller", params);

    console.log(response);

    commit("createUser", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export { createUser };
