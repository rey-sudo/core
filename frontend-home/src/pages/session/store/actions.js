import axiosAPI from "@/api/axios-api";

const getSlot = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.get(`/api/gate/get-slot/${params.id}`);

    commit("getSlot", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { success: false, response: error.response.data };
  }
};

export { getSlot };
