import axiosAPI from "@/api/axios-api";

const getOrder = async ({ commit }, params) => {
  try {
    const response = await axiosAPI.get(`/api/gateway/get-order/${params.id}`);

    commit("getOrder", response.data.payload);

    return { success: true, response: response.data };
  } catch (error) {
    return { success: false, response: error.response.data };
  }
};


const startEndpoint = async (_, params) => {
  try {
    const response = await axiosAPI.post("/api/gateway/start-endpoint", params);

    console.log(response);
    //commit("startEndpoint", response.data.payload);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export { getOrder, startEndpoint};
