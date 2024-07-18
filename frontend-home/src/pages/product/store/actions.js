import axiosApi from "@/api/axios-api";

const lockingEndpoint = async (_, params) => {
  try {
    console.log(params);
    const response = await axiosApi.post("/api/gate/locking-endpoint", params);

    console.log(response.data);

    //commit("lockingEndpoint", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

const getProduct = async (_, params) => {
  try {
    const query = `
    query Product($id: String) {
      product(id: $id) {
        id
        name
      }
    }
  `;

    const variables = {
      id: params.id,
    };

    const response = await axiosApi.post("/api/query", {
      query,
      variables,
    });

    console.log(response.data);

    //commit("lockingEndpoint", response.data);

    return { success: true, response: response.data };
  } catch (error) {
    return { success: false, response: error.response.data };
  }
};

export { lockingEndpoint, getProduct };
