import axiosApi from "@/api/axios-api";

const lockingEndpoint = async () => {
  try {
    const response = await axiosApi.get("/api/gate/locking-endpoint");

    console.log(response.data);

    //commit("lockingEndpoint", response.data);

    return { ok: true, response: response.data };
  } catch (error) {
    throw { ok: false, response: error.response.data };
  }
};

export { lockingEndpoint };
