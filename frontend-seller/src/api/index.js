const env = "dev";

const baseURL = {
  dev: "https://localhost:443",
  prod: "https://pairfy.io",
};

const HOST = baseURL[env];

export { env, baseURL, HOST };
