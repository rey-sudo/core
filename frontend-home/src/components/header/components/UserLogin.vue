<template>
  <div class="user">
    <div class="button" @click="handleSign">
      <i class="pi pi-wallet" />
      <span>Login</span>
    </div>
  </div>
</template>

<script>
import { signMessage, getAddress } from "@/api/wallet-api";

import headerAPI from "../composable/header-api";

export default {
  setup() {
    const { loginUser } = headerAPI();

    const handleSign = async () => {
      await signMessage()
        .then(async (signature) => [signature, await getAddress()])
        .then(([signature, address]) =>
          loginUser({
            signature,
            address,
            terms_accepted: true,
          })
        )
        .catch((err) => console.error(err));
    };

    return {
      handleSign,
    };
  },
};
</script>

<style lang="css" scoped>
.user {
  height: 400px;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 1rem;
}

.button {
  background: var(--primary-a);
  color: var(--text-w);
  font-size: var(--text-size-b);
  display: flex;
  padding: 0.75rem 1rem;
  width: 100%;
  font-weight: 500;
  justify-content: center;
  border-radius: 6px;
  cursor: pointer;
}

.button span {
  margin-left: 1rem;
}
</style>
