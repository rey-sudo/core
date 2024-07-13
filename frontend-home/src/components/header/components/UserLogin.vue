<template>
  <div class="user">
    <div class="button" @click="handleSign">Wallet Login</div>
  </div>
</template>

<script>
import {
  signMessage,
  getMessage,
  getAddress,
  loginUser,
} from "@/api/wallet-api";

export default {
  setup() {
    const handleSign = async () => {
      const signature = await signMessage().catch((err) => console.error(err));

      const message = getMessage();

      const address = await getAddress();

      const params = {
        address,
        message,
        signature,
      };

      await loginUser(params).then((res) => console.log(res));
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
  display: flex;
  padding: 0.75rem 1rem;
  width: 100%;
  font-weight: 500;
  justify-content: center;
  border-radius: 6px;
  cursor: pointer;
}
</style>
