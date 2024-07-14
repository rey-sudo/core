<template>
  <div class="login">
    <div class="login-wrap" v-if="!getCurrentUser">
      <div class="button" @click="handleSign">
        <i class="pi pi-wallet" />
        <span>Login</span>
      </div>
    </div>

    <div class="profile" v-if="getCurrentUser">
      <div class="profile-item">
        <span>Id</span>
        <span>{{ getCurrentUser.id }}</span>
      </div>

      <div class="profile-item">
        <span>Country</span>
        <span>{{ getCurrentUser.country }}</span>
      </div>

      <div class="profile-item">
        <span>Address</span>
        <span>{{ getCurrentUser.address }}</span>
      </div>

      <div class="profile-item">
        <span>PKH</span>
        <span>{{ getCurrentUser.pubkeyhash }}</span>
      </div>

      <div class="profile-buttons">
        <button class="logout-button" @click="logoutUser">
          <i class="pi pi-sign-out" />
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import { signMessage, getAddress } from "@/api/wallet-api";

import headerAPI from "../composable/header-api";

export default {
  setup() {
    const { loginUser, getCurrentUser } = headerAPI();

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
      getCurrentUser,
    };
  },
};
</script>

<style lang="css" scoped>
.login {
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

.profile {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
}

.profile-description {
  border-radius: 6px;
  display: flex;
  align-items: center;
  margin-top: 1rem;
}

.profile-image {
  width: 50px;
  height: 50px;
  background: green;
  border-radius: 6px;
  overflow: hidden;
  border: 1px solid var(--border-b);
}

.profile-name {
  margin-left: 1rem;
  font-size: var(--text-size-c);
  color: var(--text-a);
  display: flex;
  flex-direction: column;
}

.profile-name span {
  line-height: 1.5rem;
}

.profile-name span:nth-child(2) {
  color: var(--text-b);
  font-size: var(--text-size-a);
}

.profile-item {
  display: flex;
  flex-direction: column;
  margin-top: 1.5rem;
}

.profile-item span {
  line-height: 1.5rem;
}

.profile-item span:nth-child(1) {
  font-weight: 500;
  color: var(--text-a);
  font-size: var(--text-size-b);
}

.profile-item span:nth-child(2) {
  font-weight: 400;
  font-size: var(--text-size-b);
}

.profile-buttons {
  margin-top: auto;
  justify-content: flex-end;
  display: flex;
}

.logout-button {
  padding: 0.5rem;
  border: 1px solid var(--red-a);
  background: transparent;
  border-radius: 4px;
  color: var(--red-a);
  display: flex;
  justify-content: center;
  align-items: center;
  cursor: pointer;
}
</style>
