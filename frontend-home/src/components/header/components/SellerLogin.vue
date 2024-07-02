<template>
  <div class="login">
    <Toast />
    <div v-if="!getCurrentSeller" v-focustrap class="login-wrap">
      <div class="avatar">
        <div>
          <i class="pi pi-user" />
        </div>
      </div>
      <div class="field">
        <InputText
          id="email"
          style="width: 300px"
          v-model="email"
          type="email"
          placeholder="Email"
        />
      </div>

      <div class="field">
        <InputText
          id="password"
          style="width: 300px"
          v-model="password"
          type="password"
          placeholder="Password"
          autofocus
        />
      </div>

      <Button type="submit" label="Login" class="button" @click="handleLogin" />
    </div>

    <div v-if="getCurrentSeller" class="profile">
      <div class="profile-description">
        <div class="profile-image">
          <img
            src="https://api.dicebear.com/9.x/thumbs/svg?seed=asdasdasdas"
            alt=""
          />
        </div>
        <div class="profile-name">
          <span> {{ getCurrentSeller.username }}</span>
          <span> ID: {{ getCurrentSeller.id }}</span>
        </div>
      </div>

      <div class="profile-item">
        <span>Email</span>
        <span>{{ getCurrentSeller.email }}</span>
      </div>
      <div class="profile-item">
        <span>Country</span>
        <span>{{ getCurrentSeller.country }}</span>
      </div>


    </div>
  </div>
</template>

<script>
import { ref } from "vue";
import { headerAPI } from "@/components/header/composable/header-api";
import { useToast } from "primevue/usetoast";

export default {
  setup() {
    const email = ref();
    const password = ref();

    const { loginSeller, getCurrentSeller } = headerAPI();

    const toast = useToast();

    const handleLogin = async () => {
      const params = {
        email: email.value,
        password: password.value,
      };

      await loginSeller(params)
        .then(() => {
          toast.add({
            severity: "info",
            summary: "Info",
            detail: "Successfully Logged In",
            life: 3000,
          });
        })
        .catch((err) => {
          toast.add({
            severity: "error",
            summary: "Error Message",
            detail: err.response.errors[0].message,
            life: 3000,
          });
        });
    };

    return {
      handleLogin,
      email,
      password,
      getCurrentSeller,
    };
  },
};
</script>

<style lang="css" scoped>
.avatar {
  display: flex;
  justify-content: center;
  margin-bottom: 1rem;
}

.avatar div {
  background: var(--base-b);
  display: flex;
  justify-content: center;
  align-items: center;
  width: 80px;
  height: 80px;
  border-radius: 50%;
}

.avatar div i {
  font-size: var(--text-size-h);
}

.login {
  height: 500px;
  display: flex;
  align-items: center;
  justify-content: center;
}

.login-wrap {
  display: flex;
  flex-direction: column;
  justify-content: center;
  border-radius: 12px;
}

.field {
  margin-top: 1rem;
}

.button {
  margin-top: 1rem;
}

.profile {
  width: 100%;
  height: 100%;
  display: block;
}

.profile-description {
  padding: 1rem;
  border-radius: 6px;
  display: flex;
  align-items: center;
}

.profile-image {
  width: 50px;
  height: 50px;
  background: green;
  border-radius: 50%;
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
  padding: 1rem;
  font-size: var(--text-size-b);
}

.profile-item span {
  line-height: 1.5rem;
}

.profile-item span:nth-child(1) {
  font-weight: 500;
}
</style>
