<template>
  <div class="login">
    <Toast />
    <div v-focustrap class="login-wrap">
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

    const { loginSeller } = headerAPI();

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
            detail: "Message Content",
            life: 3000,
          });
        })
        .catch((err) => {
          console.log(err);
          toast.add({
            severity: "error",
            summary: "Error Message",
            detail: "Login Error",
            life: 3000,
          });
        });
    };

    return {
      handleLogin,
      email,
      password,
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
</style>
