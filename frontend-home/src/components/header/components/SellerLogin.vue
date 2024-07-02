<template>
  <div class="login">
    <div v-focustrap class="login-wrap">
      <div class="avatar">
        <div>
          <i class="pi pi-user" />
        </div>
      </div>
      <div class="field">
        <InputText
          id="email"
          v-model="email"
          type="email"
          placeholder="Email"
        />
      </div>

      <div class="field">
        <InputText
          id="password"
          v-model="name"
          type="password"
          placeholder="Password"
          autofocus
        />
      </div>

      <Button type="submit" label="Login" class="button" />
    </div>
  </div>
</template>

<script>
import { ref } from "vue";

export default {
  setup() {
    const name = ref();
    const email = ref();
    const accept = ref(false);

    return { name, email, accept };
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
