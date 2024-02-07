<template>
  <div class="card">
    <div class="title">Sign Up</div>

    <div class="field">
      <InputGroup>
        <InputGroupAddon>
          <i class="pi pi-envelope" />
        </InputGroupAddon>
        <InputText placeholder="Email" />
      </InputGroup>
    </div>
    <div class="field">
      <InputGroup>
        <InputGroupAddon>
          <i class="pi pi-user" />
        </InputGroupAddon>
        <InputText placeholder="Username" />
      </InputGroup>
    </div>
    <div class="field">
      <InputGroup>
        <InputGroupAddon>
          <i class="pi pi-lock" />
        </InputGroupAddon>
        <InputText placeholder="Password" />
      </InputGroup>
    </div>

    <Button type="submit" label="Register" class="button" />

    <div class="legend">
      <span>I already have an account...</span>
      <span>Or reset password.</span>
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

    return {
      name,
      email,
      accept,
    };
  },
};
</script>

<style lang="css" scoped>
.card {
  background: var(--base-a);
  padding: 2rem 1.5rem;
  border-radius: 18px;
  display: block;
}

.field {
  margin: 1rem 0;
  width: 300px;
}

.button {
  width: 100%;
  margin-top: 1rem;
}

.title {
  font-weight: 700;
  font-size: var(--text-size-g);
  text-align: left;
  margin-bottom: 2rem;
}

.legend {
  text-align: left;
  font-size: var(--text-size-a);
  cursor: pointer;
  color: var(--blue-a);
  margin-top: 1rem;
  font-weight: 600;
  display: flex;
  flex-direction: column;
}

.legend span {
  margin-top: 0.5rem;
}
</style>
