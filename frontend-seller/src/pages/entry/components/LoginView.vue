<template>
  <div class="card">
    <div class="field">
      <div class="title">Be a seller</div>
    </div>
    <div class="field">
      <InputGroup>
        <InputGroupAddon>
          <i class="pi pi-user" />
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
    <div class="field">
      <Button type="submit" label="Register" class="button" />
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
  padding: 2rem;
  border-radius: 18px;
  display: block;
}

.field {
  margin: 1rem 0;
  width: 300px;
}

.button {
  width: 100%;
}

.title {
  font-weight: 700;
  font-size: var(--text-size-g);
}
</style>
