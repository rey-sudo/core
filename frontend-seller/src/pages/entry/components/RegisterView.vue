<template>
  <div class="card">
    <div class="title">Sign Up</div>

    <div class="field">
      <InputGroup>
        <InputGroupAddon>
          <i class="pi pi-envelope" />
        </InputGroupAddon>
        <InputText placeholder="Email" v-model="email" />
      </InputGroup>
    </div>
    <div class="field">
      <InputGroup>
        <InputGroupAddon>
          <i class="pi pi-user" />
        </InputGroupAddon>
        <InputText v-model="username" placeholder="Username" />
      </InputGroup>
    </div>
    <div class="field">
      <InputGroup>
        <InputGroupAddon>
          <i class="pi pi-lock" />
        </InputGroupAddon>
        <Password v-model="password" placeholder="Password" toggleMask>
          <template #header>
            <p class="mt-2">Suggestions</p>
          </template>
          <template #footer>
            <Divider />
            <p class="mt-2">Suggestions</p>
            <ul class="pl-2 ml-2 mt-0" style="line-height: 1.5">
              <li>At least one lowercase</li>
              <li>At least one uppercase</li>
              <li>At least one numeric</li>
              <li>Minimum 8 characters</li>
            </ul>
          </template>
        </Password>
      </InputGroup>
    </div>

    <div class="field">
      <Dropdown
        v-model="selectedCountry"
        :options="countries"
        optionLabel="name"
        placeholder="Operations area"
        class="w-full md:w-14rem"
      >
        <template #value="slotProps">
          <div v-if="slotProps.value" class="dropdown-item">
            <img
              :alt="slotProps.value.label"
              src="https://primefaces.org/cdn/primevue/images/flag/flag_placeholder.png"
              :class="`mr-2 flag flag-${slotProps.value.code.toLowerCase()}`"
            />
            <div>{{ slotProps.value.name }}</div>
          </div>
          <span v-else>
            {{ slotProps.placeholder }}
          </span>
        </template>
        <template #option="slotProps">
          <div class="dropdown-item">
            <img
              :alt="slotProps.option.label"
              src="https://primefaces.org/cdn/primevue/images/flag/flag_placeholder.png"
              :class="`mr-2 flag flag-${slotProps.option.code.toLowerCase()}`"
            />
            <div>{{ slotProps.option.name }}</div>
          </div>
        </template>
      </Dropdown>
    </div>

    <Button
      type="submit"
      label="Register"
      class="button"
      @click="handleSubmit"
    />

    <div class="legend">
      <span>I already have an account</span>
      <span>Reset password</span>
    </div>
  </div>
</template>

<script>
import { ref } from "vue";
import entryAPI from "@/pages/entry/api/index";

export default {
  setup() {
    const username = ref(null);
    const email = ref(null);
    const password = ref(null);

    const { createUser } = entryAPI();

    const selectedCountry = ref();

    const countries = ref([
      { name: "United States", code: "US", number: '840' },
      { name: "Chile", code: "CL", number: '152' },
      { name: "Argentina", code: "AR", number: '032' },
      { name: "Venezuela", code: "VE", number: '862' },
      { name: "Colombia", code: "CO", number: '170' },
    ]);

    return {
      username,
      email,
      password,
      createUser,
      selectedCountry,
      countries,
    };
  },
  methods: {
    async handleSubmit() {
      const params = {
        username: this.username,
        email: this.email,
        password: this.password,
        country: this.selectedCountry.number,
      };

      console.log(params);

      const result = await this.createUser(params);

      console.log(result);
    },
  },
};
</script>
<style src="../assets/flags.css" />

<style lang="css" scoped>
.card {
  background: var(--base-a);
  padding: 1.5rem;
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
  font-size: var(--text-size-b);
  cursor: pointer;
  color: var(--blue-a);
  margin-top: 1rem;
  font-weight: 500;
  display: flex;
  flex-direction: column;
}

.legend span {
  margin-top: 0.5rem;
}
p {
  font-size: var(--text-size-a);
}

ul {
  font-size: var(--text-size-a);
}

.dropdown-item {
  display: flex;
  justify-content: space-between;
}

.dropdown-item div {
  margin-left: 1rem;
}
</style>
