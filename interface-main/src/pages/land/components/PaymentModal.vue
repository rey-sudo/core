<template>
  <div class="p-dialog" id="dialog">
    <div class="p-dialog-wrap" ref="outsideClick">
      <div class="p-dialog-wrap-head">Formulario de pago</div>

      <div class="p-dialog-wrap-body">
        <div class="p-dialog-wrap-body-label">
          ¡Pagas cuando llegue el producto a tu casa!
        </div>

        <form>
          <div class="p-dialog-wrap-body-form">
            <label for="name">
              <i class="pi pi-user" />
              <span>Nombre</span>
            </label>
            <input
              v-model="orderForm.name"
              type="text"
              id="name"
              name="name"
              required
              minlength="1"
              maxlength="50"
              placeholder="Escribe tu nombre"
              autofocus
            />
          </div>

          <div class="p-dialog-wrap-body-form">
            <label for="lastname">
              <i class="pi pi-user" />
              <span> Apellido</span>
            </label>
            <input
              v-model="orderForm.last_name"
              type="text"
              id="lastname"
              name="lastname"
              required
              minlength="1"
              maxlength="50"
              placeholder="Escribe tu apellido"
            />
          </div>

          <div class="p-dialog-wrap-body-form">
            <label for="phone">
              <i class="pi pi-phone" />
              <span>Teléfono (Whatsapp, Telegram)</span>
            </label>
            <input
              v-model="orderForm.phone"
              type="text"
              id="phone"
              name="phone"
              required
              minlength="1"
              maxlength="20"
              placeholder="Numero de contacto"
            />
          </div>

          <div class="p-dialog-wrap-body-form">
            <label for="address">
              <i class="pi pi-home" />
              <span>Dirección (Conjunto, piso, apartamento)</span>
            </label>
            <input
              v-model="orderForm.address"
              type="text"
              id="address"
              name="address"
              required
              minlength="1"
              maxlength="100"
              placeholder="Ej. Calle 4A #43-54 Barrio cundinamarca"
            />
          </div>

          <div class="p-dialog-wrap-body-form">
            <label for="department">
              <i class="pi pi-map-marker" />
              <span>Departamento</span>
            </label>
            <select
              v-model="orderForm.department"
              id="department"
              name="department"
              required
              class="department-dropdown"
            >
              <option value="" disabled selected>Selecciona</option>
              <option v-for="item in departaments" :key="item" :value="item">
                {{ item }}
              </option>
            </select>
          </div>

          <div class="p-dialog-wrap-body-form">
            <label for="city">
              <i class="pi pi-map-marker" />
              <span>Ciudad</span>
            </label>
            <input
              v-model="orderForm.city"
              type="text"
              id="city"
              name="city"
              required
              minlength="1"
              maxlength="30"
              placeholder="Ej. Bogota"
            />
          </div>
        </form>

        <div class="p-dialog-wrap-body-button">
          <button @click="handleOrder">Listo</button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { ref } from "vue";
import { onClickOutside } from "@vueuse/core";
import { useRouter } from "vue-router";
import landAPI from "@/pages/land/composable/land-api";

export default {
  setup() {
    const {
      getter__viewPaymentModal,
      action__viewPaymentModal,
      getter__productData,
      action__createOrder,
    } = landAPI();

    const router = useRouter();

    const outsideClick = ref(null);

    onClickOutside(outsideClick, () => action__viewPaymentModal(false));

    return {
      router,
      outsideClick,
      getter__viewPaymentModal,
      action__viewPaymentModal,
      getter__productData,
      action__createOrder,
    };
  },

  data() {
    return {
      orderForm: {
        name: "",
        last_name: "",
        phone: "",
        address: "",
        department: "",
        city: "",
        product_pid: this.getter__productData.pid,
      },
      departaments: [
        "Amazonas",
        "Antioquia",
        "Arauca",
        "Atlántico",
        "Bolívar",
        "Boyacá",
        "Caldas",
        "Caquetá",
        "Casanare",
        "Cauca",
        "Cesar",
        "Chocó",
        "Córdoba",
        "Cundinamarca",
        "Guainía",
        "Guaviare",
        "Huila",
        "La Guajira",
        "Magdalena",
        "Meta",
        "Nariño",
        "Norte de Santander",
        "Putumayo",
        "Quindío",
        "Risaralda",
        "San Andrés y Providencia",
        "Santander",
        "Sucre",
        "Tolima",
        "Valle del Cauca",
        "Vaupés",
        "Vichada",
      ],
    };
  },
  mounted() {
    if (this.getter__viewPaymentModal) {
      const dialog = document.getElementById("dialog");

      document.body.appendChild(dialog);
    }
  },

  methods: {
    handleOrder() {
      this.action__createOrder(this.orderForm)
        .then((res) => {
          this.action__viewPaymentModal(false);
          this.router.push({
            name: "order",
            params: { pid: res.response.pid },
          });
        })
        .catch((err) => console.error(err));
    },
  },
};
</script>

<style lang="css" scoped>
i {
  margin-right: 0.5rem;
}

.department-dropdown {
  appearance: none;
  -webkit-appearance: none;
  -moz-appearance: none;
  background: var(--base-a);
  border: 1px solid var(--border-a);
  padding: 0.75rem 1rem;
  font-size: var(--text-size-a);
  outline: none;
  cursor: pointer;
  border-radius: 8px;
}

.department-dropdown:hover {
  border: 1px solid var(--blue);
}

.department-dropdown:focus {
  border: 1px solid var(--blue);
}

.department-dropdown option {
  background-color: #fff;
  color: #333;
}

label {
  font-weight: 600;
  color: var(--text-a);
  font-size: var(--text-size-a);
  display: flex;
  align-items: center;
  background: var(--base-d);
  padding: 0.5rem;
  border: 1px solid var(--border-a);
  border-radius: 8px;
  margin-bottom: 0.5rem;
}

input[type="text"] {
  padding: 0.75rem 1rem;
  font-size: var(--text-size-a);
  border: 1px solid var(--border-a);
  border-radius: 8px;
  outline: none;
}

button {
  background: var(--blue);
  color: white;
  border: none;
  border-radius: 999px;
  padding: 0.5rem 1rem;
  cursor: pointer;
  font-weight: 600;
  font-size: var(--text-size-a);
}

button:hover {
  background: black;
}

.p-dialog {
  display: none;
  display: initial;
  position: fixed;
  z-index: 1;
  left: 0;
  top: 0rem;
  width: 100%;
  height: 100%;
  overflow: hidden;
  background: rgba(0, 0, 0, 0.5);
}

.p-dialog .p-dialog-wrap {
  background: var(--base-c);
  backdrop-filter: blur(5px);
  margin: 5% auto;
  width: 100%;
  padding: 0.75rem;
  max-width: 400px;
  border-radius: 12px;
  overflow: hidden;
  overflow-y: auto;
}

.p-dialog .p-dialog-wrap .p-dialog-wrap-head {
  padding: 1rem;
  font-weight: 600;
  color: var(--text-a);
  font-size: var(--text-size-b);
}

.p-dialog .p-dialog-wrap .p-dialog-wrap-body {
  background: var(--base-a);
  border-radius: 12px;
  padding: 1rem;
  box-sizing: border-box;
}

.p-dialog .p-dialog-wrap .p-dialog-wrap-body .p-dialog-wrap-body-label {
  color: var(--text-b);
  font-size: var(--text-size-a);
}

.p-dialog .p-dialog-wrap .p-dialog-wrap-body .p-dialog-wrap-body-form {
  margin-top: 2rem;
  display: flex;
  flex-direction: column;
  width: 100%;
}

.p-dialog .p-dialog-wrap .p-dialog-wrap-body .p-dialog-wrap-body-button {
  display: flex;
  justify-content: flex-end;
  margin-top: 1rem;
}
</style>
