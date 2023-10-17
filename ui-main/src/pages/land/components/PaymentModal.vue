<template>
  <div class="dialog" id="dialog">
    <div class="dialog-box" ref="outsideClick">
      <div class="header">Formulario de pago</div>

      <div class="body">
        <div class="summary">*Pagas cuando el producto llegue a tu casa*</div>

        <form>
          <div class="form-group">
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

          <div class="form-group">
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

          <div class="form-group">
            <label for="phone">
              <i class="pi pi-phone" />
              <span>Telefono (Whatsapp, Telegram)</span>
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

          <div class="form-group">
            <label for="address">
              <i class="pi pi-home" />
              <span>Direccion (Conjunto, piso, apartamento)</span>
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

          <div class="form-group">
            <label for="department">
              <i class="pi pi-map-marker" />
              <span>Departamento</span>
            </label>
            <select
              v-model="orderForm.department"
              id="department"
              name="department"
              required
              class="styled-dropdown"
            >
              <option value="" disabled selected>Selecciona</option>
              <option v-for="item in departaments" :key="item" :value="item">
                {{ item }}
              </option>
            </select>
          </div>

          <div class="form-group">
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

        <div class="bottom">
          <button @click="handleOrder">Listo</button>
        </div>
      </div>
    </div>
  </div>
</template>

<script>
import { ref } from "vue";
import { onClickOutside } from "@vueuse/core";
import landAPI from "@/pages/land/composable/land-api";

export default {
  setup() {
    const {
      getter__viewPaymentModal,
      action__viewPaymentModal,
      getter__productData,
      action__createOrder,
    } = landAPI();

    const outsideClick = ref(null);

    onClickOutside(outsideClick, () => action__viewPaymentModal(false));

    return {
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
        .then((res) => console.log(res))
        .catch((err) => console.error(err));
    },
  },
};
</script>

<style lang="css" scoped>
.bottom {
  display: flex;
  justify-content: flex-end;
  margin-top: 1rem;
}

i {
  margin-right: 0.5rem;
}

.styled-dropdown {
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

.styled-dropdown:hover {
  border: 1px solid var(--blue);
}

.styled-dropdown:focus {
  border: 1px solid var(--blue);
}

/* Estilos para las opciones dentro del dropdown */
.styled-dropdown option {
  background-color: #fff;
  color: #333;
}

/* Estilos para el contenedor del dropdown */
.dropdown {
  margin: 50px;
  text-align: center;
}

.payment-form {
}

.form-group {
  margin-top: 2rem;
  display: flex;
  flex-direction: column;
  width: 100%;
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

.header {
  padding: 1rem;
  font-weight: 600;
  color: var(--text-a);
  font-size: var(--text-size-b);
}

.summary {
  color: var(--text-b);
  font-size: var(--text-size-a);
}

.body {
  background: var(--base-a);
  border-radius: 8px;
  padding: 1rem;
  box-sizing: border-box;
}

.dialog {
  display: none;
  display: initial;
  position: fixed;
  z-index: 1;
  left: 0;
  top: 0rem;
  width: 100%;
  height: 100%;
  overflow: auto;
  background: rgba(0, 0, 0, 0.5);
}

.dialog-box {
  background: var(--base-c);
  backdrop-filter: blur(5px);
  margin: 5% auto;
  width: 100%;
  padding: 0.75rem;
  max-width: 400px;
  border-radius: 16px;
}
</style>
