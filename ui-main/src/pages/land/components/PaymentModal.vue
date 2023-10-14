<template>
  <div  class="dialog" id="dialog">
    <div class="dialog-box" ref="outsideClick">
      <div class="header">Formulario de pago</div>

      <div class="body">
        <div class="summary">*Pagas cuando el producto llegue a tu casa*</div>

        <form>
          <div class="form-group">
            <label for="name">
              <i class="pi pi-user" />
              Nombre</label
            >
            <input type="text" id="name" name="name" required />
          </div>

          <div class="form-group">
            <label for="lastname">
              <i class="pi pi-user" />
              Apellido</label
            >
            <input type="text" id="lastname" name="lastname" required />
          </div>

          <div class="form-group">
            <label for="card-number">
              <i class="pi pi-phone" />
              Telefono (Whatsapp, Telegram)</label
            >
            <input type="text" id="card-number" name="card-number" required />
          </div>

          <div class="form-group">
            <label for="lastname">
              <i class="pi pi-home" />
              Direccion completa ( Conjunto, piso, apartamento )</label
            >
            <input type="text" id="lastname" name="lastname" required />
          </div>

          <div class="form-group">
            <label for="department">
              <i class="pi pi-map-marker" />
              Departamento</label
            >
            <select
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
            <label for="ciudad">
              <i class="pi pi-map-marker" />
              Ciudad</label
            >
            <input type="text" id="ciudad" name="ciudad" required />
          </div>

          <div class="bottom">
            <button type="submit">Listo</button>
          </div>
        </form>
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
    const { getter__viewPaymentModal, action__viewPaymentModal } =
      landAPI();

    const outsideClick = ref(null);

    onClickOutside(outsideClick, () => action__viewPaymentModal(false));

    return {
      outsideClick,
      getter__viewPaymentModal,
      action__viewPaymentModal,
    };
  },

  data() {
    return {
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
};
</script>

<style lang="css" scoped>
.bottom {
  display: flex;
  justify-content: flex-end;
}

i {
  margin-right: 0.25rem;
}

.styled-dropdown {
  appearance: none;
  -webkit-appearance: none;
  -moz-appearance: none;
  background: var(--base-a);
  border: 1px solid var(--border-a);
  padding: 0.75rem 0.5rem;
  font-size: var(--text-size-a);
  width: 250px;
  outline: none;
  cursor: pointer;
  border-radius: 6px;
  margin-top: 0.5rem;
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
  margin-bottom: 15px;
}

label {
  display: block;
  font-weight: 600;
  margin-bottom: 5px;
  margin-top: 1rem;
  color: var(--text-a);
  font-size: var(--text-size-a);
}

input[type="text"] {
  width: 350px;
  padding: 0.75rem 0.5rem;
  border: 1px solid transparent;
  font-size: var(--text-size-a);
  border: 1px solid var(--border-a);
  border-radius: 6px;
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
  margin-bottom: 2rem;
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
  padding: 0 0.75rem;
  padding-bottom: 0.75rem;
  max-width: 400px;
  border-radius: 16px;
}
</style>
