<template>
  <Dialog
    v-model:visible="visible"
    modal
    dismissableMask
    blockScroll
    header="Buy Options"
    :draggable="false"
    :style="{ width: '70vw' }"
    :breakpoints="{ '1199px': '75vw', '575px': '90vw' }"
  >
    <DataTable
      ref="dt"
      :value="slotList"
      v-model:selection="selectedProducts"
      dataKey="id"
      style="height: 70vh"
      :paginator="true"
      :rows="10"
      :filters="filters"
      paginatorTemplate="FirstPageLink PrevPageLink PageLinks NextPageLink LastPageLink CurrentPageReport RowsPerPageDropdown"
      :rowsPerPageOptions="[5, 10, 25]"
      currentPageReportTemplate="Showing {first} to {last} of {totalRecords} products"
    >
      <template #header>
        <div class="toolbar">
          <InputText
            v-model="filters['global'].value"
            placeholder="Search..."
          />
        </div>
      </template>

      <Column
        field="id"
        header="Code"
        sortable
        style="min-width: 12rem"
      ></Column>

      <Column
        field="mode"
        header="Mode"
        sortable
        style="min-width: 10rem"
      ></Column>

      <Column
        field="contract_units"
        header="Units"
        sortable
        style="min-width: 10rem"
      ></Column>

      <Column
        field="contract_price"
        header="Price"
        sortable
        style="min-width: 10rem"
      ></Column>

      <Column
        field="contract_collateral"
        header="Collateral"
        sortable
        style="min-width: 10rem"
      ></Column>

      <Column
        field="product_discount"
        header="Discount"
        sortable
        style="min-width: 10rem"
      ></Column>

      <Column :exportable="false" style="min-width: 10rem">
        <template #body="slotProps">
          <button class="miniBuyButton" @click="editProduct(slotProps.data)">
            Buy
          </button>
        </template>
      </Column>
    </DataTable>

    <template #footer> </template>
  </Dialog>
  <!--//////////////////////////////////////////////////////////////////////////-->
  <div class="head-info">
    <span>5 available</span>
  </div>

  <div class="head-name">
    {{ getProductData?.name }}
  </div>

  <div class="head-legend">
    <span>Model: PLACCOG7OQVQY2BX </span>
    <span>SKU: P9C3KC93CK</span>
  </div>

  <div class="head-rating">
    <Rating
      :modelValue="product.rating_count"
      :stars="5"
      :readonly="true"
      :cancel="false"
      style="margin-right: 0.5rem"
    />
    <span>{{ product.rating_count }}</span>

    <span>({{ product.review_count }} Reviews)</span>
  </div>

  <div class="head-price">
    <div class="ada-label">₳</div>
    <span>3,024</span>
  </div>

  <InfoIcons />

  <div class="head-button buyButton" @click="openSlotDialog">
    <div>Buy now</div>
  </div>
</template>

<script>
import InfoIcons from "./InfoIcons.vue";
import { FilterMatchMode } from "primevue/api";
import { ref } from "vue";
import productAPI from "@/pages/product/api/index";

export default {
  components: {
    InfoIcons,
  },

  setup() {
    const { getProductData } = productAPI();

    const product = ref({
      rating_count: 4.8,
      review_count: 558,
    });

    const visible = ref(true);

    const openSlotDialog = () => {
      visible.value = true;
    };

    const filters = ref({
      global: { value: null, matchMode: FilterMatchMode.CONTAINS },
    });

    const slotList = ref([
      {
        id: "S80843894300",
        mode: "batch",
        contract_units: 2,
        contract_price: 3024,
        contract_collateral: 20,
        product_id: "adadada",
        product_discount: 0,
      },
      {
        id: "S80843894300",
        mode: "batch",
        contract_units: 2,
        contract_price: 3024,
        contract_collateral: 20,
        product_id: "adadada",
        product_discount: 0,
      },
      {
        id: "S80843894300",
        mode: "batch",
        contract_units: 2,
        contract_price: 3024,
        contract_collateral: 20,
        product_id: "adadada",
        product_discount: 0,
      },
      {
        id: "S80843894300",
        mode: "batch",
        contract_units: 2,
        contract_price: 3024,
        contract_collateral: 20,
        product_id: "adadada",
        product_discount: 0,
      },
    ]);

    const selectedProducts = ref();
    return {
      product,
      visible,
      openSlotDialog,
      filters,
      slotList,
      selectedProducts,
      getProductData,
    };
  },
};
</script>

<style lang="css" scoped>
.head-info {
  font-size: var(--text-size-a);
  font-weight: 400;
  text-align: left;
  color: var(--primary-c);
}

.head-name {
  font-size: var(--text-size-g);
  font-weight: 500;
  text-align: left;
  width: 100%;
  margin-top: 1rem;
  line-height: 2.5rem;
}

.head-legend {
  text-align: left;
  font-size: var(--text-size-a);
  margin-top: 1rem;
  font-weight: 400;
  color: var(--text-a);
}

.head-legend span {
  margin-right: 1rem;
}

.head-rating {
  font-size: var(--text-size-c);
  align-items: center;
  display: flex;
  padding-top: 1rem;
  padding-bottom: 1rem;
  width: 90%;
}

.head-rating span {
  margin-right: 0.5rem;
  font-size: var(--text-size-c);
  font-weight: 600;
}

.head-rating span:nth-child(3) {
  font-weight: 400;
  font-size: var(--text-size-a);
}

.head-price {
  text-align: left;
  margin-top: initial;
  font-size: var(--text-size-h);
  font-weight: 600;
  display: flex;
  align-items: baseline;
  margin-top: 1rem;
}

.head-price .ada-label {
  font-weight: 400;
  color: var(--text-a);
  font-size: var(--text-size-g);
  margin-right: 0.5rem;
  margin-bottom: 2px;
}

.head-button {
  border: 1px solid var(--primary-b);
  width: 90%;
  border-radius: 4px;
  color: var(--text-a);
  font-weight: 600;
  cursor: pointer;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 0.75rem;
  text-align: center;
  margin-top: 1rem;
  transition: var(--transition-a);
  background: var(--primary-b);
}

.head-button.buyButton {
  margin-top: 2rem;
}

.head-button:hover {
  opacity: 0.8;
}

.toolbar {
  display: flex;
  justify-content: flex-end;
}

.miniBuyButton {
  background: var(--primary-b);
  color: var(--text-a);
  font-weight: 500;
  border-radius: 6px;
  border: 1px solid var(--primary-b);
  cursor: pointer;
  padding: 0.25rem 1rem;
}
</style>
