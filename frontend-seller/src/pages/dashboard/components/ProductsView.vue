<template>
  <div class="products">
    <div class="products-wrap">
      <div class="products-card">
        <Dialog
          v-model:visible="messageModalVisible"
          modal
          header="Message"
          :draggable="false"
          :style="{ width: '35rem' }"
          :breakpoints="{ '1199px': '75vw', '575px': '90vw' }"
        >
          <p>{{ messageModal }}</p>

          <p v-for="e in errorModal" :key="e">{{ e }}</p>

          <template #footer>
            <div class="modal-footer">
              <Button type="button" label="Ok" @click="hideModals" />
            </div>
          </template>
        </Dialog>

        <!----->
        <DataTable
          ref="dt"
          :value="products"
          v-model:selection="selectedProducts"
          dataKey="id"
          :paginator="true"
          :rows="10"
          :filters="filters"
          paginatorTemplate="FirstPageLink PrevPageLink PageLinks NextPageLink LastPageLink CurrentPageReport RowsPerPageDropdown"
          :rowsPerPageOptions="[5, 10, 25]"
          currentPageReportTemplate="{first} to {last} of {totalRecords} items"
        >
          <template #header>
            <div class="products-header">
              <div class="products-header-left">
                <span class="products-header-title">Products</span>
                <span class="products-header-subtitle"
                  >Create and modify product and more...</span
                >
              </div>

              <span class="p-input-icon-left">
                <i class="pi pi-search" />
                <InputText
                  v-model="filters['global'].value"
                  placeholder="Search..."
                />
              </span>
            </div>

            <Toolbar class="mb-4">
              <template #start>
                <Button label="New" icon="pi pi-plus" @click="openNew" />
                <Button
                  label="Delete"
                  icon="pi pi-trash"
                  style="margin: 0 1rem"
                  @click="confirmDeleteSelected"
                  :disabled="!selectedProducts || !selectedProducts.length"
                />
              </template>

              <template #end>
                <FileUpload
                  mode="basic"
                  accept="image/*"
                  :maxFileSize="1000000"
                  label="Import"
                  chooseLabel="Import"
                  style="margin: 0 1rem"
                />
                <Button
                  label="Export"
                  icon="pi pi-upload"
                  @click="exportCSV($event)"
                />
              </template>
            </Toolbar>
          </template>

          <Column
            selectionMode="multiple"
            style="width: 3rem"
            :exportable="false"
          />
          <Column
            field="code"
            header="Code"
            sortable
            style="min-width: 12rem"
          ></Column>
          <Column
            field="name"
            header="Name"
            sortable
            style="min-width: 16rem"
          ></Column>
          <Column header="Image">
            <template #body="slotProps">
              <img
                :src="`https://primefaces.org/cdn/primevue/images/product/${slotProps.data.image}`"
                :alt="slotProps.data.image"
                class="border-round"
                style="width: 64px"
              />
            </template>
          </Column>
          <Column field="price" header="Price" sortable style="min-width: 8rem">
            <template #body="slotProps">
              {{ formatCurrency(slotProps.data.price) }}
            </template>
          </Column>
          <Column
            field="category"
            header="Category"
            sortable
            style="min-width: 10rem"
          ></Column>
          <Column
            field="rating"
            header="Reviews"
            sortable
            style="min-width: 12rem"
          >
            <template #body="slotProps">
              <Rating
                :modelValue="slotProps.data.rating"
                :readonly="true"
                :cancel="false"
              />
            </template>
          </Column>
          <Column
            field="inventoryStatus"
            header="Status"
            sortable
            style="min-width: 12rem"
          >
            <template #body="slotProps">
              <Tag
                :value="slotProps.data.inventoryStatus"
                :severity="getStatusLabel(slotProps.data.inventoryStatus)"
              />
            </template>
          </Column>
          <Column :exportable="false" style="min-width: 8rem">
            <template #body="slotProps">
              <Button
                class="table-button"
                icon="pi pi-pencil"
                outlined
                rounded
                @click="editProduct(slotProps.data)"
              />
              <Button
                class="table-button"
                icon="pi pi-trash"
                outlined
                rounded
                @click="confirmDeleteProduct(slotProps.data)"
              />
            </template>
          </Column>
        </DataTable>
      </div>

      <Dialog
        v-model:visible="productDialog"
        :style="{ width: '500px' }"
        header="Create"
        :modal="true"
        :draggable="false"
        class="p-fluid"
      >
        <img
          v-if="product.image"
          :src="`https://primefaces.org/cdn/primevue/images/product/${product.image}`"
          :alt="product.image"
          class="block m-auto pb-3"
        />
        <div class="field">
          <label for="name" class="field-label">Name</label>
          <InputText
            id="name"
            v-model="productName"
            required="true"
            autofocus
            :class="{ invalid: invalidProductName }"
          />
          <small class="p-error" v-if="invalidProductName"
            >The name is required and max {{ nameLengthLimit }} characters
            long.</small
          >
        </div>
        <div class="field">
          <label for="description" class="field-label">
            <span>Description</span>
            <i
              class="pi pi-info-circle"
              v-tooltip.top="'Write a clear description about the product.'"
            />
          </label>
          <Textarea
            id="description"
            v-model="productDescription"
            required="true"
            rows="3"
            cols="20"
            autoResize
            :class="{ invalid: invalidProductDescription }"
          />
          <small
            class="p-counter"
            :class="{
              invalid: productDescription.length > descriptionLengthLimit,
            }"
            v-if="!invalidProductDescription"
          >
            {{ productDescription.length }} / {{ descriptionLengthLimit }}
          </small>
          <small class="p-error" v-if="invalidProductDescription"
            >The description is required and
            {{ descriptionLengthLimit }} characters long.
          </small>
        </div>

        <div class="formgrid grid">
          <div class="field col">
            <label for="category" class="field-label">
              <span>Category</span>
              <i
                class="pi pi-info-circle"
                v-tooltip.top="
                  'Select the category corresponding to the product.'
                "
              />
            </label>
            <Dropdown
              v-model="productCategory"
              :options="categories"
              id="category"
              optionLabel="name"
              placeholder=""
              checkmark
              :class="{ invalid: invalidProductCategory }"
              :highlightOnSelect="false"
            />
            <small class="p-error" v-if="invalidProductCategory"
              >The category is required.
            </small>
          </div>

          <div class="field col">
            <label for="price" class="field-label">
              <span>Price</span>
              <i
                class="pi pi-info-circle"
                v-tooltip.top="'Product price in ADA.'"
              />
            </label>
            <InputNumber
              id="price"
              v-model="productPrice"
              showButtons
              prefix="₳ "
              locale="en-US"
              :min="1"
              :class="{ invalid: invalidProductPrice }"
            />
            <small class="p-error" v-if="invalidProductPrice"
              >The price is required.</small
            >
          </div>

          <div class="field col">
            <label for="collateral" class="field-label">
              <span>Collateral</span>
              <i
                class="pi pi-info-circle"
                v-tooltip.top="'Assign an ADA amount as a guarantee.'"
              />
            </label>
            <InputNumber
              id="collateral"
              v-model="productCollateral"
              showButtons
              prefix="₳ "
              locale="en-US"
              :min="1"
              :class="{ invalid: invalidProductCollateral }"
            />
            <small class="p-error" v-if="invalidProductCollateral"
              >The collateral is required.
            </small>
          </div>
          <div class="field col">
            <label for="quantity" class="field-label">
              <span>Stock</span>
              <i
                class="pi pi-info-circle"
                v-tooltip.top="'Current number of units of the product.'"
              />
            </label>
            <InputNumber
              id="quantity"
              v-model="productStock"
              integeronly
              :min="0"
              :class="{ invalid: invalidProductStock }"
            />

            <small class="p-error" v-if="invalidProductStock"
              >The stock is required.
            </small>
          </div>
          <div class="field col">
            <label for="keywords" class="field-label">Keywords</label>
            <Chips
              id="keywords"
              v-model="productKeywords"
              :allowDuplicate="false"
              separator=","
              :max="3"
              placeholder="Separate with , or  ↵"
              :class="{ invalid: invalidProductKeywords }"
            />
            <small class="p-error" v-if="invalidProductKeywords"
              >3 keywords are required.
            </small>
          </div>
        </div>

        <div class="field">
          <div class="product-upload">
            <label for="fileupload" class="field-label">Images</label>
            <Toast />
            <FileUpload
              id="fileupload"
              name="image"
              :url="mediaUrl"
              @upload="onAdvancedUpload($event)"
              :multiple="true"
              accept="image/*"
              :fileLimit="5"
              :maxFileSize="1000000"
            >
              <template #empty>
                <p :class="{ invalid: invalidProductImageSet }">
                  Drag and drop images to here to upload.
                </p>
              </template>
            </FileUpload>
          </div>
        </div>

        <template #footer>
          <Button label="Cancel" icon="pi pi-times" text @click="hideDialog" />
          <Button label="Save" icon="pi pi-check" text @click="handleSubmit" />
        </template>
      </Dialog>

      <Dialog
        v-model:visible="deleteProductDialog"
        :style="{ width: '450px' }"
        header="Confirm"
        :modal="true"
      >
        <div class="confirmation-content">
          <i class="pi pi-exclamation-triangle mr-3" style="font-size: 2rem" />
          <span v-if="product"
            >Are you sure you want to delete <b>{{ product.name }}</b
            >?</span
          >
        </div>
        <template #footer>
          <Button
            label="No"
            icon="pi pi-times"
            text
            @click="deleteProductDialog = false"
          />
          <Button label="Yes" icon="pi pi-check" text @click="deleteProduct" />
        </template>
      </Dialog>

      <Dialog
        v-model:visible="deleteProductsDialog"
        :style="{ width: '450px' }"
        header="Confirm"
        :modal="true"
      >
        <div class="confirmation-content">
          <i class="pi pi-exclamation-triangle mr-3" style="font-size: 2rem" />
          <span v-if="product"
            >Are you sure you want to delete the selected products?</span
          >
        </div>
        <template #footer>
          <Button
            label="No"
            icon="pi pi-times"
            text
            @click="deleteProductsDialog = false"
          />
          <Button
            label="Yes"
            icon="pi pi-check"
            text
            @click="deleteSelectedProducts"
          />
        </template>
      </Dialog>
    </div>
  </div>
</template>

<script>
import { FilterMatchMode } from "primevue/api";
import { HOST } from "@/api/index";
import { ref } from "vue";
import dashboardAPI from "@/pages/dashboard/api/index";

export default {
  setup() {
    const { getProductData, createProduct } = dashboardAPI();

    const productName = ref(null);
    const productDescription = ref("");
    const productCategory = ref(null);
    const productPrice = ref(null);
    const productCollateral = ref(null);
    const productStock = ref(null);
    const productKeywords = ref(null);
    const productImageSet = ref(null);

    const invalidProductName = ref(false);
    const invalidProductDescription = ref(false);
    const invalidProductCategory = ref(false);
    const invalidProductPrice = ref(false);
    const invalidProductCollateral = ref(false);
    const invalidProductStock = ref(false);
    const invalidProductKeywords = ref(false);
    const invalidProductImageSet = ref(false);

    const messageModalVisible = ref(false);
    const messageModal = ref(null);
    const errorModal = ref(null);

    return {
      messageModalVisible,
      messageModal,
      errorModal,
      getProductData,
      createProduct,
      productName,
      productDescription,
      productCategory,
      productPrice,
      productCollateral,
      productStock,
      productKeywords,
      productImageSet,
      invalidProductName,
      invalidProductDescription,
      invalidProductCategory,
      invalidProductPrice,
      invalidProductCollateral,
      invalidProductStock,
      invalidProductKeywords,
      invalidProductImageSet,
    };
  },
  data() {
    return {
      mediaUrl: HOST + "/api/media/create-image",
      products: null,
      productDialog: false,
      deleteProductDialog: false,
      deleteProductsDialog: false,
      descriptionLengthLimit: 1000,
      nameLengthLimit: 200,
      product: {},
      selectedProducts: null,
      filters: {},
      submitted: false,
      selectedCategory: null,
      categories: [
        { name: "Home", code: "home" },
        { name: "Electronics", code: "electronics" },
        { name: "Fashion", code: "fashion" },
        { name: "Beauty", code: "beauty" },
        { name: "Toys", code: "toys" },
        { name: "Tools", code: "tools" },
        { name: "Sports", code: "sports" },
        { name: "Health", code: "health" },
        { name: "Books", code: "books" },
        { name: "Automotive", code: "automotive" },
        { name: "Appliances", code: "appliances" },
        { name: "Furniture", code: "furniture" },
      ],
      statuses: [
        { label: "STOCK", value: "stock" },
        { label: "LOW", value: "low" },
        { label: "OUT", value: "out" },
      ],
    };
  },
  created() {
    this.initFilters();
  },
  mounted() {
    this.products = this.getProductData;
  },
  methods: {
    handleMessage(type, message) {
      this.messageModalVisible = true;

      console.log(type, message);

      if (type === "response") {
        this.messageModal = message.response.message;
      }

      if (type === "error") {
        this.errorModal = message.response.errors;
      }
    },
    hideModals() {
      this.messageModalVisible = false;
    },
    onAdvancedUpload(e) {
      const response = JSON.parse(e.xhr.response);

      if (response.success === true) {
        this.$toast.add({
          severity: "info",
          summary: "Success",
          detail: "File Uploaded",
          life: 3000,
        });

        console.log(response.payload);
      }
    },
    formatCurrency(value) {
      if (value)
        return value.toLocaleString("en-US", {
          style: "currency",
          currency: "USD",
        });
      return;
    },
    openNew() {
      this.product = {};
      this.submitted = false;
      this.productDialog = true;
    },
    hideDialog() {
      this.productDialog = false;
      this.submitted = false;
    },
    async handleSubmit() {
      this.submitted = true;

      const productForm = [
        (this.invalidProductName = !this.checkProductName(this.productName)),
        (this.invalidProductDescription = !this.checkProductDescription(
          this.productDescription
        )),
        (this.invalidProductCategory = !this.checkProductCategory(
          this.productCategory
        )),
        (this.invalidProductPrice = !this.checkProductPrice(this.productPrice)),
        (this.invalidProductCollateral = !this.checkProductCollateral(
          this.productCollateral
        )),
        (this.invalidProductStock = !this.checkProductStock(this.productStock)),
        (this.invalidProductKeywords = !this.checkProductKeywords(
          this.productKeywords
        )),
        (this.invalidProductImageSet = !this.checkProductImageSet(
          this.productImageSet
        )),
      ];

      console.log(productForm);

      if (productForm.includes(true)) return;

      const params = {
        name: this.productName,
        description: this.productDescription,
        category: this.productCategory,
        price: this.productPrice,
        collateral: this.productCollateral,
        stock: this.productStock,
        keywords: this.productKeywords,
        image_set: this.productImageSet,
      };

      const { success } = await this.createProduct(params);

      if (success === true) {
        this.$toast.add({
          severity: "success",
          summary: "Successful",
          detail: "Product Created",
          life: 3000,
        });

        this.product = {};
        this.productDialog = false;
      }
    },
    checkProductName(value) {
      if (!value) return false;

      if (value.length > this.nameLengthLimit) return false;

      return true;
    },
    checkProductDescription(value) {
      if (!value) return false;

      if (value.length > this.descriptionLengthLimit) return false;

      return true;
    },
    checkProductCategory(value) {
      return !value ? false : true;
    },
    checkProductPrice(value) {
      if (typeof value !== "number") {
        return false;
      }

      return Number.isInteger(value) && value > 0;
    },
    checkProductCollateral(value) {
      if (typeof value !== "number") {
        return false;
      }

      return Number.isInteger(value) && value > 0;
    },
    checkProductStock(value) {
      if (typeof value !== "number") {
        return false;
      }

      return Number.isInteger(value) && value >= 0;
    },
    checkProductKeywords(value) {
      if (!value || value.length < 3) {
        return false;
      }

      return true;
    },
    editProduct(product) {
      this.product = { ...product };
      this.productDialog = true;
    },
    confirmDeleteProduct(product) {
      this.product = product;
      this.deleteProductDialog = true;
    },
    deleteProduct() {
      this.products = this.products.filter((val) => val.id !== this.product.id);
      this.deleteProductDialog = false;
      this.product = {};
      this.$toast.add({
        severity: "success",
        summary: "Successful",
        detail: "Product Deleted",
        life: 3000,
      });
    },
    findIndexById(id) {
      let index = -1;
      for (let i = 0; i < this.products.length; i++) {
        if (this.products[i].id === id) {
          index = i;
          break;
        }
      }

      return index;
    },
    createId() {
      let id = "";
      var chars =
        "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
      for (var i = 0; i < 5; i++) {
        id += chars.charAt(Math.floor(Math.random() * chars.length));
      }
      return id;
    },
    exportCSV() {
      this.$refs.dt.exportCSV();
    },
    confirmDeleteSelected() {
      this.deleteProductsDialog = true;
    },
    deleteSelectedProducts() {
      this.products = this.products.filter(
        (val) => !this.selectedProducts.includes(val)
      );
      this.deleteProductsDialog = false;
      this.selectedProducts = null;
      this.$toast.add({
        severity: "success",
        summary: "Successful",
        detail: "Products Deleted",
        life: 3000,
      });
    },
    initFilters() {
      this.filters = {
        global: { value: null, matchMode: FilterMatchMode.CONTAINS },
      };
    },
    getStatusLabel(status) {
      switch (status) {
        case "stock":
          return "success";

        case "low":
          return "warning";

        case "out":
          return "danger";

        default:
          return null;
      }
    },
  },
};
</script>

<style lang="css" scoped>
.product-upload p {
  padding: 1rem;
}
.p-counter {
  font-weight: 500;
  text-align: right;
  padding: 1px 5px;
}
.invalid {
  border: 1px solid red;
  color: red;
  border-radius: 6px;
}

img {
  border-radius: 6px;
}
.products {
  display: flex;
  justify-content: center;
  padding-left: 56px;
  width: 100%;
  background-size: 156.45vw 99.03vw, 156.45vw 99.03vw, 156.45vw 99.03vw,
    226.86vw 145.44vw, 226.86vw 145.44vw, 226.86vw 145.44vw, 171.96vw 110.31vw,
    171.96vw 110.31vw, 171.96vw 110.31vw, 130.29vw 83.58vw, 130.29vw 83.58vw,
    130.29vw 83.58vw, 198vw 126.9vw, 198vw 126.9vw, 198vw 126.9vw, 300vw 192vw,
    300vw 192vw, 300vw 192vw;

  background-position: 37.97vw calc(((300vw - 100vh) / 2 - 85.77vw) * -1),
    37.97vw calc(((300vw - 100vh) / 2 - 85.77vw) * -1),
    37.97vw calc(((300vw - 100vh) / 2 - 85.77vw) * -1),
    -100vw calc(((300vw - 100vh) / 2 - 154.56vw) * -1),
    -100vw calc(((300vw - 100vh) / 2 - 154.56vw) * -1),
    -100vw calc(((300vw - 100vh) / 2 - 154.56vw) * -1),
    13.34vw calc(((300vw - 100vh) / 2 - 53.88vw) * -1),
    13.34vw calc(((300vw - 100vh) / 2 - 53.88vw) * -1),
    13.34vw calc(((300vw - 100vh) / 2 - 53.88vw) * -1),
    10.64vw calc(((300vw - 100vh) / 2 - 17.19vw) * -1),
    10.64vw calc(((300vw - 100vh) / 2 - 17.19vw) * -1),
    10.64vw calc(((300vw - 100vh) / 2 - 17.19vw) * -1),
    -49vw calc(((300vw - 100vh) / 2 - 41.1vw) * -1),
    -49vw calc(((300vw - 100vh) / 2 - 41.1vw) * -1),
    -49vw calc(((300vw - 100vh) / 2 - 41.1vw) * -1),
    -100vw calc(((300vw - 100vh) / 2 - 78vw) * -1),
    -100vw calc(((300vw - 100vh) / 2 - 78vw) * -1),
    -100vw calc(((300vw - 100vh) / 2 - 78vw) * -1);

  background-image: radial-gradient(
      50% 50% at 50% 50%,
      rgba(160, 51, 255, 0.024) 0,
      rgba(160, 51, 255, 0) 50%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(160, 51, 255, 0.04) 0,
      rgba(160, 51, 255, 0) 75%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(160, 51, 255, 0.064) 0,
      rgba(160, 51, 255, 0) 100%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(24, 119, 242, 0.024) 0,
      rgba(24, 119, 242, 0) 50%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(24, 119, 242, 0.04) 0,
      rgba(24, 119, 242, 0) 75%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(24, 119, 242, 0.064) 0,
      rgba(24, 119, 242, 0) 100%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(255, 108, 92, 0.024) 0,
      rgba(255, 108, 92, 0) 50%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(255, 108, 92, 0.04) 0,
      rgba(255, 108, 92, 0) 75%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(255, 108, 92, 0.064) 0,
      rgba(255, 108, 92, 0) 100%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(245, 206, 51, 0.024) 0,
      rgba(245, 206, 51, 0) 50%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(245, 206, 51, 0.04) 0,
      rgba(245, 206, 51, 0) 75%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(245, 206, 51, 0.064) 0,
      rgba(245, 206, 51, 0) 100%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(37, 211, 102, 0.024) 0,
      rgba(37, 211, 102, 0) 50%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(37, 211, 102, 0.04) 0,
      rgba(37, 211, 102, 0) 75%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(37, 211, 102, 0.064) 0,
      rgba(37, 211, 102, 0) 100%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(24, 119, 242, 0.024) 0,
      rgba(24, 119, 242, 0) 50%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(24, 119, 242, 0.04) 0,
      rgba(24, 119, 242, 0) 75%
    ),
    radial-gradient(
      50% 50% at 50% 50%,
      rgba(24, 119, 242, 0.064) 0,
      rgba(24, 119, 242, 0) 100%
    );
}

.products-wrap {
  display: flex;
  justify-content: center;
  width: 100%;
  padding: 1rem 2rem;
}

.products-card {
  width: inherit;
  border-radius: 18px;
  box-shadow: var(--shadow-a);
  padding: 1rem 2rem;
  background: var(--base-a);
}

.product-upload {
  margin-top: 1rem;
}
.table-button {
  margin-left: 1rem;
}

.field {
  margin-bottom: 0.5rem;
}
.field-radiobutton {
  display: flex;
  align-items: center;
}

.field-radiobutton label {
  margin-left: 0.25rem;
}
.field-label {
  line-height: 40px;
  color: var(--text-a);
  font-weight: 600;
  display: flex;
  align-items: center;
}

.field-label i {
  font-size: var(--text-size-a);
  margin-left: 0.5rem;
  margin-top: 3px;
}

.products-header {
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 1rem 0;
  border-bottom: 1px solid var(--border-a);
}

.products-header-left {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  padding: 1rem 0;
}

.products-header-title {
  font-weight: 700;
  font-size: var(--text-size-f);
}

.products-header-subtitle {
  font-weight: 400;
  font-size: var(--text-size-c);
}

::v-deep(.p-dropdown) {
  width: initial;
}
</style>
