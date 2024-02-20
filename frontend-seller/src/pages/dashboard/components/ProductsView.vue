<template>
  <div class="products">
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

    <Dialog
      v-model:visible="deleteProductDialog"
      :style="{ width: '520px' }"
      header="Confirm"
      :modal="true"
    >
      <div class="confirmation-content">
        <i class="pi pi-exclamation-triangle" style="font-size: 2rem" />
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
      :style="{ width: '520px' }"
      header="Confirm"
      :modal="true"
    >
      <div class="confirmation-content">
        <i class="pi pi-exclamation-triangle" style="font-size: 2rem" />
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

    <Dialog
      v-model:visible="productDialog"
      :style="{ width: '520px' }"
      header="Product details"
      :modal="true"
      :draggable="false"
      class="p-fluid"
    >
      <Carousel
        v-if="product.image_base"
        :value="getImages(product)"
        :numVisible="1"
        :numScroll="1"
        orientation="horizontal"
        verticalViewPortHeight="340px"
        contentClass="flex align-items-center"
      >
        <template #item="slotProps">
          <div class="product-image-wrap">
            <div class="product-image-preview">
              <Image
                :src="slotProps.data.image"
                alt="Image"
                width="330"
                height="330"
                preview
              />
            </div>
          </div>
        </template>
      </Carousel>

      <div v-if="product.image_base" class="field">
        <label for="mainImage" class="field-label">Front image</label>
        <div id="mainImage" class="product-image-main">
          <div
            v-for="item in getImages(product)"
            :key="item"
            :class="{ mainImage: isMainImage(item.id) }"
            @click="setMainImage(item.id)"
          >
            <Image
              :src="item.image"
              imageStyle="border-radius: 6px;display: flex;align-items: center;"
              alt="Image"
              width="50"
              height="50"
            />
          </div>
        </div>
      </div>

      <div class="field">
        <label for="name" class="field-label">Name</label>
        <InputText
          id="name"
          v-model="product.name"
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
          v-model="product.description"
          required="true"
          rows="3"
          cols="20"
          autoResize
          :class="{ invalid: invalidProductDescription }"
        />
        <small
          class="p-counter"
          :class="{
            invalid: product.description.length > descriptionLengthLimit,
          }"
          v-if="!invalidProductDescription"
        >
          {{ product.description.length }} / {{ descriptionLengthLimit }}
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
            v-model="product.category"
            :options="categories"
            id="category"
            optionLabel="name"
            placeholder=""
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
            v-model="product.price"
            showButtons
            prefix="ADA "
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
            v-model="product.collateral"
            showButtons
            prefix="ADA "
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
            v-model="product.stock"
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
            v-model="product.keywords"
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
          <label for="fileupload" class="field-label">
            <span>Images</span>
            <i
              class="pi pi-info-circle"
              v-tooltip.top="'The first image is the preview image.'"
            />
          </label>
          <Toast />
          <FileUpload
            id="fileupload"
            name="image"
            :url="mediaUrl"
            @upload="onAdvancedUpload($event)"
            @before-upload="onBeforeUpload($event)"
            :multiple="true"
            accept="image/*"
            :fileLimit="5"
            :maxFileSize="1000000"
            :withCredentials="true"
            :disabled="disableUpload"
          >
            <template #empty>
              <p>Drag and drop images to here to upload.</p>
            </template>
          </FileUpload>
          <small class="invalid" v-if="invalidProductImages">
            {{ product.image_set.length }} / {{ minProductImages }} images are
            required.
          </small>
        </div>
      </div>

      <template #footer>
        <Button label="Cancel" icon="pi pi-times" text @click="hideDialog" />
        <Button label="Save" icon="pi pi-check" text @click="handleSubmit" />
      </template>
    </Dialog>

    <!---CONTENT-->
    <div class="products-wrap">
      <div class="products-card">
        <DataTable
          ref="dt"
          :value="products"
          v-model:selection="selectedProducts"
          dataKey="product_id"
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
                <span>Products</span>
                <span>Create and modify product and more...</span>
              </div>

              <span class="p-input-icon-left">
                <i class="pi pi-search" />
                <InputText
                  v-model="filters['global'].value"
                  placeholder="Search"
                />
              </span>
            </div>

            <Toolbar>
              <template #start>
                <Button label="New" icon="pi pi-plus" @click="newProduct" />
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
            field="product_id"
            header="Code"
            sortable
            style="min-width: 12rem"
          />
          <Column
            field="name"
            header="Name"
            sortable
            style="max-width: 16rem"
          />

          <Column field="price" header="Price" sortable style="min-width: 8rem">
            <template #body="slotProps">
              {{ formatCurrency(slotProps.data.price) }}
            </template>
          </Column>

          <Column
            field="collateral"
            header="Collateral"
            sortable
            style="min-width: 8rem"
          >
            <template #body="slotProps">
              {{ formatCurrency(slotProps.data.collateral) }}
            </template>
          </Column>

          <Column
            field="moderated"
            header="Status"
            sortable
            style="min-width: 8rem"
          >
            <template #body="slotProps">
              <div
                class="table-tag"
                :class="{
                  pending: slotProps.data.moderated === 0,
                  moderated: slotProps.data.moderated === 1,
                }"
              >
                {{ checkModerated(slotProps.data.moderated) }}
              </div>
            </template>
          </Column>

          <Column
            field="category"
            header="Category"
            sortable
            style="min-width: 8rem; text-transform: capitalize"
          >
            <template #body="slotProps">
              {{ slotProps.data.category.code || slotProps.data.category }}
            </template>
          </Column>

          <Column
            field="rating"
            header="Reviews"
            sortable
            style="min-width: 8rem"
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
            field="stock_status"
            header="Stock"
            sortable
            style="min-width: 8rem"
          >
            <template #body="slotProps">
              <Tag
                :value="slotProps.data.stock_status"
                :severity="getStatusLabel(slotProps.data.stock_status)"
              />
            </template>
          </Column>

          <Column header="Image" style="max-width: 8rem">
            <template #body="slotProps">
              <img
                :src="
                  slotProps.data.image_base +
                  slotProps.data.image_path +
                  slotProps.data.image_main
                "
                :alt="slotProps.data.image_main"
                class="table-image"
              />
            </template>
          </Column>

          <Column :exportable="false" style="min-width: 8rem">
            <template #body="slotProps">
              <div class="table-buttons">
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
              </div>
            </template>
          </Column>
        </DataTable>
      </div>
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
    const { getProductsData, createProduct } = dashboardAPI();

    let product = ref({
      product_id: null,
      seller_id: null,
      name: null,
      description: "",
      category: null,
      price: null,
      collateral: null,
      stock: null,
      stock_status: null,
      slots: null,
      keywords: null,
      theme: null,
      country: null,
      moderated: false,
      image_base: null,
      image_path: null,
      image_main: null,
      image_set: [],
      created_at: null,
      schema_t: null,
      schema_v: null,
    });

    let invalidProductName = ref(false);
    let invalidProductDescription = ref(false);
    let invalidProductCategory = ref(false);
    let invalidProductPrice = ref(false);
    let invalidProductCollateral = ref(false);
    let invalidProductStock = ref(false);
    let invalidProductKeywords = ref(false);
    let invalidProductImages = ref(false);

    let messageModalVisible = ref(false);
    let messageModal = ref(null);
    let errorModal = ref(null);
    let disableUpload = ref(false);

    const resetForm = () => {
      product.value = {
        product_id: null,
        seller_id: null,
        name: null,
        description: "",
        category: null,
        price: null,
        collateral: null,
        stock: null,
        stock_status: null,
        slots: null,
        keywords: null,
        theme: null,
        country: null,
        moderated: false,
        image_base: null,
        image_path: null,
        image_main: null,
        image_set: [],
        created_at: null,
        schema_t: null,
        schema_v: null,
      };

      invalidProductName.value = false;
      invalidProductDescription.value = false;
      invalidProductCategory.value = false;
      invalidProductPrice.value = false;
      invalidProductCollateral.value = false;
      invalidProductStock.value = false;
      invalidProductKeywords.value = false;
      invalidProductImages.value = false;

      messageModalVisible.value = false;
      messageModal.value = null;
      errorModal.value = null;
      disableUpload.value = false;
    };

    return {
      product,
      disableUpload,
      resetForm,
      messageModalVisible,
      messageModal,
      errorModal,
      getProductsData,
      createProduct,
      invalidProductName,
      invalidProductDescription,
      invalidProductCategory,
      invalidProductPrice,
      invalidProductCollateral,
      invalidProductStock,
      invalidProductKeywords,
      invalidProductImages,
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
      minProductImages: 5,
      maxProductImages: 5,
      selectedProducts: null,
      filters: {},
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
    this.products = this.getProductsData;
  },
  methods: {
    onBeforeUpload() {
      if (this.product.image_set.length < this.maxProductImages) {
        this.disableUpload = false;
      }
    },
    handleMessage(type, message) {
      this.messageModalVisible = true;

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
        console.log(this.product.image_set);
        this.product.image_set.push(...response.payload);

        if (this.product.image_set.length >= this.maxProductImages) {
          this.disableUpload = true;
        }

        this.$toast.add({
          severity: "info",
          summary: "Success",
          detail: this.product.image_set.length + " File Uploaded",
          life: 3000,
        });
      }
    },
    formatCurrency(value) {
      if (value) return "ADA " + value;
    },
    newProduct() {
      this.resetForm();
      this.productDialog = true;
    },
    hideDialog() {
      this.productDialog = false;
    },
    async handleSubmit() {
      const productForm = [
        (this.invalidProductName = !this.checkProductName(this.product.name)),
        (this.invalidProductDescription = !this.checkProductDescription(
          this.product.description
        )),
        (this.invalidProductCategory = !this.checkProductCategory(
          this.product.category
        )),
        (this.invalidProductPrice = !this.checkProductPrice(
          this.product.price
        )),
        (this.invalidProductCollateral = !this.checkProductCollateral(
          this.product.collateral
        )),
        (this.invalidProductStock = !this.checkProductStock(
          this.product.stock
        )),
        (this.invalidProductKeywords = !this.checkProductKeywords(
          this.product.keywords
        )),
        (this.invalidProductImages = !this.checkProductImages(
          this.product.image_set
        )),
      ];

      if (productForm.includes(true)) {
        return;
      }

      const params = {
        name: this.product.name,
        description: this.product.description,
        category: this.product.category.code,
        price: this.product.price,
        collateral: this.product.collateral,
        stock: this.product.stock,
        keywords: this.product.keywords.join(","),
        image_set: this.product.image_set.join(","),
      };

      console.log(params);

      await this.createProduct(params).then((res) => {
        if (res.response.success === true) {
          this.$toast.add({
            severity: "success",
            summary: "Successful",
            detail: "Product Created",
            life: 3000,
          });

          this.resetForm();
          this.productDialog = false;
        }
      });
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
    checkProductImages(value) {
      if (!value.length) {
        return false;
      }

      if (this.product.image_set.length < this.minProductImages) {
        return false;
      }

      return true;
    },
    checkModerated(e) {
      if (e === 0) {
        return "Pending";
      }
      if (e === 1) {
        return "Published";
      }
    },
    editProduct(product) {
      product.keywords =
        typeof product.keywords === "string"
          ? product.keywords.split(",")
          : product.keywords;

      product.image_set =
        typeof product.image_set === "string"
          ? product.image_set.split(",")
          : product.image_set;

      let categoryCode = product.category;

      let categoryName =
        categoryCode.charAt(0).toUpperCase() + categoryCode.slice(1);

      product.category = {
        name: categoryName,
        code: categoryCode,
      };

      console.log(product.category);

      this.product = product;

      this.productDialog = true;
    },
    confirmDeleteProduct(product) {
      this.product = product;
      this.deleteProductDialog = true;
    },
    deleteProduct() {
      this.products = this.products.filter(
        (val) => val.product_id !== this.product.product_id
      );

      this.deleteProductDialog = false;

      this.resetForm();

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
    isMainImage(e) {
      return this.product.image_main === e;
    },
    setMainImage(e) {
      this.product.image_main = e;
    },
    getImages(product) {
      const data = product.image_set;

      return data.map((imageId) => ({
        main: product.image_main,
        id: imageId,
        image: product.image_base + product.image_path + imageId,
      }));
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
.product-image-main {
  display: flex;
  justify-content: center;
  align-items: center;
  height: 80px;
}
.product-image-main div {
  padding: 0.25rem;
  border: 1px solid transparent;
  border-radius: 6px;
  display: flex;
  justify-content: center;
  align-items: center;
  margin-right: 1rem;
}

.product-image-main div.mainImage {
  border: 1px solid var(--blue-a);
}
.product-image-wrap {
  display: block;
}
.product-image-preview {
  display: flex;
  justify-content: center;
  align-items: center;
}

.product-image-preview img {
  border-radius: 18px;
  width: 300px;
  height: 300px;
}

.confirmation-content {
  display: flex;
  align-items: center;
}

.confirmation-content span {
  margin-left: 1rem;
}
.table-tag {
  padding: 0.5rem 0;
}

.table-tag.moderated {
}

.table-tag.pending {
}

.table-image {
  width: 100px;
  height: 100px;
}
.table-buttons {
  display: flex;
  align-items: center;
}
.product-upload small {
  padding: 5px;
  line-height: 50px;
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
  margin-bottom: 1rem;
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

.products-header-left span:nth-child(1) {
  font-weight: 700;
  font-size: var(--text-size-f);
}

.products-header-left span:nth-child(2) {
  font-weight: 400;
  font-size: var(--text-size-c);
}

::v-deep(.p-dropdown) {
  width: initial;
}
</style>
