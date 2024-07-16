<template>
  <div class="head-gallery">
    <div class="head-gallery-boxes">
      <div
        v-for="(item, index) in images"
        :key="item"
        @click="changeGalleryImage(index)"
        @mouseover="changeGalleryImage(index)"
        :class="{ imageSelected: isGalleryImage(index) }"
      >
        <img :src="item.thumbnailImageSrc" alt="" />
      </div>

      <div>
        <span class="mask">+15</span>
      </div>

      <div>
        <span class="mask">
          <i class="pi pi-play-circle" />
        </span>
      </div>
    </div>
    <div class="head-gallery-image">
      <Galleria
        :value="galleryImage"
        :responsiveOptions="responsiveOptions"
        :numVisible="1"
        :circular="true"
        :transitionInterval="0"
        containerStyle="max-width: 60%;   margin-top: 4rem; "
        :showItemNavigators="false"
        :showThumbnails="false"
      >
        <template #item="slotProps">
          <img
            :src="slotProps.item.itemImageSrc"
            :alt="slotProps.item.alt"
            class="p-mediacomp-image"
          />
        </template>
      </Galleria>
    </div>
  </div>
</template>

<script>
import { ref } from "vue";

export default {
  setup() {
    const images = ref([
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615_sd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615_sd.jpg",
        alt: "Description for Image 1",
        title: "Title 1",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615cv11d.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615cv11d.jpg",
        alt: "Description for Image 2",
        title: "Title 2",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615cv12d.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615cv12d.jpg",
        alt: "Description for Image 3",
        title: "Title 3",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615cv13d.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6534/6534615cv13d.jpg",
        alt: "Description for Image 4",
        title: "Title 4",
      },
    ]);

    const responsiveOptions = ref([
      {
        breakpoint: "991px",
        numVisible: 4,
      },
      {
        breakpoint: "767px",
        numVisible: 3,
      },
      {
        breakpoint: "575px",
        numVisible: 1,
      },
    ]);

    const galleryImage = ref([]);

    const galleryImageIndex = ref(0);

    return {
      images,
      responsiveOptions,
      galleryImage,
      galleryImageIndex,
    };
  },

  methods: {
    isGalleryImage(index) {
      return this.galleryImageIndex === index;
    },
    setupData() {
      this.galleryImage[0] = this.images[0];
    },
    changeGalleryImage(index) {
      this.galleryImageIndex = index;
      this.galleryImage[0] = this.images[this.galleryImageIndex];
    },
  },
  mounted() {
    this.setupData();
  },
};
</script>

<style lang="css" scoped>
.head .head-gallery {
  display: flex;
  align-items: center;
  justify-content: space-between;
  width: 100%;
}

.head .head-gallery .head-gallery-boxes {
  display: flex;
  justify-content: flex-start;
  position: relative;
  flex-direction: column;
  height: 100%;
}

.mask {
  background: #55555a;
  border-radius: 4px;
  color: var(--text-w);
  font-weight: bold;
  display: flex;
  align-items: center;
  width: 100%;
  height: 100%;
  justify-content: center;
  opacity: 0.9;
}

.head .head-gallery .head-gallery-boxes div {
  border: 1px solid var(--border-b);
  border-radius: 8px;
  margin-bottom: 1rem;
  padding: 0.25rem;
  overflow: hidden;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
  width: 70px;
  height: 70px;
}

.head .head-gallery .head-gallery-boxes div.imageSelected {
  outline: 2px solid black;
}

.head .head-gallery .head-gallery-boxes div img {
  width: 100%;
  height: 100%;
  object-fit: contain;
}

.head .head-gallery .head-gallery-image {
  width: 100%;
  display: flex;
  justify-content: center;
}

.p-mediacomp-image {
  width: 100%;
  display: block;
  min-height: 350px;
  max-height: 350px;
  object-fit: contain;
}
</style>
