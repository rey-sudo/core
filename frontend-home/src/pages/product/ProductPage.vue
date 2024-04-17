<template>
  <div class="product">
    <MainHeader />
    <div class="product-wrap">
      <div class="product-wrap-top">
        <div class="product-wrap-top-left">
          <Galleria
            :value="galleryImage"
            :responsiveOptions="responsiveOptions"
            :numVisible="1"
            :circular="true"
            :transitionInterval="0"
            containerStyle="max-width: 70%; min-height: 500px; max-height: 500px; margin-top: 10rem;"
            :showItemNavigators="false"
            :showThumbnails="false"
          >
            <template #item="slotProps">
              <img
                :src="slotProps.item.itemImageSrc"
                :alt="slotProps.item.alt"
                style="width: 100%; display: block"
              />
            </template>
          </Galleria>

          <div class="gallery-boxes">
            <div
              class="gallery-boxes-item"
              v-for="(item, index) in images"
              :key="item"
              @click="changeGalleryImage(index)"
              :class="{ imageSelected: isGalleryImage(index) }"
            >
              <img :src="item.thumbnailImageSrc" alt="" />
            </div>
          </div>

          <Accordion :multiple="true" :activeIndex="[0]" style="width: 100%">
            <AccordionTab header="Description">
              <p class="accordionParagraph">
                Welcome to our cutting-edge e-commerce platform where innovation
                meets convenience! Step into the future of entertainment with
                the LG 65” Class UQ70 Series LED 4K UHD Smart webOS TV. Immerse
                yourself in the ultimate viewing experience with stunning 4K UHD
                resolution, bringing every detail to life with remarkable
                clarity and vivid colors. Whether you're watching your favorite
                movies, streaming the latest TV shows, or gaming with friends,
                this TV delivers breathtaking visuals that will captivate your
                senses. Featuring LG's webOS Smart platform, accessing your
                favorite content has never been easier. Navigate seamlessly
                through a world of entertainment with intuitive controls and a
                user-friendly interface. From popular streaming apps to a vast
                array of content options, the possibilities are endless,
                ensuring there's always something to enjoy for everyone in the
                family. With its sleek and modern design, the LG UQ70 Series TV
                effortlessly complements any living space, elevating your home
                entertainment setup to new heights. Its slim bezels and premium
                finish exude sophistication, making it a focal point in any
                room. But it's not just about looks—the LG UQ70 Series is packed
                with innovative features to enhance your viewing experience.
                From advanced image processing technologies for smoother motion
                to immersive audio capabilities that bring sound to life, every
                aspect is designed to provide you with unparalleled
                entertainment. Equipped with multiple HDMI and USB ports,
                connecting your favorite devices such as gaming consoles,
                Blu-ray players, and sound systems is a breeze, ensuring you
                have everything you need for a truly immersive entertainment
                experience. Upgrade your home entertainment setup today with the
                LG 65” Class UQ70 Series LED 4K UHD Smart webOS TV, where
                cutting-edge technology meets unparalleled convenience.
              </p>
            </AccordionTab>
            <AccordionTab header="Terms of sale">
              <p class="accordionParagraph">
                Sed ut perspiciatis unde omnis iste natus error sit voluptatem
                accusantium doloremque laudantium, totam rem aperiam, eaque ipsa
                quae ab illo inventore veritatis et quasi architecto beatae
                vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia
                voluptas sit aspernatur aut odit aut fugit, sed quia
                consequuntur magni dolores eos qui ratione voluptatem sequi
                nesciunt. Consectetur, adipisci velit, sed quia non numquam eius
                modi.
              </p>
            </AccordionTab>
            <AccordionTab header="Guarantee">
              <p class="accordionParagraph">
                At vero eos et accusamus et iusto odio dignissimos ducimus qui
                blanditiis praesentium voluptatum deleniti atque corrupti quos
                dolores et quas molestias excepturi sint occaecati cupiditate
                non provident, similique sunt in culpa qui officia deserunt
                mollitia animi, id est laborum et dolorum fuga. Et harum quidem
                rerum facilis est et expedita distinctio. Nam libero tempore,
                cum soluta nobis est eligendi optio cumque nihil impedit quo
                minus.
              </p>
            </AccordionTab>
          </Accordion>
        </div>

        <div class="product-wrap-top-right">
          <div class="product-name">
            LG - 65” Class UQ70 Series LED 4K UHD Smart webOS TV
          </div>

          <div class="product-sub">
            <span>Model: 8430288C2C</span>

            <span>ID: P9C3KC93CK</span>
          </div>

          <div class="product-rating">
            <span>{{ product.rating_count }}</span>

            <Rating
              :modelValue="product.rating_count"
              :stars="5"
              :readonly="true"
              :cancel="false"
              style="margin: 0 1rem"
            />
            <span>({{ product.review_count }})</span>
          </div>

          <div class="product-price">
            <div class="ada-label">₳</div>
            1.258
          </div>

          <div class="product-bottom">
            <div class="product-bottom-button">Buy now</div>

            <div class="product-bottom-bookmark" v-tooltip.top="'Add to cart'">
              <i class="pi pi-plus" />
            </div>

            <div
              class="product-bottom-bookmark"
              v-tooltip.top="'Save publication'"
            >
              <i class="pi pi-bookmark" />
            </div>
          </div>
        </div>
      </div>

      <div class="product-wrap-body"></div>
    </div>
  </div>
</template>

<script>
import productAPI from "@/pages/product/api";
import MainHeader from "@/components/MainHeader.vue";
import { ref } from "vue";
import { Lucid, getAddressDetails } from "lucid-cardano";
import { balanceTx } from "@/api/wallet-api";

export default {
  components: {
    MainHeader,
  },
  setup() {
    const { lockingEndpoint } = productAPI();

    const galleryImage = ref([]);

    const galleryImageIndex = ref(0);

    const images = ref([
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6501/6501938_rd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6501/6501938_rd.jpg",
        alt: "Description for Image 1",
        title: "Title 1",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6553/6553101_sd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image2/BestBuy_US/images/products/6553/6553101_sd.jpg",
        alt: "Description for Image 2",
        title: "Title 2",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image3/BestBuy_US/images/products/6537/6537363_sd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image3/BestBuy_US/images/products/6537/6537363_sd.jpg",
        alt: "Description for Image 3",
        title: "Title 3",
      },
      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image4/BestBuy_US/images/products/6538/6537363_sd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image4/BestBuy_US/images/products/6538/6537363_sd.jpg",
        alt: "Description for Image 4",
        title: "Title 4",
      },

      {
        itemImageSrc:
          "https://pisces.bbystatic.com/image4/BestBuy_US/images/products/6538/6537363_sd.jpg",
        thumbnailImageSrc:
          "https://pisces.bbystatic.com/image4/BestBuy_US/images/products/6538/6537363_sd.jpg",
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

    const product = ref({
      rating_count: 4.8,
      review_count: 80,
    });

    return {
      images,
      product,
      galleryImage,
      lockingEndpoint,
      responsiveOptions,
      galleryImageIndex,
    };
  },
  data() {
    return {
      lucid: null,
      slot_id: "",
      buyer_pubkeyhash: "",
      tx1: "84a400800181a300581d70c11355d423aed1273fa29eb0aaae09f63f025890f03fedd6ade0b4f5011a01312d0b028201d8185841d8799f004777616974696e67d87980d87980d87980581c4068ce72a0f73e850f19899a10b82ec534a55a6d860e5c5267dca2b9d87a801a01c9c38b1a01312d0bff02000e81581c4068ce72a0f73e850f19899a10b82ec534a55a6d860e5c5267dca2b9a0f5f6",
      tx2: "",
    };
  },

  created() {
    this.$watch(
      () => this.$route.params,
      (e) => (this.slot_id = e.id),
      { immediate: true }
    )();
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
    async setupLucid() {
      this.lucid = await Lucid.new();
    },
    async setupWallet() {
      const api = await window.cardano.nami.enable();

      this.lucid.selectWallet(api);
    },
    async buyProduct() {
      const addr = await this.lucid.wallet.address();
      const address = await getAddressDetails(addr);

      this.buyer_pubkeyhash = address.paymentCredential.hash;

      const params = {
        slot_id: this.slot_id,
        buyer_pubkeyhash: address.paymentCredential.hash,
      };

      this.lockingEndpoint(params)
        .then((res) => balanceTx(res.response.payload.transaction))
        .then((tx) => console.log(tx))
        .catch((err) => console.log(err));
    },

    async buyProduct1(e) {
      const addr = await this.lucid.wallet.address();
      const address = await getAddressDetails(addr);

      this.buyer_pubkeyhash = address.paymentCredential.hash;

      if (e === "1") {
        await balanceTx(this.tx1)
          .then((tx) => console.log(tx))
          .catch((err) => console.log(err));
      }

      if (e === "2") {
        await balanceTx(this.tx2)
          .then((tx) => console.log(tx))
          .catch((err) => console.log(err));
      }
    },
  },

  mounted() {
    this.setupData();

    this.setupLucid();

    this.setupWallet();
  },
};
</script>

<style lang="css" scoped>
.accordionParagraph {
  text-align: left;
  line-height: 1.5rem;
  color: var(--text-b);
  font-weight: 300;
}

.gallery-boxes-item.imageSelected {
  border: 1px solid black;
}

.product {
  display: flex;
  justify-content: center;
  min-height: 100vh;
  background: var(--base-b);
}

.product-price {
  text-align: left;
  margin-top: 1rem;
  font-size: var(--text-size-i);
  font-weight: 600;
  display: flex;
  align-items: baseline;
}

.ada-label {
  font-weight: 400;
  color: var(--text-b);
  font-size: var(--text-size-g);
  margin-right: 0.5rem;
}

.product-wrap {
  width: 90%;
  padding: 2rem;
}

.product-bottom {
  display: flex;
  width: 100%;
  align-items: center;
  margin-top: 2rem;
  justify-content: space-between;
}

.product-bottom-button {
  width: 80%;
  background: var(--blue-a);
  padding: 0.75rem;
  border-radius: 8px;
  color: var(--text-w);
  font-weight: 500;
  cursor: pointer;
}

.product-bottom-bookmark {
  width: 10%;
  cursor: pointer;
}

.product-bottom-bookmark i {
  font-size: var(--text-size-e);
}

.product-wrap-top-left {
  height: inherit;
  text-align: center;
  width: 67%;
  display: flex;
  justify-content: center;
  align-items: center;
  flex-direction: column;
}

.gallery-boxes {
  display: flex;
  justify-content: flex-start;
  margin-top: 2rem;
  margin-bottom: 2rem;
}

.gallery-boxes-item {
  border: 1px solid var(--border-b);
  border-radius: 8px;
  margin-right: 1rem;
  padding: 0.25rem;
  overflow: hidden;
  display: flex;
  align-items: center;
  justify-content: center;
  cursor: pointer;
}

.gallery-boxes-item img {
  width: 60px;
  height: 60px;
  object-fit: contain;
}

.product-wrap-top-right {
  height: inherit;
  text-align: center;
  width: 33%;
  padding: 1rem;
}

.product-wrap-top {
  min-height: 150vh;
  margin-top: 121px;
  display: flex;
  flex-wrap: wrap;
  justify-content: space-between;
  background: var(--base-a);
  padding: 1rem;
  border-radius: 28px;
}

.product-name {
  font-size: var(--text-size-g);
  font-weight: 700;
  margin-top: 5rem;
  text-align: left;
}

.product-sub {
  text-align: left;
  font-size: var(--text-size-b);
  margin-top: 1rem;
}

.product-sub span {
  margin-right: 1rem;
}

.product-rating {
  margin-top: 1rem;
  display: flex;
}

.product-rating {
  margin-right: 0.5rem;
  font-size: var(--text-size-c);
  color: var(--text-b);
  align-items: center;
}
</style>
