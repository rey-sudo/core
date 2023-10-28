<template>
  <div class="container">
    <span class="loader" />

    <section>
      <PageOne v-if="isReady" />
      <LoaderWrap v-else />
    </section>

    <section>
      <PageTwo v-if="isReady" />
    </section>

    <section>
      <PageThree v-if="isReady" />
    </section>

    <section>
      <PageFour v-if="isReady" />
    </section>

    <section>
      <PageFive v-if="isReady" />
    </section>

    <!--  -->
  </div>
</template>

<script>
/* eslint-disable */
import PageOne from "@/pages/land/components/PageOne";
import PageTwo from "@/pages/land/components/PageTwo";
import PageThree from "@/pages/land/components/PageThree.vue";
import PageFour from "@/pages/land/components/PageFour.vue";
import PageFive from "@/pages/land/components/PageFive.vue";
import landAPI from "@/pages/land/composable/land-api";
import LoaderWrap from "@/components/LoaderWrap.vue";
import { useRouter } from "vue-router";
import { ref } from "vue";

export default {
  components: {
    PageOne,
    PageTwo,
    PageThree,
    PageFour,
    PageFive,
    LoaderWrap,
  },

  setup() {
    const router = useRouter();

    const { action__getProductData } = landAPI();

    const isReady = ref(false);

    return { router, action__getProductData, isReady };
  },
  created() {
    this.$watch(
      () => this.$route.params,
      (e) => {
        const params = {
          pid: e.pid,
        };

        this.action__getProductData(params)
          .then((res) => {
            this.router.replace({
              params: { name: res.response.name.replace(/\s+/g, "-") },
            });

            this.isReady = true;
          })
          .catch((e) => console.error(e));
      },
      { immediate: true }
    )();
  },
};
</script>

<style lang="css" scoped>
.container {
  display: block;
}

.loader {
  width: 40px;
  height: 70px;
  position: relative;
  box-sizing: border-box;
  border: 2px solid #ffffff;
  margin: auto;
  position: fixed;
  bottom: 4%;
  left: calc(50% - 20px);
  z-index: 500;
  border-radius: 50% 50% 50% 50% / 25% 25% 25% 25%;
  animation-name: loaderFade;
  animation-duration: 2s;
  animation-timing-function: ease-in-out;
  animation-fill-mode: forwards;
}

.loader::before {
  content: "";
  position: absolute;
  left: 50%;
  top: 20px;
  transform: translateX(-50%);
  width: 4px;
  height: 4px;
  background: #ffffff;
  border-radius: 10px;
  animation: scrollDown 1.5s linear infinite;
}

@keyframes scrollDown {
  0% {
    top: 10%;
    height: 4px;
    opacity: 1;
  }

  33% {
    height: 30px;
  }

  66% {
    height: 10px;
    opacity: 1;
  }

  100% {
    top: 90%;
    height: 4px;
    opacity: 0;
  }
}

@keyframes loaderFade {
  0% {
    opacity: 0;
    transform: translateY(0);
  }

  10% {
    opacity: 0;
    transform: translateY(-10px);
  }

  90% {
    opacity: 0;
    transform: translateY(-10px);
  }

  100% {
    opacity: 1;
  }
}

@media screen and (max-width: 767px) {
  .loader {
    display: none;
  }
}
</style>
