<template>
  <div class="session">
    <MainHeader />

    <div class="session-wrap">
      <TimeLine />

      <div class="session-body">
        <div class="session-body-left">
          <SellerView />
        </div>

        <div class="session-body-right">
          <MiniChat />
        </div>
      </div>

      <div class="session-bottom">z</div>
    </div>
  </div>
</template>

<script>
import MainHeader from "@/components/MainHeader.vue";
import TimeLine from "@/pages/session/components/TimeLine.vue";
import MiniChat from "@/pages/session/components/MiniChat.vue";
import SellerView from "@/pages/session/components/SellerView.vue";
import { ref } from "vue";

export default {
  components: {
    MainHeader,
    MiniChat,
    TimeLine,
    SellerView,
  },
  setup() {
    let currentRoute = ref("");

    return {
      currentRoute,
    };
  },
  created() {
    this.$watch(
      () => this.$route.name,
      (e) => (this.currentRoute = e),
      { immediate: true }
    )();
  },
};
</script>

<style lang="css" scoped>
.session {
}
.session-body {
  width: 100%;
  display: flex;
  align-items: flex-start;
  margin-top: 1rem;
}

.session-body-left {
  width: 60%;
  background: var(--base-a);
}

.session-wrap {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
  padding: 0 10%;
}

.session-body-right {
  width: 40%;
}

.session-bottom {
  height: 50vh;
  background: rgba(0, 128, 0, 0);
}


</style>
