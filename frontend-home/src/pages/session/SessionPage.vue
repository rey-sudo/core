<template>
  <div class="session">
    <MainHeader />

    <div class="session-wrap">
      <TimeLine />

      <div class="session-body">
        <!---LEFT-->
        <div class="session-body-left">
          <div class="session-body-left-title">Order Information</div>
          <div class="session-body-left-price">
            <div>
              <label>Type <i class="pi pi-info-circle" /></label>
              <span>Batch</span>
            </div>

            <div>
              <label
                >Price

                <i class="pi pi-info-circle" />
              </label>
              <span>3.240 ₳</span>
            </div>

            <div>
              <label>Collateral <i class="pi pi-info-circle" /></label>
              <span>500 ₳</span>
            </div>

            <div>
              <label>Discount <i class="pi pi-info-circle" /></label>
              <span>40 ₳</span>
            </div>

            <div>
              <label>Units <i class="pi pi-info-circle" /></label>
              <span>2</span>
            </div>
          </div>
        </div>
        <!---LEFT-->

        <!---RIGHT-->
        <div class="session-body-right">
          <TwoChat />
        </div>
        <!---RIGHT-->
      </div>
      <div class="session-bottom">z</div>
    </div>
  </div>
</template>

<script>
import MainHeader from "@/components/MainHeader.vue";
import TimeLine from "@/pages/session/components/TimeLine.vue";
import TwoChat from "@/pages/session/components/MiniChat.vue";

export default {
  components: {
    MainHeader,
    TwoChat,
    TimeLine,
  },
};
</script>

<style lang="css" scoped>
.session-body-left-title {
  font-size: var(--text-size-f);
  text-align: start;
  font-weight: 600;
}

.session-body-left-price {
  display: flex;
  align-items: center;
  text-align: start;
  margin-top: 2rem;
  border-radius: 8px;
}

.session-body-left-price div {
  margin-right: 4rem;
  display: block;
}

.session-body-left-price div label {
  display: flex;
  align-items: baseline;
  font-size: var(--text-size-b);
  color: var(--text-a);
}

.session-body-left-price div label i {
  margin-left: 0.5rem;
  font-size: var(--text-size-a);
}

.session-body-left-price div span {
  line-height: 3rem;
  font-size: var(--text-size-e);
  color: var(--text-a);
}

.session {
}

.session-wrap {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
  padding: 0 10%;
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
  border-radius: 28px;
  padding: 2rem;
}

.session-body-right {
  width: 40%;
}

.session-bottom {
  height: 50vh;
  background: rgba(0, 128, 0, 0);
}
</style>
