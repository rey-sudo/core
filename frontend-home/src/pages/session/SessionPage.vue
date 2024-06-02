<template>
  <div class="session">
    <MainHeader />

    <div class="session-body">
      <div class="session-header">
        <div class="session-header-top">
          <span>Negotiation Session</span>
          <span>Slot: S78CAIKJXBRCXV </span>
        </div>

        <div class="session-header-bottom">
          <span>Space for bilateral negotiation </span>
          <span>Time Created: may 27 8008 :80:am</span>
        </div>

        <div class="session-header-timer">
          <span>2</span>
          <span>2</span>
          <div>:</div>
          <span>3</span>
          <span>8</span>
        </div>
      </div>

      <div class="session-top">
        <!---LEFT-->
        <div class="session-top-left">
          <div class="session-top-left-title">Product info</div>
          <div class="session-top-left-price">
            <div>
              <label>Type <i class="pi pi-info-circle" /></label>
              <span>Batch</span>
            </div>

            <div>
              <label
                >Price

                <i class="pi pi-info-circle" />
              </label>
              <span>3.240</span>
            </div>
            <div>
              <label>Collateral <i class="pi pi-info-circle" /></label>
              <span>500</span>
            </div>
            <div>
              <label>Discount <i class="pi pi-info-circle" /></label>
              <span>340</span>
            </div>

            <div>
              <label>Units <i class="pi pi-info-circle" /></label>
              <span>2</span>
            </div>
          </div>

          <div class="session-top-left-timeline"></div>
        </div>
        <!---LEFT-->

        <!---RIGHT-->
        <div class="session-top-right">
          <div class="session-top-right-chat">
            <div>x</div>

            <div>text</div>
          </div>
        </div>
        <!---RIGHT-->
      </div>
      <div class="session-bottom">z</div>
    </div>
  </div>
</template>

<script>
import MainHeader from "@/components/MainHeader.vue";

export default {
  components: {
    MainHeader,
  },
};
</script>

<style lang="css" scoped>
.session-top-left-title {
  font-size: var(--text-size-e);
  text-align: start;
  font-weight: 600;
}

.session-top-left-price {
  display: flex;
  align-items: center;
  text-align: start;
  margin-top: 2rem;
}

.session-top-left-price div {
  margin-right: 4rem;
  display: flex;
  flex-direction: column;
}

.session-top-left-price div label {
  font-size: var(--text-size-c);
  color: var(--text-b);
  display: flex;
  align-items: center;
}

.session-top-left-price div label i {
  margin-left: 0.5rem;
}

.session-top-left-price div span {
  line-height: 3rem;
  font-size: var(--text-size-f);
}

.session {
}

.session-body {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
  padding: 0 10%;
}

.session-header {
  height: 150px;
  background: var(--base-b);
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 2rem;
  margin-top: 150px;
  border-radius: 28px;
}

.session-header-top,
.session-header-bottom {
  display: flex;
  justify-content: space-between;
  align-items: center;
  line-height: 2rem;
}

.session-header-timer {
  display: flex;
  justify-content: center;
  align-items: start;
  line-height: 2rem;
  font-weight: 700;
}

.session-header-timer span {
  background: var(--blue-c);
  border-radius: 8px;
  padding: 0.25rem 0.75rem;
  margin: 0 0.25rem;
  color: var(--text-w);

  border: none;
}

.session-header-top span:nth-child(1) {
  font-size: var(--text-size-f);
  font-weight: 600;
}

.session-header-top span:nth-child(2) {
  font-size: var(--text-size-e);
  font-weight: 600;
}

.session-top {
  width: 100%;
  display: flex;
  align-items: flex-start;
  margin-top: 1rem;
}

.session-top-left {
  width: 50%;
  padding: 2rem;
}

.session-top-right {
  width: 50%;
}

.session-top-right-chat {
  background: var(--base-b);
  width: 400px;
  height: 700px;
  border-radius: 28px;
  margin-left: auto;
  border: 2px solid var(--border-b);
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.session-top-right-chat div:nth-child(1) {
  height: 100px;
  border-bottom: 1px solid var(--border-a);
}

.session-top-right-chat div:nth-child(2) {
  background: var(--base-a);
  min-height: 700px;
  overflow-y: scroll;
  overflow-x: hidden;
}

.session-bottom {
  height: 50vh;
  background: rgba(0, 128, 0, 0);
}
</style>
