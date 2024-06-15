<template>
  <div class="session">
    <MainHeader />

    <div class="session-wrap">
      <div class="session-head">
        <div class="session-head-top">
          <span>Negotiation Session</span>
          <span>ID: S78CAIXOWL540 </span>
        </div>

        <div class="session-head-bottom">
          <span>Space for bilateral negotiation </span>
          <span>Time Created: may 27 07:20:am</span>
        </div>

        <div class="session-body-left-timeline">
          <Timeline :value="events" layout="horizontal" align="top">
            <template #opposite="slotProps">
              <small class="p-text-secondary">{{ slotProps.item.date }}</small>
            </template>
            <template #content="slotProps">
              {{ slotProps.item.status }}
            </template>
          </Timeline>
        </div>

        <div class="session-head-timer">
          <span>2</span>
          <span>2</span>
          <div>:</div>
          <span>3</span>
          <span>8</span>
        </div>
      </div>

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
import TwoChat from "@/pages/session/components/MiniChat.vue";
import { ref } from "vue";

export default {
  components: {
    MainHeader,
    TwoChat,
  },

  setup() {
    const events = ref([
      {
        status: "Waiting",
        date: "15/10/2020 10:30",
        icon: "pi pi-shopping-cart",
        color: "#9C27B0",
      },
      {
        status: "Locking",
        date: "N/A",
        icon: "pi pi-cog",
        color: "#673AB7",
      },
      {
        status: "Delivered",
        date: "N/A",
        icon: "pi pi-shopping-cart",
        color: "#FF9800",
      },
      {
        status: "Received",
        date: "N/A",
        icon: "pi pi-check",
        color: "#607D8B",
      },
    ]);
    return {
      events,
    };
  },
};
</script>

<style lang="css" scoped>
.session-body-left-title {
  font-size: var(--text-size-f);
  text-align: start;
  font-weight: 600;
}

.session-body-left-timeline {
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

.session-head {
  background: var(--base-b);
  border: 1px solid var(--border-b);
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 2rem;
  margin-top: 100px;
  border-radius: 28px;
}

.session-head-top,
.session-head-bottom {
  display: flex;
  justify-content: space-between;
  align-items: center;
  line-height: 2rem;
}

.session-head-timer {
  display: flex;
  justify-content: center;
  align-items: start;
  line-height: 2rem;
  font-weight: 700;
}

.session-head-timer span {
  background: #000000;
  border-radius: 6px;
  padding: 0.25rem 0.75rem;
  margin: 0 0.25rem;
  color: var(--text-w);
  border: none;
}

.session-head-top span:nth-child(1) {
  font-size: var(--text-size-f);
  font-weight: 600;
}

.session-head-top span:nth-child(2) {
  font-size: var(--text-size-e);
  font-weight: 600;
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
