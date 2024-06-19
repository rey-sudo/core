<template>
  <div class="timeline">
    <div class="timeline-title">
      <span>Negotiation Session</span>
      <span>ID: 4148CA6B3865ACE </span>
    </div>

    <div class="timeline-subtitle">
      <span>Space for bilateral negotiation </span>
      <span>Time Created: may 27 07:20:am</span>
    </div>

    <div class="timeline-body">
      <Timeline
        v-if="getSlotData"
        :value="eventData"
        layout="horizontal"
        align="top"
      >
        <template #opposite="slotProps">
          <div class="transaction">
            <span>{{ slotProps.item.text }}</span>

            <button><i class="pi pi-copy" /></button>
          </div>
        </template>
        <template #content="slotProps">
          {{ slotProps.item.status }}
        </template>
      </Timeline>
    </div>

    <div class="timeline-timer">
      <span>0</span>
      <span>0</span>
      <div>:</div>
      <span>0</span>
      <span>0</span>
    </div>

    <div class="timeline-note">Deploy the script to the blockchain</div>
  </div>
</template>

<script>
import { computed } from "vue";
import { sessionAPI } from "@/pages/session/api";

export default {
  setup() {
    const { getSlotData } = sessionAPI();

    const formatHash = (str, maxLength) => {
      if (str.length <= maxLength) {
        return str;
      }

      const ellipsis = "...";
      const halfLength = Math.floor((maxLength - ellipsis.length) / 2);

      return str.slice(0, halfLength) + ellipsis + str.slice(-halfLength);
    };

    const eventData = computed(() => [
      {
        status: "Waiting",
        text: formatHash(getSlotData.value.contract_0_tx, 20),
        data: getSlotData.value.contract_0_tx,
      },
      {
        status: "Locking",
        text: "N/A",
        data: "",
      },
      {
        status: "Delivered",
        text: "N/A",
        data: "",
      },
      {
        status: "Received",
        text: "N/A",
        data: "",
      },
    ]);

    return {
      eventData,
      getSlotData,
    };
  },

  mounted() {
    //console.log(this.getSlotData?.contract_0_tx);
    // this.eventData[0].text = this.getSlotData?.contract_0_tx;
  },
};
</script>

<style lang="css" scoped>
.transaction {
  display: flex;
  align-items: center;
}

.transaction button {
  margin-left: 0.5rem;
  border-radius: 6px;
  border: none;
  cursor: pointer;
}

.timeline {
  background: var(--base-b);
  border: 1px solid var(--border-b);
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 2rem;
  padding-bottom: 1rem;
  margin-top: 100px;
  border-radius: 28px;
}

.timeline-title,
.timeline-subtitle {
  display: flex;
  justify-content: space-between;
  align-items: center;
  line-height: 2rem;
}

.timeline-timer {
  display: flex;
  justify-content: center;
  align-items: start;
  line-height: 2rem;
  font-weight: 700;
}

.timeline-timer span {
  background: black;
  border-radius: 6px;
  padding: 0.25rem 0.75rem;
  margin: 0 0.25rem;
  color: var(--text-w);
  border: none;
}

.timeline-title span:nth-child(1) {
  font-size: var(--text-size-f);
  font-weight: 600;
}

.timeline-title span:nth-child(2) {
  font-size: var(--text-size-e);
  font-weight: 500;
}

.timeline-body {
}

.timeline-note {
  padding: 1rem;
  font-size: var(--text-size-b);
}
</style>
