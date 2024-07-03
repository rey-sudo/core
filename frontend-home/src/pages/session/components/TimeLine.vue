<template>
  <div class="timeline">
    <div class="timeline-title">
      <span>Negotiation Session</span>
      <span>{{ getSlotData?.id }} </span>
    </div>

    <div class="timeline-subtitle">
      <span>Space for bilateral negotiation </span>
      <span> {{ getSlotData?.created_at }}</span>
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
            <span @click="openCardanoScan(slotProps.item.data)">{{
              slotProps.item.text
            }}</span>

            <button
              v-if="slotProps.item.data"
              @click="copy(slotProps.item.data)"
            >
              <i class="pi pi-copy" />
            </button>
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
import { useClipboard } from "@vueuse/core";
import { NETWORK } from "@/api";

export default {
  setup() {
    const { getSlotData } = sessionAPI();

    const formatHash = (str, maxLength) => {
      if (!str) {
        return "N/A";
      }

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

    const { copy } = useClipboard();
    return {
      eventData,
      getSlotData,
      copy,
    };
  },

  methods: {
    openCardanoScan(txHash) {
      const internalUrl = `https://${NETWORK}.cardanoscan.io/transaction/${txHash}`;
      window.open(internalUrl, "_blank");
    },
  },
};
</script>

<style lang="css" scoped>
.transaction {
  display: flex;
  align-items: center;
}

.transaction button {
  margin-left: 0.25rem;
  border-radius: 6px;
  border: none;
  cursor: pointer;
  background: initial;
}

.transaction button:hover {
  color: var(--blue-c);
}

.transaction span {
  cursor: pointer;
}

.transaction span:hover {
  text-decoration: underline;
}

.timeline {
  background: var(--base-b);
  border: 1px solid var(--border-b);
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 2rem;
  padding-bottom: 1rem;
  margin-top: 2rem;
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
  align-items: flex-start;
  line-height: 2rem;
  font-weight: 700;
}

.timeline-timer span {
  background: var(--black-a);
  border-radius: 6px;
  padding: 0.25rem 0.75rem;
  margin: 0 0.25rem;
  color: var(--text-w);
  border: none;
}

.timeline-title span:nth-child(1) {
  font-size: var(--text-size-f);
  font-weight: 500;
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
