<template>
  <div class="timeline">
    <div class="timeline-title">
      <span>Negotiation Session</span>
      <span>{{ getOrderData.id }} </span>
    </div>

    <div class="timeline-subtitle">
      <span>Space for bilateral negotiation </span>
      <span> {{ getOrderData.created_at }}</span>
    </div>

    <div class="timeline-body">
      <Timeline :value="timelineData" layout="horizontal" align="top">
        <template #opposite="orderProps">
          <div class="transaction">
            <span class="checker" v-if="orderProps.item.text !== 'N/A'">
              <i class="pi pi-sort-down-fill" />
            </span>


            <span @click="openCardanoScan(orderProps.item.data)">{{
              orderProps.item.text
            }}</span>

            <button class="copier" v-if="orderProps.item.data" @click="copy(orderProps.item.data)">
              <i class="pi pi-copy" />
            </button>
          </div>
        </template>
        <template #content="orderProps">
          {{ orderProps.item.status }}
        </template>
      </Timeline>
    </div>

    <div class="timeline-timer">
      <span>2</span>
      <span>3</span>
      <div>:</div>
      <span>0</span>
      <span>0</span>
    </div>

    <div class="timeline-note">Perform the action before the deadline.</div>
  </div>
</template>

<script>
import { computed } from "vue";
import { sessionAPI } from "@/pages/session/api";
import { useClipboard } from "@vueuse/core";
import { NETWORK } from "@/api";
import { shortFormat } from "@/utils";

export default {
  setup() {
    const { getOrderData } = sessionAPI();

    const timelineData = computed(() => [
      {
        status: "Waiting",
        text: shortFormat(getOrderData.value.contract_0_tx, 20),
        data: getOrderData.value.contract_0_tx,
      },
      {
        status: "Locking",
        text: shortFormat(getOrderData.value.contract_1_tx, 20),
        data: getOrderData.value.contract_1_tx,
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
      timelineData,
      getOrderData,
      copy,
      shortFormat,
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

.copier {
  margin-left: 0.5rem;
  border-radius: 6px;
  border: none;
  cursor: pointer;
  background: initial;
}

.copier i {
  font-size: 12px;
  color: var(--text-b);
}

.copier i:hover {
  color: var(--primary-c);
}

.transaction span {
  cursor: pointer;
  font-size: var(--text-size-c);
}

.transaction span:hover {
  text-decoration: underline;
}

.timeline {
  background: var(--base-a);
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 2rem;
  padding-bottom: 1rem;
  border-radius: 16px;
  border: 1px solid var(--border-b);
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
  align-items: center;
  line-height: 2rem;
  font-weight: 700;
  font-size: var(--text-size-a);
}

.timeline-timer span {
  background: var(--primary-c);
  border-radius: 8px;
  width: 30px;
  height: 40px;
  display: flex;
  justify-content: center;
  align-items: center;
  margin: 0 0.25rem;
  color: var(--text-w);
  border: 1px solid transparent;
}

.timeline-timer div {
  margin: 0 0.5rem;
}

.timeline-title span:nth-child(1) {
  font-size: var(--text-size-f);
  font-weight: 600;
}

.timeline-title span:nth-child(2) {
  font-size: var(--text-size-d);
  font-weight: 500;
}

.timeline-note {
  padding: 1rem;
  font-size: var(--text-size-b);
}

.checker {
  margin-right: 1rem;
}

.checker i {
  color: var(--green-a);
}
</style>
