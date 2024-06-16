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
      <Timeline :value="eventData" layout="horizontal" align="top">
        <template #opposite="slotProps">
          <small class="p-text-secondary">{{ slotProps.item.date }}</small>
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

    <div class="timeline-note">Deploy the script to the blockchain.</div>
  </div>
</template>

<script>
import { ref } from "vue";

export default {
  setup() {
    const eventData = ref([
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
      eventData,
    };
  },
};
</script>

<style lang="css" scoped>
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
  background: var(--blue-c);
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
  font-weight: 600;
}

.timeline-body {
}

.timeline-note {
  padding: 1rem;
  font-size: var(--text-size-b);
}
</style>
