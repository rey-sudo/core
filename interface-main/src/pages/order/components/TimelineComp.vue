<template>
  <ul class="timeline" id="timeline">
    <li class="li" :class="{ complete: getter__orderData.timeline.purchase.status }">
      <div class="timestamp">
        <span class="author">Confirmación</span>
        <span class="date">{{
          formatDate(getter__orderData.timeline.purchase.date)
        }}</span>
      </div>
      <div class="status">
        <label>Compra</label>
      </div>
    </li>
    <li class="li" :class="{ complete: getter__orderData.timeline.dispatch.status }">
      <div class="timestamp">
        <span class="author">
          {{ getter__orderData.timeline.dispatch.text }}
        </span>
        <span class="date">{{
          formatDate(getter__orderData.timeline.dispatch.date)
        }}</span>
      </div>
      <div class="status">
        <label>Despacho</label>
      </div>
    </li>
    <li class="li" :class="{ complete: getter__orderData.timeline.shipping.status }">
      <div class="timestamp">
        <span class="author">
          {{ getter__orderData.timeline.shipping.text }}
        </span>
        <span class="date">{{
          formatDate(getter__orderData.timeline.shipping.date)
        }}</span>
      </div>
      <div class="status">
        <label>Transporte</label>
      </div>
    </li>
    <li class="li" :class="{ complete: getter__orderData.timeline.delivery.status }">
      <div class="timestamp">
        <span class="author">{{
          getter__orderData.timeline.delivery.text
        }}</span>
        <span class="date">{{
          formatDate(getter__orderData.timeline.delivery.date)
        }}</span>
      </div>
      <div class="status">
        <label>Entrega</label>
      </div>
    </li>
  </ul>
</template>

<script>
import orderAPI from "@/pages/order/composable/order-api";

export default {
  setup() {
    const { getter__orderData } = orderAPI();

    return { getter__orderData };
  },
  methods: {
    formatDate(data) {
      const date = new Date(data);

      const day = date.getDate();
      const month = date.getMonth() + 1;
      const year = date.getFullYear();

      return `${month}/${day}/${year}`;
    },
  },
};
</script>

<style lang="sass" scoped>
.timeline
  list-style-type: none
  display: flex
  align-items: center
  justify-content: center
  background: var(--base-a)
  padding: 1rem
  border-radius: 12px
  box-shadow: var(--shadow-a)
.li
  transition: all 200ms ease-in
  width: 240px
  transition: all 200ms ease-in

span
  margin-bottom: 0.5rem

.date
  color: var(--text-b)

.timestamp
  margin-bottom: 1rem
  display: flex
  flex-direction: column
  font-weight: 100
  justify-content: center
  align-items: center
  text-align: center
.status
  display: flex
  justify-content: center
  align-items: center
  border-top: 2px solid var(--border-a)
  position: relative
  transition: all 200ms ease-in
  label
    font-weight: 600
    margin-top: 2rem
    margin-left: 5px
    color: var(--text-b)
  &:before
    content: ''
    width: 25px
    height: 25px
    place-items: center
    background: white
    border-radius: 25px
    line-height: initial
    color: var(--green)
    text-align: center
    vertical-align: middle
    display: inline-block
    border: 1px solid var(--border-a)
    position: absolute
    top: -15px
    left: 45%
    transition: all 200ms ease-in
.li.complete
  .status
    border-top: 2px solid var(--green)
    &:before
      background: var(--green)
      border: 1px solid transparent
      transition: all 200ms ease-in
    label
      color: var(--green)

@media (min-device-width: 320px) and (max-device-width: 700px)
  .timeline
    list-style-type: none
    display: block
  .li
    transition: all 200ms ease-in
    display: flex
    width: inherit
  .timestamp
    width: 100px
  .status
    &:before
      left: -8%
      top: 30%
      transition: all 200ms ease-in
</style>
