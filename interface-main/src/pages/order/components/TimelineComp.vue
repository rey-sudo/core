<template>
  <ul class="timeline" id="timeline">
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Confirmación</span>
        <span class="date">14/12/2023</span>
      </div>
      <div class="status">
        <label>Compra</label>
      </div>
    </li>
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Preparación</span>
        <span class="date">17/12/2023</span>
      </div>
      <div class="status">
        <label>Despacho</label>
      </div>
    </li>
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Envia #85495845</span>
        <span class="date">18/12/2023</span>
      </div>
      <div class="status">
        <label>Transporte</label>
      </div>
    </li>
    <li class="li">
      <div class="timestamp">
        <span class="author">LLegando a casa</span>
        <span class="date">21/12/2023</span>
      </div>
      <div class="status">
        <label>Entrega</label>
      </div>
    </li>
  </ul>
</template>

<script setup></script>

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
    margin-top: 18px
    margin-right: 9px
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
    left: 42%
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
