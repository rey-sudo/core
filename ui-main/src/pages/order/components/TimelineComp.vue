<template>
  <ul class="timeline" id="timeline">
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Confirmación</span>
        <span class="date">14/12/2023</span>
      </div>
      <div class="status">
        <h4>Compra</h4>
      </div>
    </li>
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Preparación</span>
        <span class="date">17/12/2023</span>
      </div>
      <div class="status">
        <h4>Despacho</h4>
      </div>
    </li>
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Envia #85495845</span>
        <span class="date">18/12/2023</span>
      </div>
      <div class="status">
        <h4>Transporte</h4>
      </div>
    </li>
    <li class="li">
      <div class="timestamp">
        <span class="author">LLegando a casa</span>
        <span class="date">21/12/2023</span>
      </div>
      <div class="status">
        <h4>Entrega</h4>
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
.li
  transition: all 200ms ease-in

span
  margin-bottom: 0.5rem

.date
  color: var(--text-b)
  
.timestamp
  margin-bottom: 1rem
  padding: 0px 40px
  display: flex
  flex-direction: column
  align-items: center
  font-weight: 100
.status
  padding: 0px 40px
  display: flex
  justify-content: center
  border-top: 2px solid var(--border-a)
  position: relative
  transition: all 200ms ease-in
  h4
    font-weight: 600
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
    h4
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
