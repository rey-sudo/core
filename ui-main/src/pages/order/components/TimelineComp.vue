<template>
  <ul class="timeline" id="timeline">
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Confirmación</span>
        <span class="date">11 sep 2023</span>
      </div>
      <div class="status">
        <h4>Compra</h4>
      </div>
    </li>
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Preparación</span>
        <span class="date">14 sep 2023</span>
      </div>
      <div class="status">
        <h4>Despacho</h4>
      </div>
    </li>
    <li class="li complete">
      <div class="timestamp">
        <span class="author">Guia #85495845</span>
        <span class="date">17 sep 2023</span>
      </div>
      <div class="status">
        <h4>Transporte</h4>
      </div>
    </li>
    <li class="li">
      <div class="timestamp">
        <span class="author">LLegando a casa</span>
        <span class="date">18 sep 2023</span>
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

.timestamp
  margin-bottom: 20px
  padding: 0px 40px
  display: flex
  flex-direction: column
  align-items: center
  font-weight: 100
.status
  padding: 0px 40px
  display: flex
  justify-content: center
  border-top: 2px solid #D6DCE0
  position: relative
  transition: all 200ms ease-in
  h4
    font-weight: 600
    color: var(--text-b)
  &:before
    content: ''
    width: 25px
    height: 25px
    background-color: white
    border-radius: 25px
    border: 1px solid #ddd
    position: absolute
    top: -15px
    left: 42%
    transition: all 200ms ease-in
.li.complete
  .status
    border-top: 2px solid #66DC71
    &:before
      background-color: #66DC71
      border: none
      transition: all 200ms ease-in
    h4
      color: #66DC71

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
