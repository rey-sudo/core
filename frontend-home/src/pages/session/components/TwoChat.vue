<template>
  <div class="chat">
    <div class="chat-top">
      <div></div>
      <span>CHEAPKING700</span>
    </div>
    <div class="chat-body">2</div>
    <div class="chat-bottom">3</div>
  </div>
</template>

<script>
export default {};
</script>

<style lang="css" scoped>
.chat {
  background: var(--base-b);
  width: 400px;
  height: 700px;
  border-radius: 28px;
  margin-left: auto;
  border: 1px solid var(--border-b);
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.chat-body {
  height: 100%;
  background: var(--base-a);
  overflow-y: scroll;
  overflow-x: hidden;
}

.chat-bottom {
  height: 100px;
  background: green;
}

.chat-top {
  height: 100px;
  padding: 1rem;
  display: flex;
  align-items: center;
  border-bottom: 1px solid var(--border-a);
}

.chat-top div:nth-child(1) {
  border-radius: 50%;
  width: 40px;
  height: 40px;
  border: 1px solid var(--border-b);
  background: black;
}

.chat-top span {
  margin-left: 1rem;
  font-weight: 500;
}
</style>
