<template>
  <div class="session">
    <div class="session-header">
      <div class="session-header-top">
        <span>Negotiation Session</span>
        <span>Slot order: 783819023812030 </span>
      </div>

      <div class="session-header-bottom">
        <span>Space for bilateral negotiation</span>
        <span>Time Created: may 27 8008 :80:am</span>
      </div>
    </div>

    <div class="session-top">
      <!---LEFT-->
      <div class="session-top-left">
        <div class="session-top-left-top">
          price slot etc... slot information
        </div>
        <div class="session-top-left-body">negotiation information</div>
        <div class="session-top-left-bottom">payment received</div>
      </div>
      <!---LEFT-->

      <!---RIGHT-->
      <div class="session-top-right">
        <div class="session-top-right-chat">
          <div>x</div>

          <div>text</div>
        </div>
      </div>
      <!---RIGHT-->
    </div>
    <div class="session-bottom">z</div>
  </div>
</template>

<script>
export default {};
</script>

<style lang="css" scoped>
.session-header {
  height: 150px;
  background: rgba(255, 0, 0, 0.1);
  display: flex;
  flex-direction: column;
  justify-content: center;
  padding: 1rem;
}

.session-header-top,
.session-header-bottom {
  display: flex;
  justify-content: space-between;
  align-items: center;
  line-height: 2rem;
}

.session-header-top span:nth-child(1) {
  font-size: var(--text-size-f);
  font-weight: 600;
}

.session-header-top span:nth-child(2) {
  font-size: var(--text-size-e);
  font-weight: 600;
}

.session {
  display: flex;
  flex-direction: column;
  min-height: 100vh;
  padding: 0 10%;
}

.session-top {
  width: 100%;
  display: flex;
  align-items: flex-start;
  padding: 1rem;
}

.session-top-left {
  width: 50%;
}

.session-top-right {
  width: 50%;
}

.session-top-right-chat {
  background: var(--base-b);
  width: 400px;
  height: 800px;
  border-radius: 24px;
  margin-left: auto;
  border: 1px solid var(--border-a);
  display: flex;
  flex-direction: column;
  overflow: hidden;
}

.session-top-right-chat div:nth-child(1) {
  height: 100px;
  border-bottom: 1px solid var(--border-a);
}

.session-top-right-chat div:nth-child(2) {
  background: var(--base-a);
  min-height: 700px;
  overflow-y: scroll;
  overflow-x: hidden;
}

.session-bottom {
  height: 50vh;
  background: rgba(0, 128, 0, 0.185);
}

.session-top-left-top {
}
</style>
