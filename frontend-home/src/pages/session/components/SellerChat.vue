<template>
  <div class="chat">
    <div class="chat-top">
      <img :src="getCurrentSeller?.avatar" alt="" />

      <span>{{ getCurrentSeller?.username }}</span>
    </div>
    <div class="chat-body">
      <ul id="messages"></ul>
    </div>
    <div class="chat-bottom">
      <form id="form" action="">
        <input id="input" autocomplete="off" />
        <button>Send</button>
      </form>
      <button id="join">Join</button>
    </div>
  </div>
</template>

<script>
import headerAPI from "@/components/header/composable/header-api";
import { io } from "socket.io-client";

export default {
  setup() {
    const { getCurrentSeller } = headerAPI();

    const socket = io("https://pairfy.dev");

    return {
      socket,
      getCurrentSeller,
    };
  },
  mounted() {
    const form = document.getElementById("form");
    const input = document.getElementById("input");
    const messages = document.getElementById("messages");

    form.addEventListener("submit", function (e) {
      e.preventDefault();
      if (input.value) {
        const payload = {
          content: input.value,
          room: "room1",
        };
        this.socket.emit("message", JSON.stringify(payload));
        input.value = "";
      }
    });

    this.socket.on("message", function (msg) {
      const item = document.createElement("li");
      item.textContent = msg;
      messages.appendChild(item);
      window.scrollTo(0, document.body.scrollHeight);
    });

    const myButton = document.getElementById("join");

    myButton.addEventListener("click", () => {
      this.socket.emit("join", "room1");
    });
  },
};
</script>

<style lang="css" scoped>
.chat {
  background: var(--base-a);
  width: 400px;
  height: 700px;
  border-radius: 16px;
  margin-left: auto;
  border: 3px solid var(--border-a);
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
}

.chat-top {
  height: 100px;
  padding: 1rem;
  display: flex;
  align-items: center;
  border-bottom: 1px solid var(--border-a);
}

.chat-top img {
  border-radius: 50%;
  width: 40px;
  height: 40px;
  border: 1px solid var(--border-a);
  background: var(--base-a);
}

.chat-top span {
  margin-left: 1rem;
  font-weight: 500;
}
</style>
