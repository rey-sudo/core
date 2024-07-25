<template>
  <div class="chat">
    <div class="chat-top">
      <img :src="getCurrentUser?.avatar" alt="" />

      <span>{{ getCurrentUser.pubkeyhash }}</span>
    </div>
    <div class="chat-body">
      <ul id="messages"></ul>
    </div>
    <div class="chat-bottom">
      <input id="input" autocomplete="off" />
      <button id="send">Send</button>
    </div>
  </div>
</template>

<script>
import headerAPI from "@/components/header/composable/header-api";
import { io } from "socket.io-client";
import { HOST } from "@/api/index";
import { sessionAPI } from "@/pages/session/api";

export default {
  setup() {
    const { getCurrentUser } = headerAPI();
    const { getSlotData } = sessionAPI();

    const socket = io(HOST, {
      query: {
        role: "USER",
        token: getCurrentUser.value.token,
      },
    });

    const connectRoom = () => {
      const payload = {
        room: getSlotData.value.id,
      };
      socket.emit("joinRoom", JSON.stringify(payload));
    };

    connectRoom();

    return {
      socket,
      connectRoom,
      getCurrentUser,
    };
  },
  mounted() {
    const messages = document.getElementById("messages");

    this.socket.on("message", function (msg) {
      const item = document.createElement("li");
      item.textContent = msg;
      messages.appendChild(item);
      window.scrollTo(0, document.body.scrollHeight);
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
