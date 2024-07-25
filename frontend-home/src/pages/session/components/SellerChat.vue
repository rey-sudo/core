<template>
  <div class="chat">
    <div class="chat-top">
      <img :src="getCurrentSeller.avatar" alt="" />

      <span>{{ getCurrentSeller.username }}</span>
    </div>
    <div class="chat-body">
      <ul id="messages"></ul>
    </div>
    <div class="chat-bottom">
      <div class="chat-wrap">
        <div id="editorElement"></div>
        <button class="chat-wrap-send" @click="sendMessage">
          <i class="pi pi-send" />
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import headerAPI from "@/components/header/composable/header-api";
import StarterKit from "@tiptap/starter-kit";
import Placeholder from "@tiptap/extension-placeholder";
import TextAlign from "@tiptap/extension-text-align";
import { Editor } from "@tiptap/core";
import { io } from "socket.io-client";
import { HOST } from "@/api/index";
import { sessionAPI } from "@/pages/session/api";
import { ref } from "vue";

export default {
  setup() {
    const { getCurrentSeller } = headerAPI();
    const { getSlotData } = sessionAPI();

    const socket = io(HOST, {
      query: {
        role: "SELLER",
        token: getCurrentSeller.value.token,
      },
    });

    const connectRoom = () => {
      const payload = {
        room: getSlotData.value.id,
      };
      socket.emit("joinRoom", JSON.stringify(payload));
    };

    connectRoom();

    const inputValue = ref("");

    const sendMessage = () => {
      const payload = {
        room: getSlotData.value.id,
        content: inputValue.value,
      };

      socket.emit("message", JSON.stringify(payload));
    };

    const editor = ref(null);

    return {
      socket,
      editor,
      connectRoom,
      inputValue,
      sendMessage,
      getCurrentSeller,
    };
  },
  mounted() {
    this.editor = new Editor({
      element: document.getElementById("editorElement"),
      editorProps: {
        attributes: {
          class: "editorClass",
        },
      },
      extensions: [
        StarterKit,
        Text,
        TextAlign.configure({
          alignments: ["left"],
          defaultAlignment: "left",
        }),
        Placeholder.configure({
          placeholder: "write a message…",
          emptyEditorClass: "is-editor-empty",
          emptyNodeClass: "my-custom-is-empty-class",
        }),
      ],
      editable: true,
      content: "",
    });

    const messages = document.getElementById("messages");

    this.socket.on("message", function (msg) {
      const item = document.createElement("li");
      item.textContent = msg;
      messages.appendChild(item);
      window.scrollTo(0, document.body.scrollHeight);
    });
  },
  beforeUnmount() {
    this.editor.destroy();
  },
};
</script>

<style lang="css" scoped>
::v-deep(.tiptap p.is-editor-empty:first-child::before) {
  color: var(--text-b);
  content: attr(data-placeholder);
  float: left;
  opacity: 0.5;
  font-size: var(--text-size-b);
  height: 0;
  font-weight: 500;
  pointer-events: none;
}

::v-deep(.tiptap p.is-empty::before) {
  color: var(--text-b);
  content: attr(data-placeholder);
  float: left;
  height: 0;
  pointer-events: none;
}
::v-deep(.editorClass) {
  width: 330px;
  padding: 0.5rem;
  border: 1px solid var(--border-b);
  border-radius: 6px;
  font-weight: 600;
  text-align: left;
  font-size: var(--text-size-b);
  max-height: 100px;
  overflow-y: scroll;
  overflow-x: hidden;
  scrollbar-width: none;
  -ms-overflow-style: none;
}

::v-deep(.editorClass::-webkit-scrollbar) {
  display: none;
}

.chat-wrap-send {
  border: none;
  width: 2rem;
  height: 2rem;
  display: flex;
  align-items: center;
  justify-content: center;
  background: var(--base-a);
  cursor: pointer;
}

.chat-wrap-send i {
  transform: rotate(45deg);
  color: var(--text-b);
  font-size: var(--text-size-e) !important;
}

.chat-wrap {
  display: flex;
  align-items: center;
}

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
  padding: 1rem;
  border-top: 1px solid var(--border-a);
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
