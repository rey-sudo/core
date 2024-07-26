<template>
  <div class="bubble-wrap" :class="{ sender: sender }">
    <editor-content :editor="editor" />
  </div>
</template>

<script>
import { onBeforeUnmount } from "vue";
import { EditorContent, useEditor } from "@tiptap/vue-3";
import StarterKit from "@tiptap/starter-kit";

export default {
  components: {
    EditorContent,
  },
  props: {
    content: {
      type: Object,
      required: true,
    },
    role: {
      type: String,
      required: true,
    },
    id: {
      type: String,
      required: true,
    },
    sender: {
      type: Boolean,
      required: true,
    },
  },
  setup(props) {
    const bubbleClass = `bubbleClass ${props.sender ? "sender" : ""}`;

    const editor = useEditor({
      extensions: [StarterKit],
      content: props.content,
      editable: false,
      editorProps: {
        attributes: {
          class: bubbleClass,
        },
      },
    });

    onBeforeUnmount(() => {
      if (editor) {
        editor.destroy();
      }
    });

    return {
      editor,
    };
  },
};
</script>

<style scoped>
.ProseMirror {
  padding: 10px;
  border: 1px solid #ddd;
  border-radius: 4px;
}

::v-deep(.bubbleClass) {
  border-radius: 8px;
  max-width: 344px;
  background: rgb(245, 245, 245);
  line-height: 24px;
  padding: 12px;
  white-space: pre-wrap;
  word-break: break-word;
  font-size: 14px;
  font-size: var(--text-size-b);
}

::v-deep(.sender) {
  background: #0084ff;
  color: var(--text-w);
}

.bubble-wrap {
  display: flex;
  margin-top: 1rem;
  border: 1px solid;
  justify-content: flex-start;
  text-align: start;
}

.bubble-wrap.sender {
  justify-content: flex-end;
}
</style>
