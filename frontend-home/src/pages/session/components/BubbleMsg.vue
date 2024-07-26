<template>
  <editor-content :editor="editor" />
</template>

<script>
import {  onBeforeUnmount } from "vue";
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
  },
  setup(props) {
    const editor = useEditor({
      extensions: [StarterKit],
      content: props.content,
      editable: false,
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
</style>
