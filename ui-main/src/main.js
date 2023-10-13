import App from './App.vue'
import { store } from "./store";
import { router } from "./router";
import { createApp } from 'vue';
import 'primeicons/primeicons.css';

const app = createApp(App)


app.use(store);

app.use(router);


router.isReady().then(() => {
    app.mount("#app");
});
