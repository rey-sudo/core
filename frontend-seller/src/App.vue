<template>
    <main>
        <router-view />
    </main>
</template>

<script>
import entryAPI from "@/pages/entry/api";
import { walletClient } from "@/api/wallet-api"
import {
    eventMachine
} from "@/api/event";

export default {
    name: "App",
    setup() {
        const {
            getUser
        } = entryAPI();

        const { startWalletService } = walletClient();

        startWalletService()
            .then(() => console.info("WALLET_SERVICE"))
            .catch((err) => console.error(err));



        return {
            getUser,
        };
    },
    mounted() {
        this.getUser()
            .then(() => eventMachine.setup())
            .catch((err) => {
                console.error(err);
            });
    },
};
</script>

<style src="./style/global.css" />
