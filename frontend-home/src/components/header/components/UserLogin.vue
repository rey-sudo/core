<template>
  <div class="p-userlogin">
    <div class="p-userlogin-wrap" v-if="!getCurrentUser">
      <div class="p-userlogin-wrap-button" @click="handleSign">
        <i class="pi pi-wallet" />
        <span>Wallet Login</span>
      </div>
    </div>

    <div class="p-userlogin-profile" v-if="getCurrentUser">
      <div class="p-userlogin-profile-item">
        <span>ID</span>
        <span>{{ getCurrentUser.id }}</span>
      </div>

      <div class="p-userlogin-profile-item">
        <span>Country</span>
        <span>{{ getCurrentUser.country }}</span>
      </div>

      <div class="p-userlogin-profile-item">
        <span>Address</span>
        <span>{{ shortFormat(getCurrentUser.address, 30) }}</span>
      </div>

      <div class="p-userlogin-profile-item">
        <span>PKH</span>
        <span style="word-break: break-word">
          {{ getCurrentUser.pubkeyhash }}
        </span>
        <span>{{ shortFormat(getCurrentUser.pubkeyhash, 30) }}</span>
      </div>

      <div class="p-userlogin-profile-item">
        <button @click="createTransaction">TX</button>
      </div>

      <div class="p-userlogin-profile-buttons">
        <button class="logout-button" @click="logoutUser">
          <i class="pi pi-sign-out" />
        </button>
      </div>
    </div>
  </div>
</template>

<script>
import { signMessage, getAddress } from "@/api/wallet-api";
import { shortFormat } from "@/utils";
import { balanceTx, lucidClient } from "@/api/wallet-api";
import headerAPI from "../composable/header-api";
import { walletClient } from "@/api/wallet-api";

export default {
  setup() {
    const { loginUser, getCurrentUser, logoutUser } = headerAPI();

    const handleSign = async () => {
      await signMessage()
        .then(async (signature) => [signature, await getAddress()])
        .then(([signature, address]) =>
          loginUser({
            signature,
            address,
            terms_accepted: true,
          })
        )
        .catch((err) => console.error(err));
    };

    const createTransaction = async () => {
      const tx0 =
        "84a80082825820919818c757b8242ffc60ed2ca4bdf4d2c0dea87203075143b1565978197dac3b00825820919818c757b8242ffc60ed2ca4bdf4d2c0dea87203075143b1565978197dac3b01018482581d706b90faf1dbaa96432761ffbea4520e56b42765f287e3c762fe33b10d1a00895440a300581d706b90faf1dbaa96432761ffbea4520e56b42765f287e3c762fe33b10d011a00989680028201d8185823d8799f01581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff82581d60405f9ca0d2c74b034bc206bbcd263d32bee842e785561771fc9a9b99821a000ffb22a1581c54a29c2626156de3af97cdead84264aaf0805857cc5c026af077fc3ba14b746872656164746f6b656e0182581d60405f9ca0d2c74b034bc206bbcd263d32bee842e785561771fc9a9b991b0000000251659cdc021a00032e2e0b5820592e7acee97cd43781865724f311b33f942e3aceedc479d33904f3d19ed492570d81825820919818c757b8242ffc60ed2ca4bdf4d2c0dea87203075143b1565978197dac3b010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082581d60405f9ca0d2c74b034bc206bbcd263d32bee842e785561771fc9a9b991b0000000251fd5527111a0004c545a3049fd8799f00581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bffff0581840000d87980821973ef1a00892a3f0681590196590193010000323232323232322323223225333007323232323232533300d300b300e375400a264a66601c601800620022940c8cc004004dd61809980a180a180a180a180a180a180a180a18081baa30133010375401044a66602400229404c94ccc040cdc79bae301500200414a2266006006002602a002264a66601c66e1d2002300f375400c264944dd7180998081baa006132533300f3370e900218081baa00714a2264944dd6980a18089baa006375a602660206ea8014dd6980918079baa004375c602260240046eb4c040004c030dd50009807001180698070009980580319805801a5eb8052613656325333006300400115333009300837540042930b0a99980319b87480080044c8c94ccc02cc03800852616375c601800260106ea800854ccc018cdc3a40080022a66601260106ea800852616153330063370e90030008a99980498041baa00214985858c018dd5000a999801980098021baa002132323232533300a300d002149858dd7180580098058011bad3009001300537540042c6e1d20005734aae7555cf2ab9f5740ae855d101f5f6";

      const { getWallet } = walletClient();

      lucidClient.selectWallet(getWallet());

      const result = await balanceTx(tx0);

      console.log(result);
    };
    return {
      handleSign,
      getCurrentUser,
      createTransaction,
      logoutUser,
      shortFormat,
    };
  },
};
</script>

<style lang="css" scoped>
.p-userlogin {
  height: 400px;
  width: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
  padding: 1rem;
}
.p-userlogin .p-userlogin-wrap {
  display: flex;
  flex-direction: column;
  justify-content: center;
  border-radius: 12px;
  width: inherit;
}

.p-userlogin .p-userlogin-wrap .p-userlogin-wrap-button {
  background: var(--primary-c);
  color: var(--text-w);
  font-size: var(--text-size-b);
  display: flex;
  padding: 0.75rem 1rem;
  width: 100%;
  font-weight: 500;
  justify-content: center;
  border-radius: 6px;
  cursor: pointer;
}

.p-userlogin .p-userlogin-wrap .p-userlogin-wrap-button span {
  margin-left: 1rem;
}

.p-userlogin .p-userlogin-profile {
  width: 100%;
  height: 100%;
  display: flex;
  flex-direction: column;
}

.p-userlogin .p-userlogin-profile .p-userlogin-profile-item {
  display: flex;
  flex-direction: column;
  margin-top: 1.5rem;
}

.p-userlogin .p-userlogin-profile .p-userlogin-profile-item span {
  line-height: 1.5rem;
}

.p-userlogin .p-userlogin-profile .p-userlogin-profile-item span:nth-child(1) {
  font-weight: 500;
  color: var(--text-a);
  font-size: var(--text-size-b);
}

.p-userlogin .p-userlogin-profile .p-userlogin-profile-item span:nth-child(2) {
  font-weight: 400;
  font-size: var(--text-size-b);
}

.p-userlogin .p-userlogin-profile .p-userlogin-profile-buttons {
  margin-top: auto;
  justify-content: flex-end;
  display: flex;
}

.logout-button {
  padding: 0.5rem;
  border: 1px solid var(--red-a);
  background: transparent;
  border-radius: 4px;
  color: var(--red-a);
  display: flex;
  justify-content: center;
  align-items: center;
  cursor: pointer;
}
</style>
