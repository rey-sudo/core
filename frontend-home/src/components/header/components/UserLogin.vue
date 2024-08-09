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
import { signMessage, getAddress, balanceTx } from "@/api/wallet-api";
import { shortFormat } from "@/utils";
import { lucidClient } from "@/api/wallet-api";
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
        "84a80082825820e6f1d52b082b8666f8b0bdfaa5de26d10aca7e562a1c42f47f1f7e4d598e7e5b008258205a57d28b0dd5636753394c9901eb2f525ed1c1b6e75f588ba428c0685079876b010182a300581d70689409889070be6a11b38f2b3c003694a0dc7e14e09796b11d69f74201821a017d7840a1581cf2592b7a63989d549077b1ff614f41371f00d7e03ccb7b2cee9ac1f4a14b746872656164746f6b656e01028201d8185823d8799f01581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000022fbcbc63021a002dc6c00b58208f31004689062560cddb2e2d6a3f3e15a828569cb1a6d655c7bcd2412925e2540d818258205a57d28b0dd5636753394c9901eb2f525ed1c1b6e75f588ba428c0685079876b010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000023104ccc3111a0044aa20a20581840001d87980821a0001669d1a0211348006815902ff5902fc01000033232323232323223223232232322533300a32533300b3004300c37546002601a6ea80084c8c8c8c8c8c94ccc044c034c048dd50028991919299980a180800288010a503233300100137586006602c6ea8c00cc058dd5005a4000444a66603200420022666006006603800466e00c94ccc05cc040c060dd50008a400026eb4c070c064dd500099299980b9808180c1baa00114c103d87a8000132330010013756603a60346ea8008894ccc070004530103d87a8000132323232533301d33722911000021533301d3371e91010000213374a9000198109ba80014bd700a6103d87a8000133006006003375a603c0066eb8c070008c080008c078004c8cc004004dd59806980c9baa300d30193754603800644a666036002298103d87a8000132323232533301c33722911000021533301c3371e91010000213374a9000198101ba60014bd700a6103d87a80001330060060033756603a0066eb8c06c008c07c008c074004004c8cc004004dd6180c180c980c980c980c980c980c980c980c980a9baa30023015375401444a66602e00229404c94ccc054cdc79bae301a00200514a226600600600260340024602e002264a666024601660266ea80184c9289bae30173014375400c264a66602666e1d20043014375400e29444c9289bad30183015375400c6eb4c05cc050dd50029bad3016301337540086eb8c054c058008dd6980a00098081baa0013012002301130120013300f0083300f0054bd700a5023010301100114984d958c94ccc024c01400454ccc030c02cdd50018a4c2c2a666012600400226464a66601c60220042930b1bae300f001300b37540062a66601266e1d20040011533300c300b37540062930b0a99980499b874801800454ccc030c02cdd50018a4c2c2c60126ea8008dc3a4004a66600a6002600c6ea80084c8c8c8c94ccc030c03c00852616375c601a002601a0046eb4c02c004c01cdd50010b1b8748000dd7000ab9a5573aaae7955cfaba05742ae893011e581cf2592b7a63989d549077b1ff614f41371f00d7e03ccb7b2cee9ac1f40001f5f6";

      const { getWallet } = walletClient();

      lucidClient.selectWallet(getWallet());

      try {
        const txHash = await balanceTx(tx0);

        console.log(`Transaction submitted with hash: ${txHash}`);
      } catch (err) {
        console.error(err);
      }
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
