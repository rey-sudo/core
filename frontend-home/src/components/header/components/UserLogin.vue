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
        "84a8008282582095f3eeb54f9fb5895569fe0c1313c2c2f8b68e6eade407cdadc9b5bf7c4060d600825820bf5d0fb68d9e0d7ea57a4e5598116fcaefd53145bb026daa226ceb32bf052f9f010182a300581d700ac6ddec08df343271c6c7753b2a9d350f3c7cb67b6def36defccb4701821a02faf080a1581ca0fb1dba6071ecc4c3e2c6781e5a5f000b156ceb98389a537120eea5a14b746872656164746f6b656e01028201d818582dd8799f01581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1a017d78401a02faf080ff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b00000002291f02a3021a002dc6c00b582067f06e2587aa09ff981d559263c7bb216c10e60da3cd92437f31894c509c89410d81825820bf5d0fb68d9e0d7ea57a4e5598116fcaefd53145bb026daa226ceb32bf052f9f010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000022a859783111a0044aa20a20581840000d87980821a0001c84e1a02c1e56506815903f05903ed01000033232323232323223223232232322533300a3232533300c3005300d375400226464646464646464646464a66602e602660306ea80244c8c8c8c8c8c8c94ccc078c068c07cdd500089919192999810980e98111baa001132323232533302500e1533302500b1533302500215333025003100114a029405280a503375e66e952004330283009330284c1010100330283752022660506ea003ccc0a0dd400725eb812f5c0600a604c6ea800ccdc4807192999812180e98129baa0011480004dd6981498131baa001325333024301d302537540022980103d87a80001323300100137566054604e6ea8008894ccc0a4004530103d87a8000132323232533302a33722911000021533302a3371e9101000021300f3302e375000297ae014c0103d87a8000133006006003375a60560066eb8c0a4008c0b4008c0ac004c8cc004004dd5980d18131baa00322533302800114c103d87a800013232323253330293372291100002153330293371e9101000021300e3302d374c00297ae014c0103d87a8000133006006003375660540066eb8c0a0008c0b0008c0a8004cc024c8cc004004dd5980c98129baa00222533302700114bd7009981418129814800998010011815000810181318119baa00116330043758600260446ea8c020c088dd500b919baf300930233754002601260466ea8c05cc08cdd50019181298131813000981198101baa00116330013758600a603e6ea8c014c07cdd500a119baf30063020375400202444646600200200644a666046002298103d87a8000132325333022300500213007330260024bd70099802002000981380118128009ba548000cc004dd6180f98101810181018101810181018101810180e1baa3002301c375402200e44646600200200644a66604000229404c94ccc078cdc79bae302300200414a226600600600260460024603c002602600c264a666030602260326ea8028528899251375a603a60346ea8024dd6980e180c9baa008375a603660380046eb4c068004c068008dd7180c000980c0011bad301600130123754002602800460266028002660220146602200e97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea800c526161533300930020011533300c300b37540062930b0a99980499b874801000454ccc030c02cdd50018a4c2c2c60126ea8008dc3a4004a66600a6002600c6ea80084c8c8c8c8c8c8c8c94ccc040c04c00852616375a602200260220046eb4c03c004c03c008dd7180680098068011bad300b001300737540042c6e1d2000375c002ae6955ceaab9e5573eae815d0aba24c11e581ca0fb1dba6071ecc4c3e2c6781e5a5f000b156ceb98389a537120eea50001f5f6";



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
