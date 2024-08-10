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
        "84a8008282582008ebfa7ffb3bf114b111818247a798760d306e3bcbc799ce430d0aa006d103ab0082582027dd6c708644ba9521876c71010adb78a305dbde46aac56e34554c154fea0b53010182a300581d7038a291af1c4226df41ec0a344ee6995b822f43b6e2620215c08f627f01821a017d7840a1581cd9ec13627b55c95a3ceb513f17dd3288d1ee62e17fa2f098ee52ed07a14b746872656164746f6b656e01028201d8185828d8799f01581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1a017d7840ff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000022e3001e3021a002dc6c00b582066637bca58d93b8caaa9145f6461f3f49d99b2fa2920cb8f3a581b09c21c7cb10d8182582027dd6c708644ba9521876c71010adb78a305dbde46aac56e34554c154fea0b53010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000022f781243111a0044aa20a20581840000d87980821a0001bb961a02b17e2e068159041a59041701000033232323232323223223232232322533300a3232533300c3005300d37540022646464646464646464a66602a6022602c6ea801c4c8c8c8c8c8c8c94ccc070c060c074dd50008991919299980f980d98101baa001132323232533302300e1533302300b1533302300215333023003100114a029405280a503375e66e952004330263009330264c101010033026375201e6604c6ea00392f5c097ae030053024375400666e24034c94ccc088c06cc08cdd50008a400026eb4c09cc090dd5000992999811180d98119baa00114c0103d87a80001323300100137566050604a6ea8008894ccc09c004530103d87a800013232323253330283372291100002153330283371e9101000021300f3302c375000297ae014c0103d87a8000133006006003375a60520066eb8c09c008c0ac008c0a4004c8cc004004dd5980c18121baa00322533302600114c103d87a800013232323253330273372291100002153330273371e9101000021300e3302b374c00297ae014c0103d87a8000133006006003375660500066eb8c098008c0a8008c0a0004cc024c8cc004004dd5980b98119baa00222533302500114bd700998131811981380099801001181400080f181218109baa00116330043758600260406ea8c020c080dd500a919baf300930213754002601260426ea8c054c084dd500191811981218120009810980f1baa00116330013758600a603a6ea8c014c074dd5009119baf3006301e375400202044646600200200644a666042002298103d87a8000132325333020300500213007330240024bd70099802002000981280118118009ba548000cc004dd6180e980f180f180f180f180f180f180f180f180d1baa3002301a375401e00a44646600200200644a66603c00229404c94ccc070cdc79bae302100200414a22660060060026042002460380026022008264a66602c601e602e6ea80204c9289bae301b30183754010264a66602e66e1d20043018375401229444c9289bad301c301937540106eb4c06cc060dd50039bad301a3017375400c6eb4c064c068008dd7180c000980c0011bad301600130123754002602800460266028002660220146602200e97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea800c5261615333009300200113232533300e3011002149858dd7180780098059baa003153330093370e90020008a99980618059baa00314985854ccc024cdc3a400c0022a66601860166ea800c5261616300937540046e1d200253330053001300637540042646464646464a66601c60220042930b1bad300f001300f002375c601a002601a0046eb4c02c004c01cdd50010b1b8748000dd7000ab9a5573aaae7955cfaba05742ae893011e581cd9ec13627b55c95a3ceb513f17dd3288d1ee62e17fa2f098ee52ed070001f5f6";

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
