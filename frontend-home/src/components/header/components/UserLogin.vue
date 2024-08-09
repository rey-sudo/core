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
        "84a800828258202dbf48e89595fa7734736f0e2fdd5345023bb2f0cbcdadb58817613b355e6f2900825820fb2ef57b6b0ec4aa71819cd70b49e247e62e47b5053b4733f0519489f426acae010182a300581d705335e23d5946949897a15543e222f3913bd812183752e598707d8c5901821a017d7840a1581cd686789520a6983db317cfd1e05d73066458c47bb5067e1e32288e3fa14b746872656164746f6b656e01028201d8185823d8799f01581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b00000002314976e3021a002dc6c00b5820c3e1bbcca2c52a90bfce62009b9c2ad2df3e0ce3237522f569b0078e905270330d81825820fb2ef57b6b0ec4aa71819cd70b49e247e62e47b5053b4733f0519489f426acae010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b0000000232918743111a0044aa20a20581840000d87980821a00015d431a02003e0106815902ec5902e901000033232323232323223223232232322533300a3232323232325333010300c3011375400a2646464a666026601e00a20042940c8ccc004004dd61801980a9baa3003301537540149000111299980c00108008999801801980d80119b80325333016300f301737540022900009bad301b3018375400264a66602c601e602e6ea8004530103d87a8000132330010013756603860326ea8008894ccc06c004530103d87a8000132323232533301c33722911000021533301c3371e91010000213374a9000198101ba80014bd700a6103d87a8000133006006003375a603a0066eb8c06c008c07c008c074004c8cc004004dd5991800980c9baa300130193754603800846038603a00244a666034002298103d87a8000132323232533301b33722911000021533301b3371e91010000213374a90001980f9ba60014bd700a6103d87a8000133006006003375660380066eb8c068008c078008c070004004c8cc004004dd6180b980c180c180c180c180c180c180c180c180a1baa30023014375401244a66602c00229404c94ccc050cdc79bae301900200514a226600600600260320024602c002264a666022601460246ea80184c9289bae30163013375400c264a66602466e1d20043013375400e29444c9289bad30173014375400c6eb4c058c04cdd50029bad3015301237540086eb8c050c054008dd6980980098079baa0013011002301030110013300e0073300e0044bd700a4c26cac64a666012600a0022a66601860166ea800c5261615333009300200113232533300e3011002149858dd7180780098059baa003153330093370e90020008a99980618059baa00314985854ccc024cdc3a400c0022a66601860166ea800c5261616300937540046e1d20025333005300130063754004264646464a666018601e0042930b1bae300d001300d002375a6016002600e6ea800858dc3a40006eb80055cd2ab9d5573caae7d5d02ba157449811e581cd686789520a6983db317cfd1e05d73066458c47bb5067e1e32288e3f0001f5f6";

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
