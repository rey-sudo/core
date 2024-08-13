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
        "84a800828258208db26fa3abb44bf24f9bcec409c2dc0935d6fe8a0201493805f50254371ff23b00825820f4e68617d0f294b1dae9fcf0d452f23507c892ace5c8333a06cfe056e7298694010182a300581d70b8c4fbb1e8ed39b79bff1577d3e2df9a73d6d72d8a204f14702352dd01821a047868c0a1581ca171d7dcc1bf7d59bddffa61a8d7d9e529cd2ccc984996073549ae6ea14b746872656164746f6b656e01028201d818584bd8799f01581cd0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af91a017d78401a02faf080581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b0000000225f64b63021a002dc6c00b58205c58f3f71beff8ff99038f5465921b2f1114cfabd3129478b785d9656c4e6d0b0d81825820f4e68617d0f294b1dae9fcf0d452f23507c892ace5c8333a06cfe056e7298694010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b0000000228da5883111a0044aa20a20581840000d8799f581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff821a0001d9d21a02d94fd8068159041659041301000033232323232323223223232232322533300a3232533300c3005300d375400226464646464646464646464a66602e602660306ea80244c8c8c8c8c8c8c8c94ccc07cc06cc080dd500089919192999811180f18119baa001132323232533302600e1533302600b1533302600215333026003100114a029405280a503375e66e952004330293009330294c1010100330293752024660526ea0040cc0a4dd4007998149ba900e4bd7025eb80c014c09cdd500199b8900e325333025301e302637540022900009bad302a3027375400264a66604a603c604c6ea80045300103d87a8000132330010013756605660506ea8008894ccc0a8004530103d87a8000132323232533302b33722911000021533302b3371e9101000021300f3302f375000297ae014c0103d87a8000133006006003375a60580066eb8c0a8008c0b8008c0b0004c8cc004004dd5980d98139baa00322533302900114c103d87a8000132323232533302a33722911000021533302a3371e9101000021300e3302e374c00297ae014c0103d87a8000133006006003375660560066eb8c0a4008c0b4008c0ac004cc024c8cc004004dd5980d18131baa00222533302800114bd7009981498131815000998010011815800810981398121baa00116330043758600260466ea8c020c08cdd500c119baf300930243754002601260486ea8c060c090dd50019181318139813800981218109baa00116330013758600a60406ea8c014c080dd500a919baf30063021375400202644646600200200644a666048002298103d87a8000132325333023300500213007330270024bd70099802002000981400118130009ba548000cc004dd6181018109810981098109810981098109810980e9baa3002301d375402400844646600200200644a66604200229404c94ccc07ccdc79bae302400200414a226600600600260480024603e002602800e6eb8c070c064dd5004899299980c1808980c9baa00a14a2264944dd6980e980d1baa009375a603860326ea8020dd6980d980e0011bad301a001301a002375c603000260300046eb4c058004c048dd5000980a0011809980a0009980880519808803a5eb80c044c038dd50008a503001300d375400446020602200229309b2b192999804980280089919299980718088010a4c2c6eb8c03c004c02cdd50018a99980498010008a99980618059baa00314985854ccc024cdc3a40080022a66601860166ea800c5261616300937540046e1d20025333005300130063754004264646464646464646464a666024602a0042930b1bae30130013013002375a602200260220046eb4c03c004c03c008dd7180680098068011bad300b001300737540042c6e1d2000375c002ae6955ceaab9e5573eae815d0aba24c11e581ca171d7dcc1bf7d59bddffa61a8d7d9e529cd2ccc984996073549ae6e0001f5f6";



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
