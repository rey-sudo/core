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
        "84a80082825820f680a5fdf41dc0cf41477981508a06b53f6a639b81b2875e6da150755445d77600825820d13922f829260eb24927457890790ab92b6f9df07af45b4ef9d5a9f24e655692010182a300581d7038bec4916a33f88dc7f9058377288230f64779029f641a16c8ee59f901821a047868c0a1581ca1652f13f2a27f5dcaafa875543d00f63629d8b36d11bf7d9f50b8f6a14b746872656164746f6b656e01028201d818584fd8799f01581cd0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af91a017d78401a02faf080d8799f581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bffff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000021fa4dce3021a002dc6c00b58204e6cbfb5fd86a85a97d9cd7ba12a191569470bd305bfb90f547a6793938c56690d81825820d13922f829260eb24927457890790ab92b6f9df07af45b4ef9d5a9f24e655692010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000022288ea03111a0044aa20a20581840001d8799f581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff821a000200101a0327e01b068159045559045201000033232323232323223223232322322533300a3232533300c3007300d375400226464646464646464646464a66602e602660306ea80244c8c8c8c8c8c8c8c94ccc07cc06cc080dd500089919192999811180f18119baa001132323232533302600e1533302600b1533302600215333026003100114a029405280a503375e66e952004330293009330294c1010100330293752024660526ea0040cc0a4dd4007998149804998149ba900e4bd7025eb812f5c0600a604e6ea800ccdc4807192999812981018131baa0011480004dd6981518139baa0013253330253020302637540022980103d87a8000132330010013756605660506ea8008894ccc0a8004530103d87a8000132323232533302b33722911000021533302b3371e9101000021300f3302f375000297ae014c0103d87a8000133006006003375a60580066eb8c0a8008c0b8008c0b0004c8cc004004dd5980d98139baa00322533302900114c103d87a8000132323232533302a33722911000021533302a3371e9101000021300e3302e374c00297ae014c0103d87a8000133006006003375660560066eb8c0a4008c0b4008c0ac004cc024c8cc004004dd5980d18131baa00222533302800114bd7009981498131815000998010011815800810981398121baa00116330043758600260466ea8c020c08cdd500c119baf300930243754002601260486ea8c060c090dd50019181318139813800981218109baa00116330013758600a60406ea8c014c080dd500a919baf30063021375400202644646600200200644a666048002298103d87a8000132325333023300500213007330270024bd70099802002000981400118130009ba548000cc004dd6181018109810981098109810981098109810980e9baa3002301d375402400844646600200200644a66604200229404c94ccc07ccdc79bae302400200414a226600600600260480024603e002602800e6eb8c070c064dd5004899299980c1809980c9baa00a14a2264944dd6980e980d1baa009375a603860326ea8020dd6980d980e0011bad301a001301a002375c603000260300046eb4c058004c048dd5000980a0011809980a000998088051980880325eb80c044c038dd50008a503001300d375400446020602200229309b2b192999804980280089919299980718088010a4c2c6eb8c03c004c02cdd50010a99980498020008a99980618059baa00214985854ccc024cdc3a40080022a66601860166ea8008526161630093754002a66600c6004600e6ea800c4c8c8c8c8c8c8c8c8c8c94ccc04cc0580084c926325333011300d0011323253330163019002149858dd7180b80098099baa00215333011300c00115333014301337540042930b0b18089baa0011630140013014002375a602400260240046eb4c040004c040008dd7180700098070011bad300c001300837540062c6e1d2002370e90001bae0015734aae7555cf2ab9f5740ae855d12611e581ca1652f13f2a27f5dcaafa875543d00f63629d8b36d11bf7d9f50b8f60001f5f6";



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
