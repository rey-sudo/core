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
        "84a80082825820a497223eed43ac7ddcc748b9bb47ce5f39a6eb617496ac2a45a4e250640d9cb00082582077dd658bb37bf0870bec45d34874e7c2aa9802113047d1800a75bac0224fd863010182a300581d7055a24d096852a5b89515941bd5c6929b4b20a2b62f0c41e276f1930001821a047868c0a1581c16310f52df43c355c2b6dd83cd842824051d2d398d64a47ee3f1fe87a14b746872656164746f6b656e01028201d818584fd8799f01581cd0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af91a017d78401a02faf080d8799f581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bffff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000021c7c25a3021a002dc6c00b58206ca057c0922ede699bae9a337ed41fe4c965ab96700460441b97a2fe097a6d7a0d8182582077dd658bb37bf0870bec45d34874e7c2aa9802113047d1800a75bac0224fd863010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000021f6032c3111a0044aa20a20581840001d8799f581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bff821a000204381a032cac5a068159045e59045b01000033232323232323223223232322322533300a3232533300c3007300d3754002264646464646464646464646464a666032602a60346ea802c4c8c8c8c8c8c8c8c94ccc084c074c088dd500089919192999812181018129baa001132323232533302800e1533302800b1533302800215333028003100114a029405280a503375e66e9520043302b30093302b4c10101003302b3752028660566ea0048cc0acdd4008198159804998159ba900e4bd7025eb812f5c0600a60526ea800ccdc4807992999813981118141baa0011480004dd6981618149baa0013253330273022302837540022980103d87a8000132330010013756605a60546ea8008894ccc0b0004530103d87a8000132323232533302d33722911000021533302d3371e9101000021300f33031375000297ae014c0103d87a8000133006006003375a605c0066eb8c0b0008c0c0008c0b8004c8cc004004dd5980e98149baa00322533302b00114c103d87a8000132323232533302c33722911000021533302c3371e9101000021300e33030374c00297ae014c0103d87a80001330060060033756605a0066eb8c0ac008c0bc008c0b4004cc024c8cc004004dd5980e18141baa00222533302a00114bd7009981598141816000998010011816800811981498131baa001163300437586002604a6ea8c020c094dd500d119baf3009302637540026012604c6ea8c068c098dd50019181418149814800981318119baa00116330013758600a60446ea8c014c088dd500b919baf30063023375400202a44646600200200644a66604c002298103d87a8000132325333025300500213007330290024bd70099802002000981500118140009ba548000cc004dd6181118119811981198119811981198119811980f9baa3002301f375402800844646600200200644a66604600229404c94ccc084cdc79bae302600200414a2266006006002604c00246042002602c0126eb8c078c06cdd5005899299980d180a980d9baa00c14a2264944dd6980f980e1baa00b375a603c60366ea8028c074c078008dd6980e000980e0011bad301a001301a002375c603000260300046eb4c058004c048dd5000980a0011809980a000998088051980880325eb80c044c038dd50008a503001300d375400446020602200229309b2b192999804980280089919299980718088010a4c2c6eb8c03c004c02cdd50010a99980498020008a99980618059baa00214985854ccc024cdc3a40080022a66601860166ea8008526161630093754002a66600c6004600e6ea800c4c8c8c8c8c8c8c8c8c8c94ccc04cc0580084c926325333011300d0011323253330163019002149858dd7180b80098099baa00215333011300c00115333014301337540042930b0b18089baa0011630140013014002375a602400260240046eb4c040004c040008dd7180700098070011bad300c001300837540062c6e1d2002370e90001bae0015734aae7555cf2ab9f5740ae855d12611e581c16310f52df43c355c2b6dd83cd842824051d2d398d64a47ee3f1fe870001f5f6";



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
