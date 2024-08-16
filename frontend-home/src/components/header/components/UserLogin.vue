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
import { signMessage, getAddress, balanceTx, lucidClient,  walletClient } from "@/api/wallet-api";
import { shortFormat } from "@/utils";
import headerAPI from "../composable/header-api";


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
        "84a8008282582045f10a60ee55115dbeebb44e4c5cdd8e60b75680456b43b67529edfd4c3bbb120082582045f10a60ee55115dbeebb44e4c5cdd8e60b75680456b43b67529edfd4c3bbb12010183a300581d70d2784d0924bc46b86ae5f4d3171320a968eea14e773959074883d4d901821a0013bac8a1581c0f0a5964b1a9a55715e7abebf2543bb76034bdca0c006cbdbcb02b94a14b746872656164746f6b656e01028201d8185830d8799f20581cd0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af91a017d78401a02faf080d87a80ff82583900d0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af98fabad4836a9bda92e0e8f98410ca95b94eaecd475ffb86485a840f11a017d784082583900d0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af98fabad4836a9bda92e0e8f98410ca95b94eaecd475ffb86485a840f11b000000023681d930021a000f42400b58207351ddc0ba8eba515c26abd15970f150ad5ba546f56675ffdc7b869e7090aace0d8182582045f10a60ee55115dbeebb44e4c5cdd8e60b75680456b43b67529edfd4c3bbb12010e81581cd0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af91082583900d0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af98fabad4836a9bda92e0e8f98410ca95b94eaecd475ffb86485a840f11b00000002368df2d8111a0016e360a20581840000d87980821a00018d081a0262192206815905b25905af01000033232323232323223223232322322533300a3232533300c3007300d3754002264646464646464646464646464646464646464646464a666044603c60466ea802c4c8c8c94ccc094c084c098dd5000899192999813981198141baa0011323232533302a0081533302a0071533302a002100114a0294052819baf3374a900219816980b99816a6010120003302d375201a6605a6ea002ccc0b4dd400499816a60103d87a80004bd7025eb80c060c0acdd500119809180c1bab301e302a375400204c605860526ea800458cc04cdd6180a98141baa30123028375403a466ebcc04cc0a4dd5000980998149baa301d302937540046054604e6ea800458cc044dd6180818131baa301030263754036466ebcc044c09cdd500080c998069bac300e30253754601e604a6ea806801cc0780204c8c8c8c8c8c8c8c8c94ccc0acc098c0b0dd500a0991919192999817981598181baa001132325333031302d30323754002264646464a66606a0122a66606a0102a66606a0042a66606a006200229405280a5014a066ebccdd2a40086607060446607098101010033038375201e660706ea0034cc0e0dd40059981c18111981c1ba90094bd7025eb812f5c06046606c6ea800ccdc480519299981a1817981a9baa0011480004dd6981c981b1baa001325333034302f303537540022980103d87a80001323300100137566074606e6ea8008894ccc0e4004530103d87a8000132323232533303a33722911000021533303a3371e910100002130283303e375000297ae014c0103d87a8000133006006003375a60760066eb8c0e4008c0f4008c0ec004c8cc004004dd59815181b1baa00322533303800114c103d87a800013232323253330393372291100002153330393371e910100002130273303d374c00297ae014c0103d87a8000133006006003375660740066eb8c0e0008c0f0008c0e8004cc070c088dd59814181a1baa0010303036303337540022c6603a6eb0c07cc0c8dd5180e18191baa02723375e603a60666ea8004c074c0ccdd5181398199baa0023034303137540022c660366eb0c068c0c0dd5180d18181baa02523375e603660626ea800408ccc05cdd6180c18179baa3019302f375404800460500126eb8c0c0c0b4dd500a099299981619b8748010c0b4dd500a8a511324a26eb4c0c4c0b8dd500a1bad3030302d3754026605e60600046eb4c0b8004c0b8008dd6981600098160011bae302a001302a002375a605000260486ea8028c098c09c008dd6981280098128011bad30230013023002375c604200260420046eb4c07c004c06cdd5000980e805980e180e80511191980080080191299980e8008a50132533301b3371e6eb8c08000801052889980180180098100009180d980e180e180e180e180e180e180e180e0009180d00091191980080080191299980d0008a60103d87a80001323253330193005002130073301d0024bd70099802002000980f001180e0009ba5480008c05cc060c0600048c008004c004004894ccc04c00452f5c02660286022602a00266004004602c002660220146602200c97ae03011300e37540022940c004c034dd50011180818088008a4c26cac64a666012600a0022a66601860166ea80085261615333009300400113232533300e3011002149858dd7180780098059baa002153330093370e90020008a99980618059baa00214985854ccc024cdc3a400c0022a66601860166ea8008526161630093754002a66600c6004600e6ea800c4c8c8c8c8c8c8c8c8c8c94ccc04cc0580084c926325333011300d0011323253330163019002149858dd7180b80098099baa00215333011300c00115333014301337540042930b0b18089baa0011630140013014002375a602400260240046eb4c040004c040008dd7180700098070011bad300c001300837540062c6e1d2002370e90001bae0015734aae7555cf2ab9f5740ae855d126011e581c0f0a5964b1a9a55715e7abebf2543bb76034bdca0c006cbdbcb02b940001f5f6";

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
