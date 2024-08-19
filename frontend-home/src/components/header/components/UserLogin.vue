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
        "84aa0082825820991e85e012fc67beeaa16ac718fda6a730e7df09113c4295660a55501727a70400825820991e85e012fc67beeaa16ac718fda6a730e7df09113c4295660a55501727a704010183a300581d7057af85c2a4afaba5b504011424264df080f0f55cc7abf7a3fd487a6e01821a017d7840a1581c5f79702de1786d12f40a09fb741281c93b076cbed7a1d904960faee9a14b746872656164746f6b656e01028201d8185833d8799f00581cd0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af91a017d78401a02faf080d87a80d87a80ff82583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1a02faf08082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000020b927733021a000f4240031a04140dcf081a0413ffbf0b5820ee1d5a7ea86efb5744fe4418247c2eb351e0e9aed63f9223e68e431fec8ebe7e0d81825820991e85e012fc67beeaa16ac718fda6a730e7df09113c4295660a55501727a704010e81581c424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b1082583900424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2bb94b52cc572ff1e84f313777f709c47356b62630bb6bf779d832fe2b1b000000020b8ad613111a0016e360a20581840000d87b80821a0002846e1a03d82cdd06815908185908150100003323232323232322322323232232322533300b3232533300d3008300e375400226464646464646464646464646464646464646464a64666044603c60466ea80284c8c8c94ccc094c084c098dd5000899192999813981198141baa0011323232533302a0081533302a0071533302a002100114a0294052819baf30083302d30163302d4c10120003302d37520186605a6ea0028cc0b4dd400499816a60103d87a80003302d4c103d87a80004bd7025eb80c05cc0acdd500119808980b9bab301d302a375400204c605860526ea800458cc048dd6180a18141baa301130283754038466ebcc048c0a4dd5000980918149baa301c302937540046054604e6ea800458cc040dd6180798131baa300f30263754034466ebcc040c09cdd500080c198061bac300d30253754601c604a6ea8064018c07801c4c8c8c8c8c8c8c8c8c8c94ccc0b0c09cc0b4dd500a09919191919191929998199817981a1baa001132325333035303130363754002264646464a6660720142a6660720122a6660720102a6660720062a666072004200229405280a5014a02940cdd7980b9981e18129981e26010101003303c3752024660786ea0040cc0f0dd40071981e18129981e1ba900b4bd701981e18129981e1ba800a4bd7025eb812f5c0604c60746ea800ccdc4806980a9bab302c303937540046603e604a6eacc0acc0e0dd500081a181d181b9baa001163302037586044606c6ea8c07cc0d8dd5015119baf3020303737540026040606e6ea8c0a8c0dcdd5001181c181a9baa001163301e3758603a60686ea8c074c0d0dd5014119baf301e3035375400204c660346eb0c06cc0ccdd5180e18199baa0270043375e00a980103d87a8000302b00b375a606660680046eb8c0c8004c0b8dd500a09919191919191919191919299981b9817981c1baa01f132323232533303b3037303c375400226464a66607a6072607c6ea80044c8c8c8c94ccc10402854ccc10402454ccc10402054ccc10400c54ccc10400840045280a5014a0294052819baf301f33044302d330444c01010000330443752022660886ea003ccc110dd400699822260103d87a8000330444c103d87a80004bd7025eb80c0b8c108dd500199b87301d3756606860826ea8008038cc09cc0b4dd5981998201baa00103c3042303f37540022c660506eb0c0a8c0f8dd51813981f1baa03223375e6050607e6ea8004c0a0c0fcdd51819181f9baa0023040303d37540022c6604c6eb0c094c0f0dd51812981e1baa03023375e604c607a6ea80040b8c94ccc0e8c0d4c0ecdd5000899299981d981b981e1baa0051337126eb4c100c0f4dd50028008a50375a607e60786ea80045281812181d9baa3024303b3754607c607e607e607e607e607e607e607e60766ea8c090c0ecdd5017a99981c181a181c9baa0031330213758604460746ea8c08cc0e8dd50171bae303d303a37540062940c0c80284c94ccc0e0cdc3a400c60726ea80804c0cc0044c0c0dd6981e981d1baa01f375a607860726ea8078c0ecc0f0008c0e8004c0e8008dd6981c000981c0011bad30360013036002375c606800260680046eb4c0c8004c0b8dd5009981818188011bad302f001302f002375a605a002605a0046eb8c0ac004c0ac008dd6981480098129baa00a2325333024301f302537540022900009bad30293026375400264a666048603e604a6ea80045300103d87a80001323300100137566054604e6ea8008894ccc0a4004530103d87a8000132323232533302a33722911000021533302a3371e910100002130173302e375000297ae014c0103d87a8000133006006003375a60560066eb8c0a4008c0b4008c0ac004c8cc004004008894ccc0a00045300103d87a800013232323253330293372291100002153330293371e910100002130163302d374c00297ae014c0103d87a8000133006006003375660540066eb8c0a0008c0b0008c0a8004dd2a40086eb4c094c098008dd6981200098120011bae30220013022002375a604000260386ea8004c07802cc074c07802888c8cc00400400c894ccc078004528099299980e19b8f375c604200400829444cc00c00c004c0840048c070c074c074c074c074c074c074c074c0740048c06c00488c8cc00400400c894ccc06c0045300103d87a800013232533301a3005002130073301e0024bd70099802002000980f801180e8009ba5480008c060c064c0640048c008004c004004894ccc05000452f5c026602a6024602c00266004004602e002660240166602400e97ae03012300f37540022940c004c038dd50011180898090008a4c26cac64a666014600c0022a66601a60186ea800c526161533300a300500113232323253330113014002149858dd6980900098090011bae3010001300c37540062a66601460040022a66601a60186ea800c526161533300a3370e90030008a99980698061baa00314985854ccc028cdc3a40100022a66601a60186ea800c5261616300a37540046e1d200453330063002300737540062646464646464646464646464a66602a6030004264649319299980a180800089919299980c980e0010a4c2c6eb4c068004c058dd50018a99980a18078008a99980b980b1baa00314985858c050dd5001192999809980780089919299980c180d8010a4c2c6eb8c064004c054dd50020a99980998070008a99980b180a9baa00414985858c04cdd50018b180b000980b001180a000980a0011bad30120013012002375a602000260200046eb8c038004c038008dd6980600098041baa00316370e90011b8748000dd7000ab9a5573aaae7955cfaba05742ae8930011e581c5f79702de1786d12f40a09fb741281c93b076cbed7a1d904960faee90001f5f6";

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
