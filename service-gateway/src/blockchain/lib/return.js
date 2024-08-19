import {
  ColdWallet,
  Core,
  Data,
  Blaze,
  makeValue,
  Blockfrost,
} from "@blaze-cardano/sdk";
import { applyParamsToScript, cborToScript } from "@blaze-cardano/uplc";

const provider = new Blockfrost({
  network: "cardano-preprod",
  projectId: "preprodex26NYImZOT84XAA67qhyHyA7TT6PCGI",
});

const externalWallet = Core.addressFromBech32(
  "addr_test1qppygdhzm0t7nnlclmds3dy0wc3du870dpy48juu0xxuu2aefdfvc4e0785y7vfhwlmsn3rn26mzvv9md0mhnkpjlc4s0jshh4"
);

const stateMachineAddress = Core.addressFromBech32(
  "addr_test1wpt6lpwz5jh6hfd4qsq3gfpxfhcgpu84tnr6haarl4y85msl6vr9v"
);

const wallet = new ColdWallet(externalWallet, 2, provider);

const blaze = await Blaze.from(provider, wallet);

///////////////////////////////////

const threadTokenUnit =
  "5f79702de1786d12f40a09fb741281c93b076cbed7a1d904960faee9746872656164746f6b656e";

const assetId = Core.AssetId(threadTokenUnit);

const threadTokenPolicyId = Core.AssetId.getPolicyId(assetId);

const assetName = Buffer.from(
  Core.AssetId.getAssetName(assetId),
  "hex"
).toString();

const threadTokenUtxos = await provider.getUnspentOutputsWithAsset(
  stateMachineAddress,
  assetId
);

if (threadTokenUtxos.length < 1) {
  throw new Error("ZERO_THREADTOKEN_UTXOS");
}

if (threadTokenUtxos.length > 1) {
  throw new Error("THREADTOKEN_QUANTITY");
}

for (const utxo of threadTokenUtxos) {
  const utxoRef = `${utxo.input().transactionId()}#${utxo.input().index()}`;
  console.log(utxoRef);

  const amountADA = utxo.output().amount().coin();
  console.log(`Amount of ADA: ${amountADA / 1000000n}`);

  const amountToken = utxo.output().amount().multiasset().get(threadTokenUnit);
  console.log(`Amount of ${assetName}: ${amountToken}`);
}

//////////////////////////////////

const stateMachineInput = Data.Enum([
  Data.Literal("Cancel"),
  Data.Object({
    Locking: Data.Object({
      buyer_param: Data.Bytes(),
      range_param: Data.Integer(),
    }),
  }),
  Data.Literal("Return"),
  Data.Literal("Shipping"),
  Data.Literal("Received"),
]);

const returnInput = "Return";

const stateMachineRedeemer = Data.to(returnInput, stateMachineInput);

const stateMachineScript = cborToScript(
  applyParamsToScript(
    "5907f2010000323232323232322322323232232322533300b3232533300d3008300e375400226464646464646464646464646464646464646464a64666044603c60466ea80284c8c8c94ccc094c084c098dd5000899192999813981198141baa0011323232533302a0081533302a0071533302a002100114a0294052819baf30083302d30163302d4c010120003302d37520186605a6ea0028cc0b4dd400499816a60103d87a80003302d4c103d87a80004bd7025eb80c05cc0acdd500119808980b9bab301d302a375400204c605860526ea800458cc048dd6180a18141baa301130283754038466ebcc048c0a4dd5000980918149baa301c302937540046054604e6ea800458cc040dd6180798131baa300f30263754034466ebcc040c09cdd500080c198061bac300d30253754601c604a6ea8064018c07801c4c8c8c8c8c8c8c8c8c8c94ccc0b0c09cc0b4dd500a09919191919191929998199817981a1baa001132325333035303130363754002264646464a6660720142a6660720122a6660720102a6660720062a666072004200229405280a5014a02940cdd7980b9981e18129981e26010101003303c3752024660786ea0040cc0f0dd40071981e18129981e1ba900b4bd701981e18129981e1ba800a4bd7025eb812f5c0604c60746ea800ccdc4806980a9bab302c303937540046603e604a6eacc0acc0e0dd500081a181d181b9baa001163302037586044606c6ea8c07cc0d8dd5015119baf3020303737540026040606e6ea8c0a8c0dcdd5001181c181a9baa001163301e3758603a60686ea8c074c0d0dd5014119baf301e3035375400204c660346eb0c06cc0ccdd5180e18199baa0270043375e00a980103d87a8000302b00b375a606660680046eb8c0c8004c0b8dd500a09919191919191919191919299981b9817981c1baa01f132323232533303b3037303c375400226464a66607a6072607c6ea80044c8c8c8c94ccc10402854ccc10402454ccc10402054ccc10400c54ccc10400840045280a5014a0294052819baf301f33044302d330444c01010000330443752022660886ea003ccc110dd400699822260103d87a8000330444c103d87a80004bd7025eb80c0b8c108dd500199b87301d3756606860826ea8008038cc09cc0b4dd5981998201baa00103c3042303f37540022c660506eb0c0a8c0f8dd51813981f1baa03223375e6050607e6ea8004c0a0c0fcdd51819181f9baa0023040303d37540022c6604c6eb0c094c0f0dd51812981e1baa03023375e604c607a6ea80040b8c94ccc0e8c0d4c0ecdd5000899299981d981b981e1baa0051337126eb4c100c0f4dd50028008a50375a607e60786ea80045281812181d9baa3024303b3754607c607e607e607e607e607e607e607e60766ea8c090c0ecdd5017a99981c181a181c9baa0031330213758604460746ea8c08cc0e8dd50171bae303d303a37540062940c0c80284c94ccc0e0cdc3a400c60726ea80804c0cc0044c0c0dd6981e981d1baa01f375a607860726ea8078c0ecc0f0008c0e8004c0e8008dd6981c000981c0011bad30360013036002375c606800260680046eb4c0c8004c0b8dd5009981818188011bad302f001302f002375a605a002605a0046eb8c0ac004c0ac008dd6981480098129baa00a2325333024301f302537540022900009bad30293026375400264a666048603e604a6ea80045300103d87a80001323300100137566054604e6ea8008894ccc0a4004530103d87a8000132323232533302a33722911000021533302a3371e910100002130173302e375000297ae014c0103d87a8000133006006003375a60560066eb8c0a4008c0b4008c0ac004c8cc004004008894ccc0a00045300103d87a800013232323253330293372291100002153330293371e910100002130163302d374c00297ae014c0103d87a8000133006006003375660540066eb8c0a0008c0b0008c0a8004dd2a40086eb4c094c098008dd6981200098120011bae30220013022002375a604000260386ea8004c07802cc074c07802888c8cc00400400c894ccc078004528099299980e19b8f375c604200400829444cc00c00c004c0840048c070c074c074c074c074c074c074c074c0740048c06c00488c8cc00400400c894ccc06c0045300103d87a800013232533301a3005002130073301e0024bd70099802002000980f801180e8009ba5480008c060c064c0640048c008004c004004894ccc05000452f5c026602a6024602c00266004004602e002660240166602400e97ae03012300f37540022940c004c038dd50011180898090008a4c26cac64a666014600c0022a66601a60186ea800c526161533300a300500113232323253330113014002149858dd6980900098090011bae3010001300c37540062a66601460040022a66601a60186ea800c526161533300a3370e90030008a99980698061baa00314985854ccc028cdc3a40100022a66601a60186ea800c5261616300a37540046e1d200453330063002300737540062646464646464646464646464a66602a6030004264649319299980a180800089919299980c980e0010a4c2c6eb4c068004c058dd50018a99980a18078008a99980b980b1baa00314985858c050dd5001192999809980780089919299980c180d8010a4c2c6eb8c064004c054dd50020a99980998070008a99980b180a9baa00414985858c04cdd50018b180b000980b001180a000980a0011bad30120013012002375a602000260200046eb8c038004c038008dd6980600098041baa00316370e90011b8748000dd7000ab9a5573aaae7955cfaba05742ae89",
    [threadTokenPolicyId],
    { dataType: "list", items: [{ dataType: "bytes" }] }
  ),
  "PlutusV2"
);

const productPrice = 50n * 1_000_000n;

const productCollateral = 25n * 1_000_000n;

const threadTokenAsset = makeValue(
  productCollateral,
  ...[[threadTokenUnit, 1n]]
);

const minFee = 1n * 1_000_000n;

const data = {
  state: 0n,
  seller: "d0f4b0252c3c54d0ec21fe600c51489db9d5c534f14afc3227aa7af9",
  collateral: productCollateral,
  price: productPrice,
  buyer: null,
  range: null,
};

const Datum = Data.Object({
  state: Data.Integer(),
  seller: Data.Bytes(),
  collateral: Data.Integer(),
  price: Data.Integer(),
  buyer: Data.Nullable(),
  range: Data.Nullable(),
});

const returnDatum = Data.to(data, Datum);

const laterTime = Date.now() + 1 * 60 * 60 * 1000;

try {
  const tx = await blaze
    .newTransaction()
    .addInput(threadTokenUtxos[0], stateMachineRedeemer)
    .lockAssets(stateMachineAddress, threadTokenAsset, returnDatum)
    .payLovelace(externalWallet, productPrice)
    .provideScript(stateMachineScript)
    .addRequiredSigner(
      "424436e2dbd7e9cff8fedb08b48f7622de1fcf684953cb9c798dce2b"
    )
    .setValidFrom(toSlot(Core.SLOT_CONFIG_NETWORK.Preprod, Date.now()))
    .setValidUntil(toSlot(Core.SLOT_CONFIG_NETWORK.Preprod, laterTime))
    .setChangeAddress(externalWallet)
    .setMinimumFee(minFee)
    .complete();

  const cbor = tx.toCbor();
  console.log(cbor);
} catch (err) {
  console.error(err);
}

/*
    .setValidFrom(Core.Slot(currentTime))
    .setValidUntil(Core.Slot(laterTime))
*/

function toUnixTime(config, slot) {
  const { zeroSlot, slotLength, zeroTime } = config;
  const deltaSlot = slot - zeroSlot;
  const halfSlotLength = Math.floor(0.5 * slotLength);
  const msAfterZeroSlot = deltaSlot * slotLength + halfSlotLength;
  return zeroTime + msAfterZeroSlot;
}

function toSlot(config, unixTime) {
  const { zeroSlot, slotLength, zeroTime } = config;
  const timePassed = unixTime - zeroTime;
  const slotsPassed = Math.floor(timePassed / slotLength);
  return Core.Slot(zeroSlot + slotsPassed);
}

const mySlot = toSlot(Core.SLOT_CONFIG_NETWORK.Preprod, Date.now());
