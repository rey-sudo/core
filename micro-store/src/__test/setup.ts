import { MongoMemoryReplSet } from "mongodb-memory-server";
import mongoose from "mongoose";
import jwt from "jsonwebtoken";

declare global {
  var signin: () => Promise<string[]>;
}

jest.setTimeout(900000);

let mongo: any;
beforeAll(async () => {
  process.env.NODE_ENV = "test";

  process.env.USERS_JWT_KEY = "service_users_key";

  process.env.AUDITS_JWT_KEY = "service_audits_key";

  process.env.ADMIN_JWT_KEY = "service_admin_key";

  process.env.EVENT_BUS_URI = "event_bus_key";

  process.env.TOKEN_EXPIRATION_TIME = "1h";

  process.env.NODE_TLS_REJECT_UNAUTHORIZED = "0";

  mongoose.set("strictQuery", false);

  const mongo = await new MongoMemoryReplSet({
    replSet: { storageEngine: "wiredTiger" },
  });

  await mongo.waitUntilRunning();

  const mongoUri = await mongo.getUri();

  await mongoose.connect(mongoUri, {});
});

beforeEach(async () => {
  const collections = await mongoose.connection.db.collections();

  for (let collection of collections) {
    await collection.deleteMany({});
  }
});

afterAll(async () => {
  await sleep(11000);

  if (mongo) {
    await mongo.stop();
  }

  await mongoose.connection.close();
});

const userSignInGlobal = () => {
  const payload = {
    scope: "signin",
    entity: "user",
    pid: "xxx",
    email: "test@test.com",
    username: "username1",
    avatar: "https://avatar",
  };

  const token = jwt.sign(payload, process.env.USERS_JWT_KEY!);

  const session = { jwt: token };

  const sessionJSON = JSON.stringify(session);

  const base64 = Buffer.from(sessionJSON).toString("base64");

  return [`session=${base64}`];
};

const adminSignInGlobal = () => {
  const payload = {
    scope: "signin",
    entity: "admin",
    pid: "xxx",
    email: "test@test.com",
    username: "username1",
    avatar: "https://avatar",
  };

  const token = jwt.sign(payload, process.env.ADMIN_JWT_KEY!);

  const session = { jwt: token };

  const sessionJSON = JSON.stringify(session);

  const base64 = Buffer.from(sessionJSON).toString("base64");

  return [`session=${base64}`];
};

const auditorSignInGlobal = (iteration_?: number):any => {
  const iteration = iteration_ || 0;

  const payload = {
    scope: "signin",
    entity: "auditor",
    pid: `xxx${iteration}`,
    email: `test${iteration}@test.com`,
    username: `username${iteration}`,
    avatar: "https://avatar",
  };

  const token = jwt.sign(payload, process.env.AUDITS_JWT_KEY!);

  const session = { jwt: token };

  const sessionJSON = JSON.stringify(session);

  const base64 = Buffer.from(sessionJSON).toString("base64");

  return [`session=${base64}`];
};

const sleep = (timeInMs: number) => {
  return new Promise((resolve) => setTimeout(() => resolve(false), timeInMs));
};

const ROUND_STAGES = {
  pending: "pending",
  selection: "selection",
  "selection-ended": "selection-ended",
  governance: "governance",
  "governance-ended": "governance-ended",
  auditing: "auditing",
  "auditing-ended": "auditing-ended",
};

const generateProject = (iteration?: number) => {
  return {
    name: `liqwid${iteration}`,
    category: "lending",
    funded: "catalyst",
    listed: "community",
    audited: "not",
    symbol: "lq",
    description:
      "Liqwid is an open source, algorithmic and non-custodial interest rate protocol built for lenders, borrowers and developers.",
    logo_url: {
      base: "https://auditdao.nyc3.digitaloceanspaces.com",
      path:  "/logo/liqwid.png",
      base64: "/logo/liqwid.json"
    },
    whitepaper:
      "https://drive.google.com/file/d/1uZNmg72LO19br7s_besobV6RpYJl4Rfl/view",
    website: "https://www.liqwid.finance/",
    repository: { name: "github", link: "https://github.com/Liqwid-Labs" },
    community: {
      reddit: "",
      telegram: "",
      instagram: "",
      youtube: "",
      discord: "https://discord.com/invite/rQhbEBzpYm",
      medium: "https://liqwid-finance.medium.com/",
      facebook: "",
      twitter: "https://twitter.com/liqwidfinance",
    },
    tags: ["liquidity", "borrowing", "earn", "yield"],
  };
};

const generateRandomString = (length: number) => {
  let result = "";
  const characters =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
  for (let i = 0; i < length; i++) {
    const randomIndex = Math.floor(Math.random() * characters.length);
    const randomCharacter = characters.charAt(randomIndex);
    result += randomCharacter;
  }
  return result;
};


const generateReport = () => {
  const REPORT_DATA = {
    data: [
      { id: 0, answer: 1, textarea: "000000000000000000000000", input: null },
      { id: 1, answer: 1, textarea: "1111111111111111111111111", input: null },
      { id: 2, answer: 1, textarea: "222222222222222222222222222", input: null },
      {
        id: 3,
        answer: 1,
        textarea: "3333333333333333333333333333333",
        input: null,
      },
      {
        id: 4,
        answer: 1,
        textarea: "444444444444444444444444444444444",
        input: null,
      },
      { id: 5, answer: 1, textarea: "55555555555555555555555", input: null },
      { id: 6, answer: 1, textarea: "6666666666666666666666666", input: null },
      { id: 7, answer: 2, textarea: "777777777777777777777777777", input: null },
      { id: 8, answer: 1, textarea: "8888888888888888888888", input: null },
      {
        id: 9,
        answer: 2,
        textarea: "",
        input: {
          labels: ["Development team", "OTHER"],
          datasets: [
            {
              data: [50, 50],
              backgroundColor: [
                "#3b82f6",
                "#27aeef",
                "#3bd0b1",
                "#6cecd4",
                "#ffcc5d",
                "#faae04",
                "#fa6367",
                "#fdc1c2",
                "#8a7cf8",
                "#adb9ca",
                "#55c6e0",
                "#1967b3",
                "#ec4899",
                "#b33dc6",
                "#2ea78e",
                "#ea5545",
              ],
              hoverBackgroundColor: [
                "#3b82f6",
                "#27aeef",
                "#3bd0b1",
                "#6cecd4",
                "#ffcc5d",
                "#faae04",
                "#fa6367",
                "#fdc1c2",
                "#8a7cf8",
                "#adb9ca",
                "#55c6e0",
                "#1967b3",
                "#ec4899",
                "#b33dc6",
                "#2ea78e",
                "#ea5545",
              ],
              borderWidth: 1,
            },
          ],
        },
      },
      { id: 10, answer: 1, textarea: "10", input: null },
      { id: 11, answer: 1, textarea: "11", input: null },
      { id: 12, answer: 1, textarea: "12", input: null },
      { id: 13, answer: 1, textarea: "13", input: null },
      { id: 14, answer: 1, textarea: "14", input: null },
      { id: 15, answer: 2, textarea: "15", input: null },
      { id: 16, answer: 1, textarea: "", input: 544 },
      { id: 17, answer: 1, textarea: "17", input: null },
      { id: 18, answer: 1, textarea: "", input: 677 },
      { id: 19, answer: 1, textarea: "19", input: null },
      { id: 20, answer: 1, textarea: "", input: 266 },
      { id: 21, answer: 1, textarea: "21", input: null },
      { id: 22, answer: 1, textarea: "", input: 654 },
      { id: 23, answer: 1, textarea: "23", input: null },
      { id: 24, answer: 1, textarea: "24", input: null },
      { id: 25, answer: 1, textarea: "25", input: null },
      { id: 26, answer: 1, textarea: "26", input: null },
      { id: 27, answer: 1, textarea: "27", input: null },
      { id: 28, answer: 2, textarea: "28", input: null },
      { id: 29, answer: 1, textarea: "29", input: null },
      { id: 30, answer: 1, textarea: "30", input: null },
      { id: 31, answer: 2, textarea: "31", input: null },
      { id: 32, answer: 1, textarea: "32", input: null },
    ],
    notes: {
      answer: "Negative",
      textarea: "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    },
  };

  return REPORT_DATA
}















export {
  ROUND_STAGES,
  userSignInGlobal,
  adminSignInGlobal,
  auditorSignInGlobal,
  generateProject,
  generateReport,
  sleep,
  generateRandomString,
};
