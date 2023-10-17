import { Telegraf } from "telegraf";
import { message } from "telegraf/filters";

const bot = new Telegraf("6508873772:AAEG1DR3D6_wvREhsHYToXnEBIhlT9w9Iig");

const BOT_1_KEY = "bot-de-magia";
const BOT_2_KEY = "bot-de-paco";

let BOT_1_CHATID: number | undefined;

const BOT_1_DELAY = 10000;
const BOT_2_DELAY = 10000;

let BOT_1_TIMEOUT: NodeJS.Timeout | undefined;
let BOT_2_TIMEOUT: NodeJS.Timeout | undefined;



bot.start((ctx) => {
  ctx.reply("Welcome");
});


const startCommand = BOT_1_KEY + ":" + "start";

bot.hears(startCommand, (ctx) => {
  ctx.reply("Escuchando ordenes");

  BOT_1_CHATID = ctx.message.chat.id;

  console.log("COMPLETEDEEDEDEDEDE", BOT_1_CHATID);

  BOT_1_TIMEOUT = setInterval(() => {
    ctx.reply("En linea 🟢");
  }, BOT_1_DELAY);
});


const stopCommand = BOT_1_KEY + ":" + "detener";

bot.hears(stopCommand, (ctx) => {
  ctx.reply("Bot detenido");
  clearInterval(BOT_1_TIMEOUT);
});

bot.help((ctx) => ctx.reply("Send me a sticker"));

export { bot, BOT_1_CHATID };
