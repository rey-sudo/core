import { Telegraf } from "telegraf";
import { message } from "telegraf/filters";

const bot = new Telegraf("6508873772:AAEG1DR3D6_wvREhsHYToXnEBIhlT9w9Iig");

const S1_KEY = "bot-de-magia";
const S2_KEY = "bot-de-paco";

let S1_CHATID: number | undefined;

const S1_DELAY = 20000;
const S2_DELAY = 20000;

let S1_TIMEOUT: NodeJS.Timeout | undefined;
let S2_TIMEOUT: NodeJS.Timeout | undefined;

bot.start((ctx) => {
  ctx.reply("Welcome");
});

const startCommand = S1_KEY + ":" + "start";

bot.hears(startCommand, (ctx) => {
  ctx.reply("Escuchando ordenes");

  S1_CHATID = ctx.message.chat.id;

  S1_TIMEOUT = setInterval(() => ctx.reply("En linea 🟢"), S1_DELAY);
});

const stopCommand = S1_KEY + ":" + "detener";

bot.hears(stopCommand, (ctx) => {
  ctx.reply("Bot detenido");
  clearInterval(S1_TIMEOUT);
});

bot.help((ctx) => ctx.reply("Send me a sticker"));

export { bot, S1_CHATID };
