const { Telegraf } = require("telegraf");
const { message } = require("telegraf/filters");

const bot = new Telegraf("6508873772:AAEG1DR3D6_wvREhsHYToXnEBIhlT9w9Iig");

bot.start((ctx) => {
  ctx.reply("Welcome");
});


const BOT_1_KEY = "bot-de-magia";
const BOT_2_KEY = "bot-de-paco";

const BOT_1_DELAY = 10000;
const BOT_2_DELAY = 10000;


let BOT_1_INTERVAL = null;
let BOT_2_INTERVAL = null;


//BOT1
/////////////////////////////////////////////////////////
bot.hears(BOT_1_KEY, (ctx) => {
  ctx.reply("Escuchando ordenes");
  ctx.reply("Se verificará cada 10 segundos");

  BOT_1_INTERVAL = setInterval(() => {
    ctx.reply("En linea 🟢");
  }, BOT_1_DELAY);
});

bot.hears(BOT_1_KEY + ":" + 'detener', (ctx) => {
  ctx.reply("Bot detenido");
  clearInterval(BOT_1_INTERVAL);
});
/////////////////////////////////////////////////////////




bot.help((ctx) => ctx.reply("Send me a sticker"));

bot.launch();

process.once("SIGINT", () => bot.stop("SIGINT"));
process.once("SIGTERM", () => bot.stop("SIGTERM"));
