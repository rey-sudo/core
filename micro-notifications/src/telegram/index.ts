import { Telegraf } from "telegraf";
import { message } from "telegraf/filters";

const bot = new Telegraf("6508873772:AAEG1DR3D6_wvREhsHYToXnEBIhlT9w9Iig");

interface SLAVE {
  active: boolean;
  user: string;
  key: string;
  chat_id: number | undefined;
  interval: NodeJS.Timeout | undefined;
  interval_duration: number | undefined;
}

let S1: SLAVE = {
  active: false,
  user: "magia",
  key: "password1",
  chat_id: undefined,
  interval: undefined,
  interval_duration: 10000,
};
//////////////

bot.hears(S1.user + ":" + S1.key, (ctx) => {
  if (S1.active === false) {
    ctx.reply("Escuchando ordenes");

    S1.chat_id = ctx.message.chat.id;

    S1.interval = setInterval(
      () => ctx.reply("En linea 🟢"),
      S1.interval_duration
    );

    S1.active = true;
  }
});

bot.hears(S1.user + ":" + "stop", (ctx) => {
  if (S1.active) {
    S1.active = false;
    S1.chat_id = undefined;
    clearInterval(S1.interval);

    ctx.reply("Bot detenido");
  }
});

//////////////

bot.start((ctx) => {
  ctx.reply("Welcome");
});

bot.help((ctx) => ctx.reply("Send me a sticker"));

export { bot, S1 };
