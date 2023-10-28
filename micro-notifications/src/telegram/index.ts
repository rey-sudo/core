import { Telegraf } from "telegraf";
import { message } from "telegraf/filters";

const bot = new Telegraf(process.env.TELEGRAM_API!);

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
  interval_duration: 3600000,
};

//////////////

bot.hears(S1.user + ":" + S1.key, (ctx) => {
  if (S1.active === false) {
    ctx.reply("LISTENING");

    S1.chat_id = ctx.message.chat.id;

    S1.interval = setInterval(
      () => ctx.reply("Live 🟢"),
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
    ctx.reply("BOT STOPPED");
  }
});



bot.start((ctx) => {
  ctx.reply("Welcome");
});

bot.help((ctx) => ctx.reply("Send me a sticker"));

export { bot, S1 };
