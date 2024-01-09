import { _ } from "../utils/pino";
import { app } from "../app";

interface Checkpoints {
  ready: boolean;
}

const checkList: Checkpoints = {
  ready: false,
};

const checkpoint = (point: string) => {
  _.info(`${point} connected`);

  Object.defineProperty(checkList, point, {
    value: true,
  });

  if (isReady(checkList)) {
    const port = process.env.EXPRESS_PORT || "8000";

    const timeout = process.env.EXPRESS_TIMEOUT || "5000";

    const server = app.listen(port, () =>
      _.info(`express server listening in ${port}`)
    );

    server.setTimeout(parseInt(timeout));
  }
};

const isReady = (checkList: Checkpoints) =>
  !Object.values(checkList).includes(false);

const check = () => {
  const time = parseInt(process.env.POD_TIMEOUT!) || 120000;

  setTimeout(() => isReady(checkList) || catcher("pod:timeout"), time);
};

const catcher = (message?: any, error?: any, bypass?: boolean) => {
  _.error(`EXIT=${message}-${error}`);

  return bypass || process.exit(1);
};

export { checkpoint, catcher, check };
