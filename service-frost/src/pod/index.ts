import { _ } from "../utils/logger";
import { app } from "../app";

interface PodCheckList {
  cacheStore: boolean;
}

const checkList: PodCheckList = {
  cacheStore: true
};

function checkpoint(processName: string) {
  _.info(`${processName} connected`);

  Object.defineProperty(checkList, processName, {
    value: true
  });

  if (!checkListChecker(checkList)) {
    const server = app.listen(process.env.EXPRESS_PORT, () => {
      _.info(`express server listening in ${process.env.EXPRESS_PORT}`);
    });

    server.setTimeout(parseInt(process.env.EXPRESS_TIMEOUT!));               

  }
}

function checkListChecker(checkList: PodCheckList) {
  return Object.values(checkList).includes(false);
}

function setTimeOut() {
  setTimeout(
    () => (checkListChecker(checkList) ? errorHandler("pod timeout") : false),
    120000
  );
}

function errorHandler(msg?: any, err?: any, bypass?: boolean) {
  _.error(`[POD-EXIT]:${msg} | ${err}`);

  if (bypass) return;

  process.exit(1);
}



export { checkpoint, errorHandler, setTimeOut };
