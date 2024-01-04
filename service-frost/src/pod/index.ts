import { _ } from "../utils/logger";
import { app } from "../app";

interface PodCheckList {
  ready: boolean;
}

const checkList: PodCheckList = {
  ready: false,
};

function checkpoint(processName: string) {
  _.info(`${processName} connected`);

  Object.defineProperty(checkList, processName, {
    value: true,
  });

  if (!checkPodList(checkList)) {
    const port = process.env.EXPRESS_PORT || "8000";

    const timeout = process.env.EXPRESS_TIMEOUT! || "5000";

    const server = app.listen(port, () =>
      _.info(`express server listening in ${port}`)
    );

    server.setTimeout(parseInt(timeout));
  }
}

function checkPodList(checkList: PodCheckList) {
  return Object.values(checkList).includes(false);
}

function checkPod() {
  setTimeout(
    () => (checkPodList(checkList) ? errorHandler("pod timeout") : false),
    120000
  );
}

function errorHandler(msg?: any, err?: any, bypass?: boolean) {
  _.error(`[POD-EXIT]:${msg} | ${err}`);

  if (bypass) return;

  process.exit(1);
}

export { checkpoint, errorHandler, checkPod };
