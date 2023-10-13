import { _ } from "../utils/logger";

interface PodCheckList {
  eventBus: boolean;
  eventDriver: boolean;
  mongoWrapper: boolean;
}

const checkList: PodCheckList = {
  eventBus: false,
  eventDriver: false,
  mongoWrapper: false
};

function connHandler(processName: string) {
  _.info(`${processName} connected`);

  Object.defineProperty(checkList, processName, {
    value: true
  });
}

function checkListChecker(checkList: PodCheckList) {
  return Object.values(checkList).includes(false);
}

function setTimeOut() {
  setTimeout(
    () => (checkListChecker(checkList) ? errorHandler("pod timeout") : false),
    70000
  );
}


function errorHandler(msg?: any, err?: any, bypass?: boolean) {
  _.error(`[POD-EXIT]:${msg} | ${err}`);

  if (bypass) return;

  process.exit(1);
}

export { connHandler, errorHandler, setTimeOut };
