const __ = require("lodash");

/**
 * Calculates the number of rewards
 * 
 * @param {number} budget
 * @param {number} reward 
 * @returns budget / reward = posibleRewards
 */
const calculatePosibleRewards = (budget: number, reward: number) => {
  return Math.floor(budget / reward);
};

/**
 * Calculates the number of auditors required according to the number of projects.
 * 
 * @param {number} projectsLength
 * @param {number} auditorPerProject
 * @returns projectsLength * auditorPerProject = requiredAuditors
 */
const calculateRequiredAuditors = (
  projectsLength: number,
  auditorPerProject: number
) => {
  return projectsLength * auditorPerProject;
};

/**
 * takes a list of indices and performs permutations at each location using the random() function.
 * @param activeAuditors 
 * @returns shuffled list of indices
 */
const randomAuditorsOrdering = (activeAuditors: any) => {
  let array = activeAuditors.map((e: { auditor_pid: string }) => e.auditor_pid);

  let m = array.length;
  let temp;
  let random;

  while (m) {
    random = Math.floor(Math.random() * m--);
    temp = array[m];
    array[m] = array[random];
    array[random] = temp;
  }

  return array;
};
/**
 * takes a list of indices and extracts a required sample.
 * 
 * @param sortedAuditors 
 * @param requiredAuditors 
 * @returns sample list of indices
 */
const auditorsSampleSelection = (
  sortedAuditors: string[],
  requiredAuditors: number
) => __.sampleSize(sortedAuditors, requiredAuditors);


/**
 * takes a list of indices and groups them according to size.
 * 
 * @param {number} data 
 * @param {number} size 
 * @returns returns a two-dimensional array
 */
const auditorGroupingByNum = (data: any[], size: number) => {
  let array = data;

  return array.reduce((array_, item, index) => {
    const chunkIndex = Math.floor(index / size);

    if (!array_[chunkIndex]) array_[chunkIndex] = [];

    array_[chunkIndex].push(item);

    return array_;
  }, []);
};

/**
 * creates a new array with formatted schemas.
 * 
 * @param {string}auditPid 
 * @param {string[][]}groupedAuditors 
 * @param {string[]}projectsInput 
 * @returns Group[]
 */
const formatGroupScheme = (
  auditPid: string,
  groupedAuditors: string[][],
  projectsInput: string[]
) => {
  let auditors = groupedAuditors;

  let projects = projectsInput;

  const groups = auditors.map((e, i) => {
    return {
      group_name: "A" + i,
      round_pid: auditPid,
      project_pid: projects[i],
      reviewer_pid: e[0],
      auditor_pid: e[1],
    };
  });

  return groups;
};

const createDaovotesSimulation = (
  round_pid: string,
  projects: string[],
  daovotes: number[]
) => {
  let result: any = [];

  daovotes.forEach((e, index) => {
    for (let i = 0; i < e; i++) {
      result.push({ round_pid: round_pid, project_pid: projects[index] });
    }
  });

  return result;
};

export {
  randomAuditorsOrdering,
  auditorGroupingByNum,
  formatGroupScheme,
  createDaovotesSimulation,
  auditorsSampleSelection,
  calculatePosibleRewards,
  calculateRequiredAuditors,
};
