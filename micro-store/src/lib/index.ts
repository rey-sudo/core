import {
  dependencyTreeCategories,
  originalQuestionScheme,
  schemeCategories,
} from "./questions-scheme";
import crypto from "crypto";
import { _ } from "../utils/logger";

const evaluator = (answers: any): any =>
  new Promise((resolve, reject) => {
    try {
      resolve(evaluateData(answers));
    } catch (e) {
      reject(e);
    }
  });

async function evaluateData(answers: any) {
  //GROUP QUESTION BY CATEGORY
  const groupedQuestions = generateCategoryScheme();

  //EVALUATE INDIVIDUAL QUESTION
  originalQuestionScheme.forEach((originalQuestion) =>
    groupedQuestions[schemeCategories[originalQuestion.category_index]].push(
      evaluateQuestion(originalQuestion, answers[originalQuestion.id])
    )
  );

  //DISCARD INAPPLICABLE CATEGORIES
  Object.keys(groupedQuestions).forEach((key) => {
    groupedQuestions[key] = handleDependencyTree(groupedQuestions[key], key);
  });

  const category_scheme = groupedQuestions;

  const category_score = evaluateCategoryScore(groupedQuestions);

  const category_max_score = calculateCategoryMaxScore(groupedQuestions);

  const total_score = calculateTotalScore(category_score);

  const total_max_score = calculateTotalMaxScore(category_max_score);

  const total_percentage = calculateTotalPercentage(
    total_score,
    total_max_score
  );

  const hash = sha256(JSON.stringify(answers));

  return {
    category_scheme,
    category_score,
    category_max_score,
    total_score,
    total_max_score,
    total_percentage,
    hash,
  };
}

/////////////////////////////////////////////////////////////////////
/**
Iterates over each category, calculates the total score of the category by adding the individual score of the questions. */
function evaluateCategoryScore(groupedQuestions: any) {
  const objectSumScore: any = {};

  for (let key in groupedQuestions) {
    let sumScore = 0;

    if (groupedQuestions[key] !== "inapplicable") {
      groupedQuestions[key].forEach((element: any) => {
        sumScore += element.score;
      });

      objectSumScore[key] = sumScore;
    } 
  }

  return objectSumScore;
}
/////////////////////////////////////////////////////////////////////
/**Iterates over each category, calculates the max score of the category by adding the individual max_score of the questions. */
function calculateCategoryMaxScore(groupedQuestions: any) {
  const objectSumScore: any = {};

  for (let key in groupedQuestions) {
    let sumScore = 0;

    if (groupedQuestions[key] !== "inapplicable") {
      groupedQuestions[key].forEach((element: any) => {
        sumScore += element.max_score;
      });

      objectSumScore[key] = sumScore;
    }
  }

  return objectSumScore;
}

/////////////////////////////////////////////////////////////////////
/**Iterates over each category, calculate the total score */
function calculateTotalScore(groupedQuestions: any) {
  let totalScore = 0;

  for (let key in groupedQuestions) {
    totalScore += groupedQuestions[key];
  }

  return totalScore;
}

/////////////////////////////////////////////////////////////////////
/**Iterates over each category, calculate the max score */
function calculateTotalMaxScore(groupedQuestions: any) {
  let totalScore = 0;

  for (let key in groupedQuestions) {
    totalScore += groupedQuestions[key];
  }

  return totalScore;
}

/////////////////////////////////////////////////////////////////////
/**Calculate total percentage */
function calculateTotalPercentage(total_score: any, total_max_score: any) {
  const value = (total_score * 100) / total_max_score;
  return value.toFixed(2);
}

//////////////////////////////////////////////////////////////////////////////

function evaluateQuestion(originalQuestion: any, inputQuestion: any) {
  for (const options of originalQuestion.options) {
    ///////////////////////////////////////////
    const checker = originalQuestion.options.find(
      (option: any) => option.id === inputQuestion.answer
    );

    if (!checker) {
      throw new Error(`Invalid answer value ID-${inputQuestion.id}.`);
    }

    if (inputQuestion.answer === options.id) {
      return {
        id: originalQuestion.id,
        tag: originalQuestion.tag,
        score: options.value,
        max_score: originalQuestion.max_score,
        question: originalQuestion.question,
        answer_id: inputQuestion.answer,
        answer_name: options.name,
        name: originalQuestion.name,
        category: originalQuestion.category,
      };
    }
  }
}

///////////////////////////////////////////////////////////////////////////////
/**If node 0 does not apply, the category is inapplicable.*/
function handleDependencyTree(categoryWrap: any, categoryName: any) {
  const check =
    categoryWrap[0].answer_id === 0 &&
    dependencyTreeCategories.includes(categoryName);

  return check ? "inapplicable" : categoryWrap;
}
///////////////////////////////////////////////////////////////////////////////

function generateCategoryScheme() {
  return schemeCategories.reduce((acc: any, curr: any) => {
    acc[curr] = [];
    return acc;
  }, {});
}
///////////////////////////////////////////////////////////////////////////////

function sha256(message: string): string {
  const hash = crypto.createHash("sha256");
  hash.update(message);
  return hash.digest("hex");
}

export { evaluator };
