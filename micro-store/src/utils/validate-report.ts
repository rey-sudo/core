import { _ } from "./logger";

import Ajv from "ajv";

const ajv = new Ajv();

const chartDonutScheme = {
  type: "object",
  properties: {
    labels: {
      type: "array",
      minItems: 0,
      maxItems: 100,
      items: {
        type: "string",
        pattern: "^[a-zA-Z0-9 ]+$",
        minLength: 1,
        maxLength: 100,
      },
    },
    datasets: {
      type: "array",
      items: {
        type: "object",
        properties: {
          data: {
            type: "array",
            minItems: 0,
            maxItems: 100,
            items: {
              type: "integer",
              minimum: 0,
            },
          },
          backgroundColor: {
            type: "array",
            items: { type: "string" },
            const: [
              "#3b82f6",
              "#27aeef",
              "#3bd0b1",
              "#6cecd4",
              "#ffcc5d",
              "#faae04",
              "#fa6367",
              "#fdc1c2",
              "#8a7cf8",
              "#adb9ca",
              "#55c6e0",
              "#1967b3",
              "#ec4899",
              "#b33dc6",
              "#2ea78e",
              "#ea5545",
            ],
          },
          hoverBackgroundColor: {
            type: "array",
            items: { type: "string" },
            const: [
              "#3b82f6",
              "#27aeef",
              "#3bd0b1",
              "#6cecd4",
              "#ffcc5d",
              "#faae04",
              "#fa6367",
              "#fdc1c2",
              "#8a7cf8",
              "#adb9ca",
              "#55c6e0",
              "#1967b3",
              "#ec4899",
              "#b33dc6",
              "#2ea78e",
              "#ea5545",
            ],
          },
          borderWidth: { type: "number", const: 1 },
        },
        required: [
          "data",
          "backgroundColor",
          "hoverBackgroundColor",
          "borderWidth",
        ],
      },
    },
  },
  required: ["labels", "datasets"],
};

const chartDonutSchemeValidator = ajv.compile(chartDonutScheme);

export const validateReportInput = (input: any) => {
  const inputType = typeof input;

  const validTypes = [null, "number", "object"].includes(inputType);

  if (!validTypes) {
    return false;
  }

  if (input === null) {
    return true;
  }

  if (inputType === "number") {
    return Number.isInteger(input) && input >= 0;
  }

  if (inputType === "object") {
    return chartDonutSchemeValidator(input);
  }
};
