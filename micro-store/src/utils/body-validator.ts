import { body, check, param } from "express-validator";
import { validateReportInput } from "./validate-report";

const CREATE_PRODUCT = [
  body("name")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("max length"),

  body("space_url")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 300 })
    .withMessage("max length"),

  body("image.small")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 300 })
    .withMessage("max length"),

  body("image.medium")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 300 })
    .withMessage("max length"),

  body("image.large")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 300 })
    .withMessage("max length"),


    body("payment_type")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("max length"),

  body("shipping_tax")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isBoolean()
    .withMessage("must be boolean")
    .bail(),

  body("shipping_label")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("max length"),

  body("stock_supply")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number")
    .bail()
    .isInt({ min: 0 })
    .withMessage("must be natural"),

  body("price")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number")
    .bail()
    .isInt({ min: 0 })
    .withMessage("must be natural"),

  body("price_diff")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number")
    .bail()
    .isInt({ min: 0 })
    .withMessage("must be natural"),

  body("discount_label")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("max length"),

  body("discount_color")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 7 })
    .withMessage("max length"),

  body("theme.title")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("max length"),

  body("theme.subtitle")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("max length"),

  body("theme.price")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("max length"),

  body("theme.background.pageOne.color")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 7 })
    .withMessage("max length"),

  body("theme.background.pageOne.mask")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 200 })
    .withMessage("max length"),

  body("theme.background.pageTwo.color")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 7 })
    .withMessage("max length"),

  body("theme.background.pageTwo.mask")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 200 })
    .withMessage("max length"),

  body("theme.background.pageThree.color")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 7 })
    .withMessage("max length"),

  body("theme.background.pageThree.mask")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .isString()
    .withMessage("must be string")
    .bail()
    .trim()
    .isLength({ min: 1, max: 200 })
    .withMessage("max length"),

  body("theme.slider_images")
    .notEmpty()
    .withMessage("must be provided")
    .bail()
    .custom((value) =>
      value.every((item: any) => item.url && typeof item.url === "string")
    )
    .withMessage("invalid format")
];










const GET_AUDIT_REPORT = [
  param("report_pid")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isString()
    .withMessage("must be string.")
    .bail()
    .trim()
    .isLength({ min: 1, max: 30 }),

];

const GET_ALL_REPORTS = [
  param("project_name")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isString()
    .trim()
    .isLength({ min: 1, max: 100 }),
];

const CREATE_AUDITOR = [
  body("email")
    .notEmpty()
    .withMessage("email must be provided.")
    .bail()
    .isEmail()
    .withMessage("email must be valid.")
    .bail()
    .normalizeEmail({
      all_lowercase: true,
      gmail_lowercase: true,
      gmail_remove_dots: false,
      gmail_remove_subaddress: true,
      gmail_convert_googlemaildotcom: true,
      outlookdotcom_lowercase: true,
      outlookdotcom_remove_subaddress: true,
      yahoo_lowercase: true,
      yahoo_remove_subaddress: true,
      icloud_lowercase: true,
      icloud_remove_subaddress: true,
    }),

  body("password")
    .notEmpty()
    .withMessage("password must be provided.")
    .bail()
    .isString()
    .withMessage("password must be valid.")
    .bail()
    .isLength({ min: 8, max: 30 })
    .withMessage("password must be between 8 and 30 characters.")
    .bail()
    .matches(/(?=.*\d)(?=.*[a-z])(?=.*[A-Z])/)
    .withMessage("password must contain a number, lowercase and uppercase."),

  body("username")
    .notEmpty()
    .withMessage("username must be provided.")
    .bail()
    .isString()
    .withMessage("username must be string.")
    .isAlphanumeric()
    .withMessage("username must contain only numbers and letters.")
    .trim()
    .isLength({ min: 8, max: 15 })
    .withMessage("username must be between 8 and 15 characters."),

  body("wallet")
    .notEmpty()
    .withMessage("wallet must be provided.")
    .bail()
    .isString()
    .withMessage("wallet must be string.")
    .isAlphanumeric()
    .withMessage("wallet must be alphanumeric.")
    .trim()
    .isLength({ min: 8, max: 300 })
    .withMessage("wallet max length."),

  body("contact")
    .notEmpty()
    .withMessage("contact must be provided.")
    .bail()
    .isString()
    .withMessage("contact must be string.")
    .bail()
    .trim()
    .isLength({ min: 8, max: 300 })
    .withMessage("contact max length.")
    .bail()
    .isURL({
      protocols: ["https"],
      require_tld: true,
      require_protocol: true,
      require_host: true,
      require_port: false,
      require_valid_protocol: true,
      allow_underscores: true,
      allow_trailing_dot: false,
      allow_protocol_relative_urls: false,
      allow_fragments: true,
      allow_query_components: true,
      disallow_auth: false,
      validate_length: true,
    })
    .blacklist(`$*^(){}[]:;|&'"`),
];

const LOGIN_AUDITOR = [
  body("email")
    .notEmpty()
    .withMessage("email must be provided.")
    .bail()
    .isEmail()
    .withMessage("email must be valid.")
    .bail()
    .normalizeEmail({
      all_lowercase: true,
      gmail_lowercase: true,
      gmail_remove_dots: false,
      gmail_remove_subaddress: true,
      gmail_convert_googlemaildotcom: true,
      outlookdotcom_lowercase: true,
      outlookdotcom_remove_subaddress: true,
      yahoo_lowercase: true,
      yahoo_remove_subaddress: true,
      icloud_lowercase: true,
      icloud_remove_subaddress: true,
    }),

  body("password")
    .notEmpty()
    .withMessage("password must be provided.")
    .bail()
    .isString()
    .withMessage("invalid credentials.")
    .bail()
    .isLength({ min: 8, max: 30 })
    .withMessage("invalid credentials.")
    .bail()
    .matches(/(?=.*\d)(?=.*[a-z])(?=.*[A-Z])/)
    .withMessage("invalid credentials."),
];

const CREATE_ROUND = [
  body("round")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isString()
    .withMessage("must be string.")
    .bail()
    .isAlphanumeric()
    .withMessage("must be alphanumeric.")
    .bail()
    .escape()
    .trim(),

  check("budget")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number.")
    .bail()
    .isInt({ min: 1 })
    .withMessage("must be natural."),

  check("reward")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number.")
    .bail()
    .isInt({ min: 1 })
    .withMessage("must be natural."),
];

const START_SELECTION = [
  check("delay")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be integer.")
    .bail()
    .isInt({ min: 1 })
    .withMessage("must be natural."),
];

const START_GOVERNANCE = [
  check("delay")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be integer.")
    .bail()
    .isInt({ min: 1 })
    .withMessage("must be natural."),
];

const START_AUDITING = [
  check("stage_delay")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be integer.")
    .bail()
    .isInt({ min: 1 })
    .withMessage("must be natural."),

  check("draft_delay")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be integer.")
    .bail()
    .isInt({ min: 1 })
    .withMessage("must be natural."),

  check("revision_delay")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be integer.")
    .bail()
    .isInt({ min: 1 })
    .withMessage("must be natural."),
];

const CREATE_GROUPS = [
  body("projects")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isArray({ min: 1 })
    .withMessage("must be array.")
    .bail()
    .custom((item) =>
      item.every((e: any) => typeof e === "string" && e.length < 150)
    )
    .withMessage("max length."),
];

const CREATE_DAOVOTES = [
  body("projects")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isArray({ min: 1 })
    .withMessage("must be array.")
    .bail()
    .custom((item) =>
      item.every((e: any) => typeof e === "string" && e.length < 150)
    )
    .withMessage("max length."),

  body("daovotes")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isArray({ min: 1 })
    .withMessage("must be array.")
    .bail()
    .custom((item) => item.every((e: any) => Number.isInteger(e)))
    .withMessage("must be integers.")
    .bail()
    .isInt({ min: 0 })
    .withMessage("must be natural."),
];

const CREATE_REPORT = [
  body("data")
    .isArray({ min: 33, max: 33 })
    .withMessage("invalid scheme.")
    .bail()
    .custom(
      (item) => new Set(item.map((obj: any) => obj.id)).size === item.length
    )
    .withMessage("invalid scheme."),

  check("data.*.id")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number.")
    .bail()
    .isInt({ min: 0, max: 32 })
    .withMessage("invalid scheme."),

  check("data.*.answer")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number.")
    .bail()
    .isInt({ min: 0, max: 4 })
    .withMessage("invalid scheme."),

  check("data.*.input")
    .exists({ checkNull: false })
    .withMessage("must be provided.")
    .bail()
    .custom((item) => validateReportInput(item))
    .withMessage("Avoid especial symbols"),

  check("data.*.textarea")
    .exists({ checkFalsy: false })
    .withMessage("must be provided.")
    .bail()
    .isString()
    .withMessage("must be string.")
    .bail()
    .escape()
    .trim(),

  body("notes.answer")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isIn(["Positive", "Negative", "Neutral"]),

  body("notes.textarea")
    .exists({ checkFalsy: false })
    .withMessage("must be provided.")
    .bail()
    .isString()
    .withMessage("must be string.")
    .bail()
    .escape()
    .trim(),
];

const CREATE_REVIEW = [
  body("data")
    .isArray({ min: 33, max: 33 })
    .withMessage("invalid scheme.")
    .bail()
    .custom(
      (item) => new Set(item.map((obj: any) => obj.id)).size === item.length
    )
    .withMessage("invalid scheme."),

  check("data.*.id")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number.")
    .bail()
    .isInt({ min: 0, max: 32 })
    .withMessage("invalid scheme."),

  check("data.*.answer")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .custom((item) => Number.isInteger(item))
    .withMessage("must be number.")
    .bail()
    .isInt({ min: 0, max: 4 })
    .withMessage("invalid scheme."),

  check("data.*.input")
    .exists({ checkNull: false })
    .withMessage("must be provided.")
    .bail()
    .custom((item) => validateReportInput(item))
    .withMessage("chart or input error."),

  check("data.*.textarea")
    .exists({ checkFalsy: false })
    .withMessage("must be provided.")
    .bail()
    .isString()
    .withMessage("must be string.")
    .bail()
    .escape()
    .trim(),

  body("notes.answer")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isIn(["Positive", "Negative", "Neutral"]),

  body("notes.textarea")
    .exists({ checkFalsy: false })
    .withMessage("must be provided.")
    .bail()
    .isString()
    .withMessage("must be string.")
    .bail()
    .escape()
    .trim(),
];

const CREATE_PREVOTE = [
  check("project_pid")
    .notEmpty()
    .withMessage("must be provided.")
    .bail()
    .isString()
    .withMessage("must be string.")
    .bail()
    .isLength({ max: 30 })
    .withMessage("max length."),
];

export {
  CREATE_PRODUCT,
  CREATE_AUDITOR,
  LOGIN_AUDITOR,
  CREATE_REPORT,
  CREATE_REVIEW,
  CREATE_ROUND,
  START_SELECTION,
  START_GOVERNANCE,
  CREATE_GROUPS,
  START_AUDITING,
  CREATE_PREVOTE,
  GET_AUDIT_REPORT,
  CREATE_DAOVOTES,
  GET_ALL_REPORTS,
};
