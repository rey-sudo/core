import { body } from "express-validator";

export const signupBodyRules = [
  body("email")
    .notEmpty()
    .withMessage("(Email) must be provided.")
    .isEmail()
    .withMessage("(Email) must be valid."),

  body("password")
    .notEmpty()
    .withMessage("(Password) must be provided")
    .isString()
    .withMessage("(Password) must be string")
    .trim()
    .isLength({ min: 8, max: 30 })
    .withMessage("(Password) must be between 6 and 30 characters.")
    .matches(/(?=.*\d)(?=.*[a-z])(?=.*[A-Z])/)
    .withMessage("(Password) must contain a number, uppercase and lowercase."),

  body("username")
    .notEmpty()
    .withMessage("(Username) must be provided")
    .isString()
    .withMessage("(Username) must be string")
    .trim()
    .isLength({ min: 8, max: 15 })
    .withMessage("(Username) must be between 6 and 15 characters.")
    .isAlphanumeric()
    .withMessage("(Username) must contain only numbers and letters."),
];

export const signInBodyRules = [
  body("email")
    .notEmpty()
    .withMessage("Email must be provided.")
    .isString()
    .withMessage("Email must be string")
    .isEmail()
    .withMessage("Invalid credentials"),

  body("password")
    .notEmpty()
    .withMessage("(Password) must be provided")
    .isString()
    .withMessage("(Password) must be string")
    .trim()
    .notEmpty()
    .withMessage("Password must be provided")
    .isLength({ min: 8, max: 30 })
    .withMessage("Invalid password"),
];

export const recoveryBodyRules = [
  body("email")
    .notEmpty()
    .withMessage("Email must be provided.")
    .isEmail()
    .withMessage("Email must be valid."),
];

export const passResetBodyRules = [
  body("password")
    .notEmpty()
    .withMessage("(Password) must be provided")
    .isString()
    .withMessage("(Password) must be string")
    .trim()
    .isLength({ min: 8, max: 30 })
    .withMessage("(Password) must be between 6 and 30 characters.")
    .matches(/(?=.*\d)(?=.*[a-z])(?=.*[A-Z])/)
    .withMessage("(Password) must contain a number, uppercase and lowercase."),

  body("rpassword")
    .notEmpty()
    .withMessage("(Password) must be provided")
    .isString()
    .withMessage("(Password) must be string")
    .trim()
    .isLength({ min: 8, max: 30 })
    .withMessage("(Password) must be between 6 and 30 characters.")
    .matches(/(?=.*\d)(?=.*[a-z])(?=.*[A-Z])/)
    .withMessage("(Password) must contain a number, uppercase and lowercase."),

  body("token").trim().notEmpty().withMessage("Token must be provided"),
];

export const create_project = [
  body("name")
    .notEmpty()
    .withMessage("(name) must be provided.")
    .isString()
    .withMessage("(name) must be string")
    .isLowercase()
    .withMessage("(name) must be lowercase")
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("(name) must be between 1 and  100 characters."),

  body("category")
    .notEmpty()
    .withMessage("(category) must be provided.")
    .isString()
    .withMessage("(category) must be string")
    .isLowercase()
    .withMessage("(category) must be lowercase")
    .trim()
    .isLength({ min: 1, max: 20 })
    .withMessage("(category) must be between 1 and  20 characters."),

  body("description")
    .notEmpty()
    .withMessage("(description) must be provided.")
    .isString()
    .withMessage("(description) must be string.")
    .isLowercase()
    .withMessage("(description) must be lowercase.")
    .trim()
    .isLength({ min: 1, max: 130 })
    .withMessage("(description) must be between 1 and 130 characters."),

  body("symbol")
    .notEmpty()
    .withMessage("(symbol) must be provided.")
    .isString()
    .withMessage("(symbol) must be string.")
    .isLowercase()
    .withMessage("(symbol) must be lowercase.")
    .trim()
    .isLength({ min: 1, max: 10 })
    .withMessage("(symbol) must be between 1 and 10 characters."),

  body("logo")
    .notEmpty()
    .withMessage("(logo) must be provided.")
    .isString()
    .withMessage("(logo) must be string."),

  body("tags")
    .notEmpty()
    .withMessage("(tags) must be provided.")
    .isArray({ max: 5 })
    .withMessage("(tags) must be array."),

  body("links")
    .notEmpty()
    .withMessage("(links) must be provided.")
    .isObject()
    .withMessage("(links) must be object."),
];

export const update_project = [
  body("name")
    .notEmpty()
    .withMessage("(name) must be provided.")
    .isString()
    .withMessage("(name) must be string")
    .isLowercase()
    .withMessage("(name) must be lowercase")
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("(name) must be between 1 and  100 characters."),

  body("category")
    .notEmpty()
    .withMessage("(category) must be provided.")
    .isString()
    .withMessage("(category) must be string")
    .isLowercase()
    .withMessage("(category) must be lowercase")
    .trim()
    .isLength({ min: 1, max: 20 })
    .withMessage("(category) must be between 1 and  20 characters."),

  body("description")
    .notEmpty()
    .withMessage("(description) must be provided.")
    .isString()
    .withMessage("(description) must be string.")
    .isLowercase()
    .withMessage("(description) must be lowercase.")
    .trim()
    .isLength({ min: 1, max: 130 })
    .withMessage("(description) must be between 1 and 130 characters."),

  body("symbol")
    .notEmpty()
    .withMessage("(symbol) must be provided.")
    .isString()
    .withMessage("(symbol) must be string.")
    .isLowercase()
    .withMessage("(symbol) must be lowercase.")
    .trim()
    .isLength({ min: 1, max: 10 })
    .withMessage("(symbol) must be between 1 and 10 characters."),

  body("logo")
    .notEmpty()
    .withMessage("(logo) must be provided.")
    .isString()
    .withMessage("(logo) must be string."),

  body("tags")
    .notEmpty()
    .withMessage("(tags) must be provided.")
    .isArray({ max: 5 })
    .withMessage("(tags) must be array."),

  body("links")
    .notEmpty()
    .withMessage("(links) must be provided.")
    .isObject()
    .withMessage("(links) must be object."),
];

export const delete_project = [
  body("name")
    .notEmpty()
    .withMessage("(name) must be provided.")
    .isString()
    .withMessage("(name) must be string")
    .trim()
    .isLength({ min: 1, max: 100 })
    .withMessage("(name) must be between 1 and  100 characters."),
];

