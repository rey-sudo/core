const { removeBackground } = require("@imgly/background-removal-node");
const fs = require("fs");
const path = require("path");

const FOLDER_PATH = "files";

async function main() {
  fs.readdir(FOLDER_PATH, (err, files) => {
    if (err) {
      console.error("Error reading folder:", err);
      return;
    }

    files.forEach((file) => {
      const filePath = path.join(FOLDER_PATH, file);

      fs.stat(filePath, async (err, stats) => {
        if (err) {
          console.error("Error getting file stats:", err);
          return;
        }

        if (stats.isFile()) {
          await prepareImage(filePath, file);
        }
      });
    });
  });
}

async function prepareImage(filePath, file) {
  const whitelist = ["png"];

  const format = file.split(".")[1];
  const name = file.split(".")[0];

  if (whitelist.includes(format)) {
    console.log(file);
    await processImage(filePath, name);
  }
}

async function processImage(filePath, name) {
  const blob = await removeBackground(filePath, { debug: false });
  const buffer = await blob.arrayBuffer();
  try {
    await fs.promises.mkdir("output", { recursive: true });
    await fs.promises.writeFile(`output/${name}-transparent.png`, Buffer.from(buffer));
  } catch (error) {
    console.error(error);
  }
}

main();
