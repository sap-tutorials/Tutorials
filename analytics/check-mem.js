'use strict';

const path = require('path');
const readline = require('readline');
const colorLog = require('color-log');

const fs = require('./helpers/fs');
const constants = require('./constants');

async function printUnusedFiles({ parentDir, filePath, files, title }) {
  return new Promise((resolve) => {
    const lineReader = readline.createInterface({
      input: fs.createReadStream(filePath),
    });
    let notFound = [...files];

    lineReader.on('line', (line) => {
      const foundEntries = notFound.filter(v => line.includes(v));

      foundEntries.forEach((v) => {
        notFound.splice(notFound.indexOf(v), 1);
      });
    });
    lineReader.on('close', () => {
      resolve(notFound);
    });
  })
    .then((notFound) => {
      let totalSize = 0;
      const report = notFound.reduce((result, fileName) => {
        const { size } = fs.statSync(path.join(parentDir, fileName));
        const kbSize = Number((size / 1024.0).toFixed(3));
        totalSize += kbSize;

        return result + `${fileName} - ${kbSize}Kb\n`;
      }, '');

      if (notFound.length === 0) {
        colorLog.info(`${title}: OK\n`);
        return totalSize;
      }

      colorLog.warn(`${title} - ${totalSize.toFixed(2)}Kb could be saved\nNot used:\n${report}`);
      return totalSize;
    });
}

async function run() {
  const tutorialsPath = path.join(__dirname, '../', constants.tutorialsFolderName);
  const prodTutorials = await fs.readDir(tutorialsPath);
  let sizeToRemove = 0;

  const promises = prodTutorials
    .map(async (tutorialName) => {
      const tutorialFileName = `${tutorialName}.md`;
      const mdFilePath = path.join(tutorialsPath, tutorialName, tutorialFileName);
      const tutorialDir = path.join(tutorialsPath, tutorialName);
      let allFiles = await fs.readDir(tutorialDir);
      allFiles = allFiles.filter(file => file !== tutorialFileName);

      return printUnusedFiles({
        filePath: mdFilePath,
        files: allFiles,
        title: tutorialName,
        parentDir: tutorialDir,
      })
        .then((totalKb) => {
          sizeToRemove += totalKb;
        });
    });

  return Promise
    .all(promises)
    .then(() => {
      colorLog.error((`\n${(sizeToRemove / 1024).toFixed(2)}Mb can be cleared`).toUpperCase());
      process.exit(0)
    })
    .catch((error) => {
      colorLog.error(error);
      process.exit(1);
    });
}

module.exports = run();
