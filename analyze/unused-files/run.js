'use strict';

const path = require('path');
const readline = require('readline');

const fs = require('../helpers/fs');
const constants = require('../constants');

const csvHelper = require('./csv-helper');
const output = require('../helpers/output');

async function getUnusedFiles({ parentDir, filePath, files }) {
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
      const results = notFound.reduce((result, fileName) => {
        const { size } = fs.statSync(path.join(parentDir, fileName));
        const kbSize = Number((size / 1024.0));
        totalSize += kbSize;
        result.push([fileName, kbSize.toFixed(2)]);

        return result;
      }, []);

      if (notFound.length === 0) {
        return { totalSize: 0, files: [] };
      }

      return {
        totalSize,
        files: results,
      };
    });
}

async function run(pathToQA) {
  const tutorialsPath = pathToQA
    ? path.join(pathToQA, constants.tutorialsFolderName)
    : path.join(__dirname, '../../', constants.tutorialsFolderName);
  const result = [];

  const tutorials = await fs.readDir(tutorialsPath);
  let sizeToRemove = 0;

  const promises = tutorials
    .map(async (tutorialName) => {
      const tutorialFileName = `${tutorialName}.md`;
      const mdFilePath = path.join(tutorialsPath, tutorialName, tutorialFileName);
      const tutorialDir = path.join(tutorialsPath, tutorialName);
      let allFiles = await fs.readDir(tutorialDir);
      allFiles = allFiles.filter(file => file !== tutorialFileName);

      return getUnusedFiles({
        filePath: mdFilePath,
        files: allFiles,
        title: tutorialName,
        parentDir: tutorialDir,
      })
        .then((stats) => {
          if (stats.files.length > 0) {
            stats.files.forEach(([fileName, size]) => {
              result.push({ tutorial: tutorialName, file: fileName, size: `${size}Kb` });
            });

            result.push({ tutorial: 'TOTAL', file: '', size: `${stats.totalSize.toFixed(2)}Kb`});
          }
          sizeToRemove += stats.totalSize;
        });
    });

  return Promise
    .all(promises)
    .then(() => {
      debugger;
      result.push({ tutorial: 'GRAND TOTAL', file: '', size: `${Number(sizeToRemove / 1024).toFixed(2)}Mb`});
      csvHelper.save(result);
    });
}

module.exports = run();
