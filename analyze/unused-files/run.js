'use strict';

const path = require('path');
const readline = require('readline');

const colorLog = require('color-log');

const fs = require('../helpers/fs');
const constants = require('../constants');

const csvHelper = require('./csv-helper');

const excludedFiles = ['rules.vr'];

async function getUnusedFiles({ parentDir, filePath, files }) {
  return new Promise((resolve, reject) => {
    const stream = fs.createReadStream(filePath);

    const lineReader = readline.createInterface({
      input: stream,
    });
    let notFound = [...files];

    lineReader.on('line', (line) => {
      const foundEntries = notFound.filter(v => line.includes(v));

      foundEntries.forEach((v) => {
        notFound.splice(notFound.indexOf(v), 1);
      });
    });
    stream.on('error', reject);
    lineReader.on('error', reject);
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
    .filter((tutorial) => {
      // filter off files, we need directories only
      return path.extname(tutorial) === '';
    })
    .map(async (tutorialName) => {
      const tutorialFileName = `${tutorialName}.md`;
      const mdFilePath = path.join(tutorialsPath, tutorialName, tutorialFileName);
      const tutorialDir = path.join(tutorialsPath, tutorialName);
      let allFiles = await fs.readDir(tutorialDir);
      allFiles = allFiles.filter((file) => {
        return file !== tutorialFileName && !excludedFiles.includes(file);
      });

      return getUnusedFiles({
        filePath: mdFilePath,
        files: allFiles,
        title: tutorialName,
        parentDir: tutorialDir,
      })
        .then((stats) => {
          if (stats.files.length > 0) {
            stats.files.forEach(([fileName, size]) => {
              result.push({
                tutorial: tutorialName,
                file: fileName,
                size: `${size}Kb`,
                error: '',
              });
            });

            result.push({
              tutorial: 'TOTAL',
              file: '',
              size: `${stats.totalSize.toFixed(2)}Kb`,
              error: '',
            });
          }
          sizeToRemove += stats.totalSize;
        })
        .catch((error) => {
          result.push({
            tutorial: tutorialName,
            file: 'UNKNOWN',
            size: 'UNKNOWN',
            error: error.message,
          });
          colorLog.error(`${tutorialName} FAILED:\n${error.message}`);
        });
    });

  return Promise
    .all(promises)
    .then(() => {
      result.push({ tutorial: 'GRAND TOTAL', file: '', size: `${Number(sizeToRemove / 1024).toFixed(2)}Mb` });
      csvHelper.save(result, pathToQA ? 'qa':  'prod');
    });
}

module.exports = run();
