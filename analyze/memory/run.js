'use strict';

const path = require('path');
const readline = require('readline');

const colorLog = require('color-log');

const fs = require('../helpers/fs');
const output = require('../helpers/output');
const constants = require('../constants');
const csvHelper = require('./csv-helper');

const excludedFiles = ['rules.vr'];
const bigFileSize = 1024; // 1 Mb

async function analyzeFiles({ parentDir, filePath, files }) {
  return new Promise((resolve, reject) => {
    const stream = fs.createReadStream(filePath);

    const lineReader = readline.createInterface({
      input: stream,
    });
    let notFound = [...files];
    let found = [];

    lineReader.on('line', (line) => {
      found = notFound.filter(v => line.includes(v));

      found.forEach((v) => {
        notFound.splice(notFound.indexOf(v), 1);
      });
    });
    stream.on('error', reject);
    lineReader.on('error', reject);
    lineReader.on('close', async () => {
      resolve(notFound);
    });
  })
    .then((notFound) => {
      let bigFilesTotalSize = 0;
      let unusedFilesTotalSize = 0;

      const unusedFiles = notFound.reduce((result, fileName) => {
        const { size } = fs.statSync(path.join(parentDir, fileName));
        const kbSize = Number(size / 1024.0);
        unusedFilesTotalSize += kbSize;
        result.push({ fileName, size: kbSize.toFixed(2) });

        return result;
      }, []);

      const bigFiles = files.reduce((result, fileName) => {
        const { size } = fs.statSync(path.join(parentDir, fileName));
        const kbSize = Number(size / 1024.0);
        if (kbSize > bigFileSize) {
          bigFilesTotalSize += kbSize;
          result.push({ fileName, size: kbSize.toFixed(2) });
        }

        return result;
      }, []);

      return {
        bigFilesTotalSize,
        unusedFilesTotalSize,
        bigFiles,
        unusedFiles,
      };
    });
}

async function run(pathToQA) {
  const tutorialsPath = pathToQA
    ? path.join(pathToQA, constants.tutorialsFolderName)
    : path.resolve('./', constants.tutorialsFolderName);

  let result = [];

  const tutorials = await fs.readDir(tutorialsPath);
  let bigFilesTotalSize = 0;
  let unusedFilesTotalSize = 0;

  const counters = {
    unusedFiles: 0,
    bigFiles: 0,
  };

  const filtered = tutorials
    .filter((tutorial) => {
      // filter off files, we need directories only
      return path.extname(tutorial) === '';
    });

  const promises = filtered
    .map(async (tutorialName) => {
      const tutorialFileName = `${tutorialName}.md`;
      const mdFilePath = path.join(tutorialsPath, tutorialName, tutorialFileName);
      const tutorialDir = path.join(tutorialsPath, tutorialName);
      let allFiles = await fs.readDir(tutorialDir);
      allFiles = allFiles.filter((file) => {
        return file !== tutorialFileName && !excludedFiles.includes(file);
      });

      return analyzeFiles({
        filePath: mdFilePath,
        files: allFiles,
        title: tutorialName,
        parentDir: tutorialDir,
      })
        .then((stats) => {
          const maxLength = Math.max(stats.unusedFiles.length, stats.bigFiles.length);

          if (maxLength > 0) {
            counters.unusedFiles += stats.unusedFiles.length;
            counters.bigFiles += stats.bigFiles.length;

            for (let i = 0; i < maxLength; i++) {
              const unusedFile = (stats.unusedFiles[i] || { fileName: '', size: '' });
              const bigFile = (stats.bigFiles[i] || { fileName: '', size: '' });
              const isLast = i === (maxLength - 1);

              result.push({
                tutorial: tutorialName,
                unusedFile: unusedFile.fileName,
                unusedFileSize: unusedFile.size ? `${unusedFile.size}Kb` : '',
                bigFile: bigFile.fileName,
                bigFileSize: bigFile.size ? `${bigFile.size}Kb` : '',
                error: '',
                total: isLast
                  ? `${(stats.bigFilesTotalSize + stats.unusedFilesTotalSize).toFixed(2)}Kb`
                  : '',
              });
            }
          }

          bigFilesTotalSize += stats.bigFilesTotalSize;
          unusedFilesTotalSize += stats.unusedFilesTotalSize;

          return result;
        })
        .catch((error) => {
          result.push({
            tutorial: tutorialName,
            unusedFile: 'UNKNOWN',
            unusedFileSize: 'UNKNOWN',
            bigFile: 'UNKNOWN',
            bigFileSize: 'UNKNOWN',
            error: error.message,
            total: '',
          });
          colorLog.error(`${tutorialName} FAILED:\n${error.message}`);
        });
    });

  return Promise
    .all(promises)
    .then(() => result.sort((a, b) => a.tutorial.localeCompare(b.tutorial)))
    .then(() => {
      const sizeMb = `${Number((bigFilesTotalSize + unusedFilesTotalSize) / 1024).toFixed(2)}Mb`;
      result.push({
        tutorial: 'GRAND TOTAL',
        unusedFile: `${counters.unusedFiles} unused files (${( unusedFilesTotalSize / 1024).toFixed(2)})Mb`,
        bigFile: `${counters.bigFiles} big files (${(bigFilesTotalSize / 1024).toFixed(2)})Mb`,
        unusedFileSize: '',
        bigFileSize: '---',
        total: sizeMb,
        error: '---',
      });
      csvHelper.save(result, pathToQA ? 'qa' : 'prod');

      const stats = {
        total: sizeMb,
        unusedFile: counters.unusedFiles,
        bigFile: counters.bigFiles,
      };

      output({
        stats,
        fileName: csvHelper.fileName,
        type: constants.checkTypes.memoryUsage,
      });
    });
}

module.exports = run;
