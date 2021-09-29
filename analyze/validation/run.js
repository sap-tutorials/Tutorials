'use strict';

const path = require('path');
const colorLog = require('color-log');

const fs = require('../helpers/fs');
const constants = require('../constants');
const validator = require('./validator');
const output = require('../helpers/output');
const csvHelper = require('./csv-helper');

const { validationKeysMap } = constants;
const fieldNames = Object.keys(constants.csvHeaders.validation);

function buildResultStructure() {
  return fieldNames.reduce((result, fieldName) => {
    result[fieldName] = [];

    return result;
  }, {});
}

async function run(pathToQA) {
  const tutorialsPath = path.join(__dirname, '../../', constants.tutorialsFolderName);
  const prodTutorials = await fs.readDir(tutorialsPath);
  const result = buildResultStructure();

  const cache = {
    resultEntries: null,
  };

  return Promise
    .all(prodTutorials
    .map(async (tutorialName) => {
      const mdFilePath = path.join(tutorialsPath, tutorialName, `${tutorialName}.md`);
      const rulesFilePath = path.join(
        pathToQA,
        constants.tutorialsFolderName,
        tutorialName,
        constants.rulesFileName
      );

      try {
        const content = await fs.readFile(mdFilePath, 'utf-8');
        const validationResult = await validator.check({
          content,
          rulesFilePath,
          filePath: mdFilePath,
        });

        Object
          .entries(validationResult)
          .forEach(([checkName, checkResult]) => {
            if (checkResult) {
              result[checkName].push(tutorialName);
            }
          });

        return result;
      } catch (e) {
        result[validationKeysMap.failedToProcess].push(tutorialName);
      }
    }))
    .then(() => {
      cache.resultEntries = Object.entries(result);
      const lengths = cache.resultEntries.map(([, array]) => array.length);
      const maxLength = Math.max(...lengths);
      const rows = [];

      for (let i = 0; i < maxLength; i += 1) {
        const row = cache.resultEntries
          .reduce((row, [fieldName, value]) => {
            if (fieldName !== validationKeysMap.everythingValid) {
              row[fieldName] = value[i] || '';
            }

            return row;
          }, {});

        const isEmpty = Object.values(row).every(value => !value);

        if (!isEmpty) {
          rows.push(row);
        }
      }

      return rows;
    })
    .then((rows) => csvHelper.save(rows))
    .then(() => {
      const stats = cache.resultEntries
        .reduce((temp, [key, items]) => {
          temp[key] = items.length;

          return temp;
        }, {});

      return output({
        stats,
        fileName: csvHelper.fullFilePath,
        type: constants.checkTypes.validation,
      });
    })
    .catch((error) => {
      colorLog.error(error);
      process.exit(1);
    });
}

module.exports = run;
