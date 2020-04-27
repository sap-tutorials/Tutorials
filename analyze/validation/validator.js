'use strict';

const fs = require('../helpers/fs');
const { regexp } = require('../../test-tool/src/constants');
const { common } = require('../../test-tool/src/utils');
const { csvHeaders, validationKeysMap } = require('../constants');

const {
  validation: {
    accordions,
    validate,
    validate_vr: validateVr,
    done,
    codeBlock,
    codeLine,
  },
} = regexp;

const fieldNames = Object.keys(csvHeaders.validation);

function buildResultStructure() {
  return fieldNames.reduce((result, fieldName) => {
    result[fieldName] =  false;

    return result;
  }, {});
}

module.exports = {
  async check({ filePath, content, rulesFilePath }) {
    const result = buildResultStructure();

    const accordionMatches = this.extractAccordions(content);
    let hasValidate;
    let noValidateOrDone;

    if (accordionMatches) {
      hasValidate = this.hasValidate(accordionMatches);
      noValidateOrDone = this.hasNoValidateOrDone(accordionMatches);
    } else {
      result[validationKeysMap.noSteps] = true;
      return result;
    }

    const vrFileExists = await this.rulesFileExists(rulesFilePath);

    if (!hasValidate && !noValidateOrDone) {
      result[validationKeysMap.onlyDone] = true;
    } else {
      const validationMatches = this.extractValidationMatches(accordionMatches);

      const duplicates = common.findDuplicates(validationMatches);

      if (duplicates.length > 0) {
        result[validationKeysMap.duplicateRuleDefinition] = true;
      }

      if (vrFileExists) {
        const uniqueMatches = new Set(validationMatches);

        const { notDefinedValidate, duplicateRuleDefinition } = this.checkValidates({
          rulesFilePath,
          matches: uniqueMatches,
        });

        result[validationKeysMap.notDefinedValidate] = notDefinedValidate;
        result[validationKeysMap.duplicateRuleDefinition] = duplicateRuleDefinition;
      } else if (!vrFileExists && validationMatches.length > 0){
        result[validationKeysMap.noRulesFile] = true;
      }
    }
    if (noValidateOrDone) {
      result[validationKeysMap.stepWithNoDoneValidate] = true;
    }

    result[validationKeysMap.everythingValid] = Object.values(result).every(value => !value);

    return result;
  },

  hasValidate(accordionMatches) {
    return accordionMatches.some(step => step.match(validate));
  },

  extractAccordions(content) {
    const clearContent = this.removeCodeBlocks(content);

    return clearContent.match(accordions)
  },

  extractValidationMatches(accordionMatches) {
    return accordionMatches
      .map((step) => {
        const matches = step.match(validate);

        if (matches) {
          return matches.map(match => match.replace('[VALIDATE_', ''));
        }

        return matches;
      })
      .filter(match => !!match)
      .reduce((arr, matches) => arr.concat(matches), []);
  },

  removeCodeBlocks(content) {
    return content.replace(codeBlock, '').replace(codeLine, '');
  },

  hasNoValidateOrDone(accordionMatches) {
    return accordionMatches.every((step) => !step.match(validate) && !step.match(done));
  },

  rulesFileExists(rulesFilePath) {
    return fs.access(rulesFilePath)
      .then(() => true)
      .catch(() => false)
  },

  async checkValidates({ matches, rulesFilePath }) {
    const result = {};
    const vrFile = await fs.readFile(rulesFilePath, 'utf-8');

    for (let validationNumber of matches) {
      const validateBlock = vrFile.match(validateVr(validationNumber));

      if (result.duplicateRuleDefinition && result.notDefinedValidate) {
        break;
      }

      if (!validateBlock && !result[validationKeysMap.notDefinedValidate]) {
        result[validationKeysMap.notDefinedValidate] = true;
      } else if (validateBlock && validateBlock.length > 1 && !result[validationKeysMap.duplicateRuleDefinition]) {
        result[validationKeysMap.duplicateRuleDefinition] = true;
      }
    }

    return result;
  },
};
