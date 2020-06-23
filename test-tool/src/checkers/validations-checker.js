const path = require('path');
const fs = require('fs');

const { regexp } = require('../constants');
const { common } = require('../utils');

module.exports = {
  check: (filePath, content, isProduction) => {
    const err = [];
    const {
      validation: {
        auto_validation: autoValidation,
        accordions,
        validate,
        validate_vr: validateVr,
        done,
        codeBlock,
        inlineCodeBlock,
        codeLine,
        messages,
      },
    } = regexp;
    const autoValidationMatch = content.match(autoValidation);
    let clearContent = content
      .replace(codeBlock, ' ')
      .replace(inlineCodeBlock, ' ');
    clearContent = (clearContent.replace(codeLine, ' '));
    const accordionMatches = clearContent.match(accordions);
    let validationFormExists;
    let accordionsWOAnyValidation;

    if (accordionMatches) {
      validationFormExists = accordionMatches.some(step => step.match(validate));
      accordionsWOAnyValidation = accordionMatches.map((step, index) => ({
        step,
        id: index + 1,
      })).filter(({ step }) => !step.match(validate) && !step.match(done));
    }
    if (autoValidationMatch) {
      const [, value] = autoValidationMatch;
      const dirPath = path.dirname(filePath);
      const rulesPath = path.join(dirPath, 'rules.vr');
      const vrFileExists = fs.existsSync(rulesPath);
      if (isProduction) {
        if (vrFileExists) {
          err.push(messages.production.rules_vr);
        }
      }
      // eslint-disable-next-line eqeqeq
      if (value && (value == 'true' || value == 'false')) {
        if (!validationFormExists) {
          err.push(messages.validate_restrictions.at_least_one);
        } else {
          const validationsMatch = accordionMatches
            .map((step) => {
              const matches = step.match(validate);

              if (matches) {
                return matches.map(match => match.replace('[VALIDATE_', ''));
              }

              return matches;
            })
            .filter(match => match)
            .reduce((arr, matches) => arr.concat(matches), []);
          const duplicates = common.findDuplicates(validationsMatch);
          duplicates
            .forEach(duplicate => err.push(messages.validate_restrictions.duplicates(duplicate)));

          if (!isProduction) {
            if (vrFileExists) {
              const vrFile = fs.readFileSync(rulesPath, 'utf-8');
              (new Set(validationsMatch)).forEach((validationNumber) => {
                const validateBlock = vrFile.match(validateVr(validationNumber));
                if (!validateBlock) {
                  err.push(messages.validate_restrictions.not_defined(validationNumber));
                } else if (validateBlock.length > 1) {
                  err.push(messages.validate_restrictions.duplicates_vr(validationNumber));
                }
              });
            } else {
              err.push(messages.rules_vr_missed);
            }
          }
        }
        if (accordionsWOAnyValidation) {
          accordionsWOAnyValidation.forEach(
            ({ id }) => err.push(messages.validate_restrictions.missed_validation(id))
          );
        }
      }
    } else if (validationFormExists) {
      err.push(messages.validate_wo_property);
    }
    return err;
  },
};

