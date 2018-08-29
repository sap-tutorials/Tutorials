const path = require('path');
const fs = require('fs');

const { regexp } = require('../constants');
const { common } = require('../utils');

module.exports = {
    check: (filePath, content, isProduction) => {
        const err = [];
        const { validation: { auto_validation, accordions, validate, validate_vr, done, codeBlock, codeLine, messages } } = regexp;
        const autoValidationMatch = content.match(auto_validation);
        const steps = content.replace(codeBlock, '').replace(codeLine, '').match(accordions);
        let validationFormExists, stepsWOAnyValidation;
        if(steps) {
            validationFormExists = steps.some(step => step.match(validate));
            stepsWOAnyValidation = steps.map((step, index) => ({ step, id: index + 1})).filter(({ step }) => !step.match(validate) && !step.match(done));
        }
        if(autoValidationMatch) {
            const [prop, value] = autoValidationMatch;
            const dirPath = path.dirname(filePath);
            const rulesPath = path.join(dirPath, 'rules.vr');
            const vrFileExists = fs.existsSync(rulesPath);
            if(isProduction) {
                if(vrFileExists) {
                    err.push(messages.production.rules_vr);
                }
            }
            if(value && (value == 'true' || value == 'false')) {
                if(!validationFormExists) {
                    err.push(messages.validate_restrictions.at_least_one);
                } else {
                    const validationsMatch = steps
                        .map(step => step.match(validate))
                        .filter(match => match)
                        .reduce((arr, matches) => arr.concat(matches), []);
                    const duplicates = common.findDuplicates(validationsMatch);
                    duplicates.forEach(duplicate => err.push(messages.validate_restrictions.duplicates(duplicate)));
                    if(!isProduction) {
                        if (vrFileExists) {
                            const vrFile = fs.readFileSync(rulesPath, 'utf-8');
                            (new Set(validationsMatch)).forEach(validationNumber => {
                                const validateBlock = vrFile.match(validate_vr(validationNumber));
                                if(!validateBlock) {
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
                if(stepsWOAnyValidation) {
                    stepsWOAnyValidation.forEach(({ step, id }) => err.push(messages.validate_restrictions.missed_validation(id)));
                }
            }
        } else if(validationFormExists) {
            err.push(messages.validate_wo_property);
        }
        return err;
    },
};
