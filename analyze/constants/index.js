'use strict';

const PATH_TO_QA_CMD_OPTION = {
  name: 'qaPath', alias: 'q', type: String,
};
const DELETE_FILES_CMD_OPTION = {
  name: 'deleteFiles', alias: 'd', type: Boolean,
};

const checkTypes = {
  validation: 'validation',
  memoryUsage: 'memory-usage',
};

const validationKeysMap = {
  onlyDone: 'onlyDone',
  noRulesFile: 'noRulesFile',
  notDefinedValidate: 'notDefinedValidate',
  stepWithNoDoneValidate: 'stepWithNoDoneValidate',
  duplicateRuleDefinition: 'duplicateRuleDefinition',
  failedToProcess: 'failedToProcess',
  noSteps: 'noSteps',
  everythingValid: 'everythingValid',
};

module.exports = {
  commandLineOptions: [
    PATH_TO_QA_CMD_OPTION,
    DELETE_FILES_CMD_OPTION,
  ],
  PATH_TO_QA_CMD_OPTION,
  requiredOptions: ['qaPath'],
  configFileName: '.config',
  qaRepoName: 'Tutorials-Contribution',
  reportFileName: 'report_{{type}}_{{date}}.csv',
  tutorialsFolderName: 'tutorials',
  rulesFileName: 'rules.vr',
  checkTypes,
  validationKeysMap,
  csvHeaders: {
    [checkTypes.validation]: {
      [validationKeysMap.onlyDone]: 'ONLY DONE',
      [validationKeysMap.noRulesFile]: 'NO RULES.VR',
      [validationKeysMap.notDefinedValidate]: 'NOT DEFINED VALIDATE',
      [validationKeysMap.stepWithNoDoneValidate]: 'STEP WITH NO DONE OR VALIDATE',
      [validationKeysMap.duplicateRuleDefinition]: 'DUPLICATE RULES',
      [validationKeysMap.failedToProcess]: 'FAILED TO PROCESS',
      [validationKeysMap.noSteps]: 'NO STEPS',
      [validationKeysMap.everythingValid]: 'OTHER',
    },
    [checkTypes.memoryUsage]: {
      tutorial: 'TUTORIAL',
      unusedFile: 'UNUSED FILE',
      unusedFileSize: 'UNUSED FILE SIZE',
      bigFile: 'BIG FILE',
      bigFileSize: 'BIG FILE SIZE',
      error: 'ERROR',
      total: 'TOTAL',
    },
  },
};
