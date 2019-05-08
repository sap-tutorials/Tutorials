'use strict';

const CONFIG_CMD_OPTIONS = { name: 'qaPath', alias: 'q', type: String };

module.exports = {
  commandLineOptions: [
    CONFIG_CMD_OPTIONS,
  ],
  requiredOptions: ['qaPath'],
  configFileName: '.config',
  qaRepoName: 'Tutorials-Contribution',
  reportFileName: 'Tutorials-Contribution',
};