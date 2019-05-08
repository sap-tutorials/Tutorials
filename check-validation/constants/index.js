'use strict';

const CONFIG_CMD_OPTIONS = { name: 'qaPath', alias: 'q', type: String };

module.exports = {
  commandLineOptions: [
    { name: 'tutorials', alias: 't', type: Boolean, defaultValue: true },
    { name: 'wip', alias: 'w', type: Boolean },
    CONFIG_CMD_OPTIONS,
  ],
  requiredOptions: ['qaPath'],
  configFileName: '.config',
  qaRepoName: 'Tutorials-Contribution',
  configOptions: [
    CONFIG_CMD_OPTIONS,
  ],
};