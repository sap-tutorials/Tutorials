'use strict';

const commandArgs = require('command-line-args');

const constants = require('../constants');
const configHelper = require('./config-helper');
const fs = require('../helpers/fs');

module.exports = {
  async readCmdOptions() {
    const options = commandArgs(constants.commandLineOptions);
    const configOptions = await configHelper.read();

    if (!options.qaPath) {
      if (!configOptions.qaPath) {
        configOptions.qaPath = await configHelper.getQaPath();
        await configHelper.write(`--qaPath ${configOptions.qaPath}`);
      }

      Object.assign(options, configOptions);
    } else {
      await this.validate(options);
    }

    if (!options.qaPath) {
      throw new Error(
        'Path to QA repo not found. Please run the command  with the following parameters "--qaPath <your path to QA repo>"'
      );
    }

    const theSame = configOptions.qaPath === options.qaPath;

    if (!configOptions.qaPath || !theSame) {
      await configHelper.write(`--qaPath ${options.qaPath}`);
    }

    return options;
  },

  validate(options) {
    return fs.access(options.qaPath);
  },
};