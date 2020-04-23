'use strict';

const commandArgs = require('command-line-args');

const constants = require('../constants');
const configHelper = require('./config');
const fs = require('../helpers/fs');

module.exports = {
  async readCmdOptions(name, isRequired) {
    const options = commandArgs(constants.commandLineOptions);
    console.log(options);
    const configOptions = await configHelper.read();

    if (!options[name] && isRequired) {
      if (!configOptions[name]) {
        configOptions[name] = await configHelper.getOption(name);
        await configHelper.write(`--${name} ${configOptions[name]}`);
      }

      Object.assign(options, configOptions);
    } else {
      await this.validate(options, name);
    }

    if (!options[name] && isRequired) {
      throw new Error(
        'Path to QA repo not found. Please run the command  with the following parameters "--qaPath <your path to QA repo>"'
      );
    }

    const isSame = configOptions[name] === options[name];

    if (!configOptions[name] || !isSame) {
      await configHelper.write(`--qaPath ${options[name]}`);
    }

    return options;
  },

  validate(options, name) {
    return fs.access(options[name]);
  },
};
