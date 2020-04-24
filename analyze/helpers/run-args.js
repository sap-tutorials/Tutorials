'use strict';

const commandArgs = require('command-line-args');

const constants = require('../constants');
const configHelper = require('./config');
const fs = require('../helpers/fs');

module.exports = {
  async readCmdOptions(name, isRequired) {
    const options = commandArgs(constants.commandLineOptions);
    const configOptions = await configHelper.read();

    if (isRequired) {
      if (!options[name]) {
        if (!configOptions[name]) {
          configOptions[name] = await configHelper.getOption(name);

          if (configOptions[name]) {
            const isValid = await this.validate(configOptions, name);
            if (isValid) {
              Object.assign(options, configOptions);
              await configHelper.write(`--${name} "${configOptions[name]}"`);
            }
          }
        } else {
          const isValid = await this.validate(configOptions, name);
          if (isValid) {
            Object.assign(options, configOptions);
          }
        }
      }

      if (!options[name]) {
        throw new Error(
          `${name} not found. Please run the command  with the following parameters "--${name} <value>"`
        );
      }

      const isSame = configOptions[name] === options[name];

      if ((!configOptions[name] || !isSame)) {
        await configHelper.write(`--${name} ${options[name]}`);
      }
    }

    return options;
  },

  validate(options, name) {
    switch (name) {
      case 'qaPath':
        return fs.access(options[name])
          .then(() => true)
          .catch(() => false);
      default:
        return false;
    }
  },
};
