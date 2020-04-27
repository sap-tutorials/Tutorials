'use strict';

const path = require('path');

const commandArgs = require('command-line-args');
const parse = require('shell-quote').parse;

const constants = require('../constants');
const fs = require('./fs');

const configPath = path.join(__dirname, `../../${constants.configFileName}`);
const qaPath = path.join(__dirname, `../../../${constants.qaRepoName}`);

module.exports = {
  configExists() {
    return fs.access(configPath)
      .then(() => true)
      .catch(() => false);
  },

  write(data) {
    return fs.writeFile(configPath, data);
  },

  async read() {
    if (!await this.configExists()) {
      return {};
    }

    const contents = await fs.readFile(configPath, 'utf-8');

    let result = {};

    try {
      result = commandArgs(constants.commandLineOptions, { argv: parse(contents) });
    } catch (e) {
      console.debug('Couldn\'t read local config');
    }

    return result;
  },

  async getOption(name) {
    switch (name) {
      case 'qaPath':
        try {
          await fs.access(qaPath);
          return qaPath;
        } catch (e) {
          return undefined;
        }
      default:
        return undefined;
    }
  },
};
