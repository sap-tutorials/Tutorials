'use strict';

const path = require('path');
const commandArgs = require('command-line-args');

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
      result = commandArgs(constants.commandLineOptions, { argv: contents.split(' ') });
    } catch (e) {
      console.debug('Couldn\'t read local config. Trying to find QA repository in parent directory...');
    }

    if (!result.qaPath) {
      result.qaPath = await this.getQaPath();
    }

    return result;
  },

  async getQaPath() {
    // assuming it is in a parent folder
    if (fs.access(qaPath)) {
      return qaPath;
    }
  },
};