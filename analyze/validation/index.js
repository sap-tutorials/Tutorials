'use strict';

const run = require('./run');
const extractRunOptions = require('../helpers/run-args');
const constants = require('../constants');

async function runMemoryCheck() {
  let options = {};

  try {
    options = await extractRunOptions.readCmdOptions(constants.CONFIG_CMD_OPTIONS.name, true);
  } catch (e) {
    console.error(e);
    process.exit(1);
  }

  return run(options.qaPath);
}

module.exports = runMemoryCheck();
