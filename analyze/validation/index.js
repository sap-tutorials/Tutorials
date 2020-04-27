'use strict';

const run = require('./run');
const extractRunOptions = require('../helpers/run-args');
const constants = require('../constants');

async function runMemoryCheck() {
  let options = {};

  try {
    options = await extractRunOptions.readCmdOptions(constants.PATH_TO_QA_CMD_OPTION.name, true);
  } catch (e) {
    console.error(e);
    process.exit(1);
  }

  return run(options.qaPath);
}

module.exports = runMemoryCheck();
