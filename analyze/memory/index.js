'use strict';

const run = require('./run');
const extractRunOptions = require('../helpers/run-args');
const constants = require('../constants');

async function runMemoryCheck() {
  let options = {};

  try {
    options = await extractRunOptions.readCmdOptions(constants.PATH_TO_QA_CMD_OPTION.name, false);
  } catch (e) {
    console.error(e);
  }

  return run(options);
}

module.exports = runMemoryCheck();
