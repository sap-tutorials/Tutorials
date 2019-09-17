'use strict';

const run = require('./run');
const extractRunOptions = require('./helpers/run-args');

async function runValidation() {
  let options = {};

  try {
    options = await extractRunOptions.readCmdOptions();
  } catch (e) {
    console.error(e);
  }

  return run(options.qaPath);
}

module.exports = runValidation();