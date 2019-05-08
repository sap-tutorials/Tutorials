'use strict';

const run = require('./run');
const extractRunOptions = require('./helpers/run-args');

module.exports = (async () => {
  let options = {};
  try {
    options = await extractRunOptions.readCmdOptions();
    console.log(options);
  } catch (e) {
    console.error(e);
  }

  return run(options);
})();