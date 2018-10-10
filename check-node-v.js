'use strict';

var childProcess = require('child_process');
var util = require('util');
var logger = require('color-log');
var packageMeta = require('./package.json');

var exec = util.promisify(childProcess.exec);

module.exports = function () {
  logger.info('Checking node.js version...');

  return exec('node -v')
    .then(function(result) {
      if (result.stderr) {
        throw new Error(result.stderr);
      }

      var minVersion = parseFloat(packageMeta.engines.node.replace(/[\=\<\>]*/g, ''));
      var clearOut = (result.stdout).replace(/\s*/g, '');
      var currentVersion = parseFloat((clearOut).replace('v', ''));

      if (currentVersion < minVersion) {
        logger.error(`
          ATTENTION!
          You are using an outdated Node.js version. Currently installed version ${clearOut} is not supported, please upgrade it to v${parseInt(minVersion)} or higher.
          Test tool cannot be run. Exiting.
          `);
        return process.exit(1);
      } else {
        logger.info(`Node version is ${clearOut}. Ready to run test tool.`);
      }
    })
    .catch(function(error) {
      logger.error(error);
      logger.error('Unable to check Node.js version. Trying to run test tool..');
    });
};
