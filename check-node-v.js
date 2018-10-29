'use strict';

var childProcess = require('child_process');
var logger = require('color-log');
var packageMeta = require('./package.json');

module.exports = function () {
  logger.info('Checking node.js version...');

  return new Promise(function (resolve, reject) {
    return childProcess.exec('node -v', function (error, stdout, stderr) {
      if (error) {
        return reject(error);
      }
      if (stderr) {
        return reject(stderr);
      }


      return resolve(stdout);
    });
  })
    .then(function (result) {
      var engine = packageMeta.engines.node.replace(/[\=\<\>]*/g, '');
      var minVersion = parseFloat(engine);
      var clearOut = result.replace(/\s*/g, '');
      var currentVersion = parseFloat(clearOut.replace('v', ''));

      if (currentVersion < minVersion) {
        logger.error(`
          ATTENTION!
          You are using an outdated Node.js version. Currently installed version ${clearOut} is not supported, please upgrade to v${engine} or higher.
          Test tool cannot be run. Exiting.
          `);
        return process.exit(1);
      } else {
        logger.info(`Node version is ${clearOut}. Ready to run test tool.`);
      }
    })
    .catch(function (error) {
      logger.error('Unable to check Node.js version. Trying to run test tool..');
      logger.error(error);
    });
};
