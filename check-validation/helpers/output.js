'use strict';

const colorLog = require('color-log');

const { csvHeaders } = require('../constants');

function buildMessage(key, value = 0) {
  return `  ${csvHeaders[key]}: ${value}\n`;
}

function wrapMessage(message, fileName) {
  return `
  REPORT:
  -------

${message}
  -------
  
  Check ${fileName} for details
  `;
}

function output(stats, fileName) {
  const messages = Object
    .entries(stats)
    .reduce((result, [key, value]) => {
      return result.concat(buildMessage(key, value));
    }, '');

  return colorLog.info(wrapMessage(messages, fileName));
}

module.exports = output;
