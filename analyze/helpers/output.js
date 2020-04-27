'use strict';

const colorLog = require('color-log');

const { csvHeaders } = require('../constants');

function buildMessage({ key, value = 0, type }) {
  return `  ${csvHeaders[type][key]}: ${value}\n`;
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

function output({ stats, fileName, type }) {
  const messages = Object
    .entries(stats)
    .reduce((result, [key, value]) => {
      return result.concat(buildMessage({ key, value, type }));
    }, '');

  return colorLog.info(wrapMessage(messages, fileName));
}

module.exports = output;
