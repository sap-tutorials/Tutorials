'use strict';

const fs = require('fs');
const util = require('util');

const accessAsync = util.promisify(fs.access.bind(fs));
const readFileAsync = util.promisify(fs.readFile.bind(fs));
const writeFileAsync = util.promisify(fs.writeFile.bind(fs));

module.exports = {
  access: accessAsync,
  writeFile: writeFileAsync,
  readFile: readFileAsync,
};