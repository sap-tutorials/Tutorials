'use strict';

const fs = require('fs');
const util = require('util');

module.exports = {
  access: util.promisify(fs.access.bind(fs)),
  writeFile: util.promisify(fs.writeFile.bind(fs)),
  readFile: util.promisify(fs.readFile.bind(fs)),
  readDir: util.promisify(fs.readdir.bind(fs)),
};