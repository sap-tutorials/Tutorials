'use strict';

const fs = require('fs');
const util = require('util');

const redefined = {
  access: util.promisify(fs.access.bind(fs)),
  stat: util.promisify(fs.stat.bind(fs)),
  writeFile: util.promisify(fs.writeFile.bind(fs)),
  readFile: util.promisify(fs.readFile.bind(fs)),
  readDir: util.promisify(fs.readdir.bind(fs)),
  unlink: util.promisify(fs.unlink.bind(fs)),
};

const proxy = new Proxy(fs, {
  get(target, prop) {
    if (prop in redefined) {
      return redefined[prop];
    } else if (prop in target) {
      return target[prop];
    }
  }
});

module.exports = proxy;
