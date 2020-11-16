'use strict';
const path = require('path');

const { reportFileName } = require('../constants');
const fs = require('./fs');

class CsvHelper {
  constructor(type) {
    this.type = type;
    this.fileName = reportFileName
      .replace('{{type}}', type)
      .replace('{{date}}', Date.now());
    this.fullFilePath = path.join('analyze', 'reports', this.fileName);
  }

  save(rows) {
    const csv = this.prepare(rows);

    return fs.writeFile(this.fullFilePath, csv);
  }

  prepare() {
    throw new Error('Method should be defined explicitly!');
  }
}

module.exports = CsvHelper;
