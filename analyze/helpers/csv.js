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
  }

  save(rows) {
    const csv = this.prepare(rows);
    const filePath = path.join('analyze', 'reports', this.fileName);

    return fs.writeFile(filePath, csv);
  }

  prepare() {
    throw new Error('Method should be defined explicitly!');
  }
}

module.exports = CsvHelper;
