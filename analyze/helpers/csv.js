'use strict';

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

    return fs.writeFile(this.fileName, csv);
  }

  prepare() {
    throw new Error('Method should be defined explicitly!');
  }
}

module.exports = CsvHelper;
