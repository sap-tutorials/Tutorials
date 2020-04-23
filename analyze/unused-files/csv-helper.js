'use strict';

const { Parser } = require('json2csv');

const { csvHeaders, checkTypes } = require('../constants');
const CsvHelper = require('../helpers/csv');

class UnusedFilesCsvHelper extends CsvHelper {
  constructor() {
    super(checkTypes.unusedFiles);
  }

  save(rows, env) {
    this.fileName = this.fileName.replace('report_', `report_${env}_`);
    super.save(rows);
  }

  prepare(rows) {
    const fields = Object
      .entries(csvHeaders[this.type])
      .reduce((result, [value, label]) => {
        return result.concat({
          value,
          label,
        });
      }, []);

    const parser = new Parser({ fields });

    return parser.parse(rows);
  }
}

module.exports = new UnusedFilesCsvHelper();
