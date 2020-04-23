'use strict';

const { Parser } = require('json2csv');

const { csvHeaders, checkTypes, validationKeysMap } = require('../constants');
const CsvHelper = require('../helpers/csv');

class ValidationCsvHelper extends CsvHelper {
  constructor() {
    super(checkTypes.validation);
  }

  prepare(rows) {
    const fields = Object
      .entries(csvHeaders[this.type])
      .reduce((result, [value, label]) => {
        if (value !== validationKeysMap.everythingValid) {
          return result.concat({
            value,
            label,
          });
        }

        return result;
      }, []);

    const parser = new Parser({ fields });

    return parser.parse(rows);
  }
}

module.exports = new ValidationCsvHelper();
