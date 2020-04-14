'use strict';

const { Parser } = require('json2csv');

const { csvHeaders, reportFileName, validationKeysMap } = require('../constants');
const fs = require('./fs');

const fileName = reportFileName.replace('{{date}}', Date.now());

module.exports = {
  fileName,
  save(rows) {
    const csv = this.prepare(rows);

    return fs.writeFile(fileName, csv);
  },

  prepare(rows) {
    const fields = Object
      .entries(csvHeaders)
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
  },
};