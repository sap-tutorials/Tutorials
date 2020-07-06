'use strict';

const regexp = require('../constants/regexp');
const linkChecker = require('./link-checker');

const { content: { metadata } } = regexp;

const checkNames = Object.keys(metadata);
const checkEntries = Object.entries(metadata);

module.exports = {
  /**
   * @param {String[]} meta
   * @returns {Object[]}
   * */
  check(meta) {
    const found = checkNames.reduce((result, name) => ({
      ...result,
      [name]: 0,
    }), {});

    return meta.reduce((result, line, index) => {
      checkEntries.forEach(([checkName, { regexp, message }]) => {
        if (!found[checkName]) {
          const match = line.match(regexp);

          if (match) {
            found[checkName] += 1;
            return;
          }

          const isLast = index === (meta.length - 1);
          if (isLast) {
            result.push({ msg: message, line: 1 });
          }
        }
      });

      return result;
    }, []);
  },

  async checkAuthorProfile(url) {
    const result = await linkChecker.checkLink(url);

    if (result.err) {
      return result;
    }
  },
};
