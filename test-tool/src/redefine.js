const filters = require('markdown-spellcheck/es5/filters');

function filterFactory(regexp) {
  return function (errors) {
    return errors.filter(function (e) {
      return !e.word.match(regexp);
    });
  };
}

module.exports = function redefine() {
  const numbers = filterFactory(/^[\w\W]*[0-9,\.\-#]+[\w\W]*$/);
  const acronyms = filterFactory(/^[A-Z0-9]{2,}(['\u2018-\u2019]s)?(\-\w+)?$/);

  filters.default.numbers = numbers;
  filters.default.acronyms = acronyms;
  filters.default.filter = function (words, options) {
    const ignoreAcronyms = options && options.ignoreAcronyms;
    const ignoreNumbers = options && options.ignoreNumbers;

    if (ignoreAcronyms) {
      words = acronyms(words);
    }
    if (ignoreNumbers) {
      words = numbers(words);
    }
    return words;
  };
};
