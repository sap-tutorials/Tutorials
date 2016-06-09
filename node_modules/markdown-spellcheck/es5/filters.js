"use strict";

exports.__esModule = true;
function filterFactory(regexp) {
  return function (errors) {
    return errors.filter(function (e) {
      return !e.word.match(regexp);
    });
  };
}

var numbers = filterFactory(/^[0-9,\.\-#]+(th|st|nd|rd)?$/);
var acronyms = filterFactory(/^[A-Z0-9]{2,}(['\u2018-\u2019]s)?$/);

exports.default = {
  acronyms: acronyms,
  numbers: numbers,
  filter: function filter(words, options) {
    var ignoreAcronyms = options && options.ignoreAcronyms;
    var ignoreNumbers = options && options.ignoreNumbers;

    if (ignoreAcronyms) {
      words = acronyms(words);
    }
    if (ignoreNumbers) {
      words = numbers(words);
    }
    return words;
  }
};