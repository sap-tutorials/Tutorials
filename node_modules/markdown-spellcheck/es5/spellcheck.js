'use strict';

exports.__esModule = true;

var _hunspellSpellchecker = require('hunspell-spellchecker');

var _hunspellSpellchecker2 = _interopRequireDefault(_hunspellSpellchecker);

var _fs = require('fs');

var _fs2 = _interopRequireDefault(_fs);

var _path = require('path');

var _path2 = _interopRequireDefault(_path);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var spellchecker = void 0,
    dict = void 0;

function initialise(options) {

  var dictionaryOptions = options && options.dictionary;

  var baseFile = _path2.default.join(__dirname, '../data/en-GB');
  if (dictionaryOptions && dictionaryOptions.file) {
    baseFile = dictionaryOptions.file;
  } else if (dictionaryOptions && dictionaryOptions.language) {
    switch (dictionaryOptions.language) {
      case 'en-us':
        baseFile = _path2.default.join(__dirname, '../data/en_US-large');
        break;
      case 'en-gb':
        // default - do nothing
        break;
      case 'es-es':
        baseFile = _path2.default.join(__dirname, '../data/es_ANY');
        break;
      default:
        throw new Error("unsupported language:" + dictionaryOptions.language);
    }
  }

  spellchecker = new _hunspellSpellchecker2.default();
  dict = spellchecker.parse({
    aff: _fs2.default.readFileSync(baseFile + '.aff'),
    dic: _fs2.default.readFileSync(baseFile + '.dic')
  });
  spellchecker.use(dict);
}

function normaliseApos(word) {
  return word.replace(/\u2019/, "'");
}

function checkWord(word) {
  if (!spellchecker) {
    initialise();
  }
  word = normaliseApos(word);
  if (spellchecker.check(word)) {
    return true;
  }

  if (word.match(/'s$/)) {
    var wordWithoutPlural = word.substr(0, word.length - 2);
    if (spellchecker.check(wordWithoutPlural)) {
      return true;
    }
  }

  // for etc. as we cannot tell if it ends in "." as that is stripped
  var wordWithDot = word + ".";
  if (spellchecker.check(wordWithDot)) {
    return true;
  }

  if (word.indexOf('-')) {
    var subWords = word.split('-');

    if (subWords.every(function (subWord) {
      return spellchecker.check(subWord);
    })) {
      return true;
    }
  }

  return false;
}

function checkWords(words) {
  var mistakes = [];
  for (var i = 0; i < words.length; i++) {
    var wordInfo = words[i];
    if (!checkWord(wordInfo.word)) {
      mistakes.push(wordInfo);
    }
  }
  return mistakes;
}

function _addWord(word) {
  dict.dictionaryTable[word] = [[]];
}

var customDictionary = [];
var needsReset = false;
function addWord(word, temporary) {
  if (!spellchecker) {
    initialise();
  }

  word = normaliseApos(word);

  if (!temporary) {
    customDictionary.push(word);
  } else {
    needsReset = true;
  }
  _addWord(word);
}

function resetTemporaryCustomDictionary() {
  if (needsReset) {
    if (!spellchecker) {
      initialise();
    }
    customDictionary.forEach(function (word) {
      return _addWord(word);
    });
  }
}

function suggest(word) {
  return spellchecker.suggest(word);
}

exports.default = {
  initialise: initialise,
  checkWords: checkWords,
  checkWord: checkWord,
  addWord: addWord,
  resetTemporaryCustomDictionary: resetTemporaryCustomDictionary,
  suggest: suggest
};