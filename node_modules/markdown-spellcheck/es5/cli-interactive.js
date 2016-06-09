"use strict";

exports.__esModule = true;

exports.default = function (file, src, options, fileProcessed) {
  spellAndFixFile(file, src, options, function () {
    _spellConfig2.default.writeFile(fileProcessed);
  });
};

var _index = require("./index");

var _index2 = _interopRequireDefault(_index);

var _spellcheck = require("./spellcheck");

var _spellcheck2 = _interopRequireDefault(_spellcheck);

var _inquirer = require("inquirer");

var _inquirer2 = _interopRequireDefault(_inquirer);

var _filters = require("./filters");

var _filters2 = _interopRequireDefault(_filters);

var _context = require("./context");

var _context2 = _interopRequireDefault(_context);

var _spellConfig = require("./spell-config");

var _spellConfig2 = _interopRequireDefault(_spellConfig);

var _writeCorrections = require("./write-corrections");

var _writeCorrections2 = _interopRequireDefault(_writeCorrections);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }

var ACTION_IGNORE = "ignore";
var ACTION_FILE_IGNORE = "fileignore";
var ACTION_ADD = "add";
var ACTION_ADD_CASED = "add-cased";
var ACTION_CORRECT = "enter";

var CHOICE_IGNORE = { name: "Ignore", value: ACTION_IGNORE };
var CHOICE_FILE_IGNORE = { name: "Add to file ignores", value: ACTION_FILE_IGNORE };
var CHOICE_ADD = { name: "Add to dictionary - case insensitive", value: ACTION_ADD };
var CHOICE_ADD_CASED = { name: "Add to dictionary - case sensitive", value: ACTION_ADD_CASED };
var CHOICE_CORRECT = { name: "Enter correct spelling", value: ACTION_CORRECT };

var previousChoices = Object.create(null);

function incorrectWordChoices(word, message, filename, options, done) {
  var suggestions = options.suggestions ? _spellcheck2.default.suggest(word) : [];

  var choices = [CHOICE_IGNORE, CHOICE_FILE_IGNORE, CHOICE_ADD, CHOICE_CORRECT];

  if (word.match(/[A-Z]/)) {
    choices.splice(3, 0, CHOICE_ADD_CASED);
  }

  var defaultAction = ACTION_CORRECT;
  if (previousChoices[word]) {
    var previousAction = previousChoices[word];
    if (previousAction.newWord) {
      var suggestionIndex = suggestions.indexOf(previousAction.newWord);
      if (suggestions.indexOf(previousAction.newWord) >= 0) {
        defaultAction = suggestionIndex.toString();
      } else {
        suggestions.unshift(previousAction.newWord);
        defaultAction = "0";
      }
    } else {
      defaultAction = previousAction.action;
    }
  }

  suggestions.forEach(function (suggestion, index) {
    choices.push({
      key: index,
      name: suggestion,
      value: index.toString()
    });
  });

  _inquirer2.default.prompt([{
    type: "list",
    name: "action",
    message: message,
    choices: choices,
    default: defaultAction
  }]).then(function (answer) {
    switch (answer.action) {
      case ACTION_ADD:
        word = word.toLowerCase();
      /* fallthrough */
      case ACTION_ADD_CASED:
        _spellcheck2.default.addWord(word);
        _spellConfig2.default.addToGlobalDictionary(word);
        done();
        break;
      case ACTION_CORRECT:
        getCorrectWord(word, filename, options, done);
        break;
      case ACTION_FILE_IGNORE:
        _spellcheck2.default.addWord(word, true);
        _spellConfig2.default.addToFileDictionary(filename, word);
        previousChoices[word] = answer;
        done();
        break;
      case ACTION_IGNORE:
        _spellcheck2.default.addWord(word);
        done();
        break;
      default:
        var suggestionId = Number(answer.action);
        if (isNaN(suggestionId) || suggestionId >= suggestions.length) {
          throw new Error("unrecognise prompt action");
        }
        previousChoices[word] = { newWord: suggestions[suggestionId] };
        done(suggestions[Number(answer.action)]);
        break;
    }
  });
}

function getCorrectWord(word, filename, options, done) {
  _inquirer2.default.prompt([{
    type: "input",
    name: "word",
    message: "correct word >",
    default: word
  }]).then(function (answer) {
    var newWords = answer.word.split(/\s/g);
    var hasMistake = false;

    for (var i = 0; i < newWords.length; i++) {
      var newWord = newWords[i];
      if (_filters2.default.filter([{ word: newWord }], options).length > 0 && !_spellcheck2.default.checkWord(newWord)) {
        hasMistake = true;
      }
    }

    if (hasMistake) {
      if (newWords.length === 1) {
        incorrectWordChoices(answer.word, "Corrected word is not in dictionary..", filename, options, function (newNewWord) {
          var finalNewWord = newNewWord || answer.word;
          previousChoices[word] = { newWord: finalNewWord };
          done(finalNewWord);
        });
        return;
      }

      console.log("Detected some words in your correction that may be invalid. Re-run to check.");
    }

    previousChoices[word] = { newWord: answer.word };
    done(answer.word);
  });
}

function spellAndFixFile(filename, src, options, onFinishedFile) {
  var corrections = [];

  function onSpellingMistake(wordInfo, done) {
    var displayBlock = _context2.default.getBlock(src, wordInfo.index, wordInfo.word.length);
    console.log(displayBlock.info);
    incorrectWordChoices(wordInfo.word, " ", filename, options, function (newWord) {
      if (newWord) {
        corrections.push({ wordInfo: wordInfo, newWord: newWord });
      }
      done();
    });
  }

  _index2.default.spellCallback(src, options, onSpellingMistake, function () {
    if (corrections.length) {
      (0, _writeCorrections2.default)(src, filename, corrections, onFinishedFile);
    } else {
      onFinishedFile();
    }
  });
}