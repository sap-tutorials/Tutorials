'use strict';

exports.__esModule = true;

exports.default = function (inputPatterns, options, fileCallback, resultCallback) {
  var allFiles = [];

  _async2.default.parallel([_spellConfig2.default.initialise.bind(_spellConfig2.default, './.spelling'), function (processed) {
    (0, _globby2.default)(inputPatterns).then(function (files) {
      allFiles = files;
      processed();
    }).catch(function () {
      console.error("Error globbing:", inputPatterns);
      process.exitCode = 1;
      processed();
    });
  }], function () {

    // finished callback - config loaded and glob has returned all files

    _spellConfig2.default.getGlobalWords().forEach(function (word) {
      return _spellcheck2.default.addWord(word);
    });

    _async2.default.mapSeries(allFiles, function (file, fileProcessed) {

      _fs2.default.readFile(file, 'utf-8', function (err, src) {

        if (err) {
          console.error("Failed to open file:" + file);
          console.error(err);
          process.exitCode = 1;
          return fileProcessed();
        }

        _spellConfig2.default.getFileWords(file).forEach(function (word) {
          return _spellcheck2.default.addWord(word, true);
        });

        fileCallback(file, src, function (err, result) {
          _spellcheck2.default.resetTemporaryCustomDictionary();
          fileProcessed(err, result);
        });
      });
    }, resultCallback);
  });
};

var _globby = require('globby');

var _globby2 = _interopRequireDefault(_globby);

var _async = require('async');

var _async2 = _interopRequireDefault(_async);

var _spellConfig = require('./spell-config');

var _spellConfig2 = _interopRequireDefault(_spellConfig);

var _spellcheck = require('./spellcheck');

var _spellcheck2 = _interopRequireDefault(_spellcheck);

var _fs = require('fs');

var _fs2 = _interopRequireDefault(_fs);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }