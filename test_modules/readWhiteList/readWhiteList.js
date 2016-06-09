'use strict';

var fs = require('fs');
var spellCheck = require('../../node_modules/markdown-spellcheck/es5/spellcheck.js');

module.exports = function(callback) {
    //read all words from whiteList (.spelling)
    var whiteList = fs.readFileSync("./.spelling", "utf-8").split(/\r?\n/);

    //add all words from whiteList to spellCheck
    whiteList.forEach(function(word) {
        spellCheck['default'].addWord(word);
    });

    callback();

}
