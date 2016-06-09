'use strict';

var index = require('../../node_modules/markdown-spellcheck/es5/index.js');
var reportGenerator = require('../../node_modules/markdown-spellcheck/es5/report-generator');

module.exports = function(filePath, callback) {

    //set options for spelling check
    var options = {
        ignoreAcronyms: true,
        ignoreNumbers: true,
        suggestions: false,
        dictionary: {
            language: "en-us",
            file: "../../markdown-spellcheck/data/en_US-large.dic"
        }
    };

    //get all errors by checkking file
    var errors = index['default'].spellFile(filePath, options);

    //return report of erros with infos
    callback(reportGenerator.generateFileReport("", {
        errors: errors.errors,
        src: errors.src
    }));
}
