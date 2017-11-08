'use strict';

var path = require('path');

var md = require('markdown-spellcheck').default;

var options = {
    ignoreAcronyms: true,
    ignoreNumbers: true,
    suggestions: false,
    dictionary: {
        file: path.join(__dirname, './dictionary/en-us')
    }
};

md.spellcheck.initialise(options);

module.exports = function(filePath, callback) {
    
    var errors = md.spellFile(filePath, options);

    callback(md.generateFileReport("", {
        errors: errors.errors,
        src: errors.src
    }));
}
