'use strict';

var index = require('../../node_modules/markdown-spellcheck/es5/index.js');
var reportGenerator = require('../../node_modules/markdown-spellcheck/es5/report-generator');
var fs = require('fs');

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

    // Read the file, and change any --- horizontal rules in to - - -, avoiding the mdspell bug
    var fileData = fs.readFileSync(filePath, 'utf8');
    var hrcount = 0;
    var modified = fileData.replace(/---\r?\n/g, function(x){return (hrcount++ < 2 ? "---\n" : "- - -\n") } );

    //get all errors by checkking file
    var errors = index['default'].spell(modified, options);

    //return report of erros with infos
    callback(reportGenerator.generateFileReport("", {
	errors: errors,
	src: modified,
    }));
}
