'use strict';

var fs = require('fs');
var path = require('path');

module.exports = function(file, callback) {
    //exclude contributing.md and all file in the template folder
    if (path.basename(file) != "contributing.md" && path.parse(file).dir.split(path.sep)[0] != "templates") {
        //read complete file
        var fileContent = fs.readFileSync(file, 'utf8');
        //return array with all lines of file
        callback({
            src: fileContent,
            lines: fileContent.split(/\r?\n/),
        });
    } else {
        callback(null);
    }
}
