'use strict';

var fs = require('fs');

module.exports = function(input, callback) {
    //set metadata for md file
    var metadata = "---\n" +
        "title: " + input.title + "\n" +
        "description: " + input.description + "\n" +
        "tags: [ " + input.tags + " ]\n" +
        "---"

    //read content of template
    fs.readFile('./templates/tutorial.md', 'utf8', function(err, data) {
        //build file path
        var file = "./work-in-progress/" + input.filename + "/" + input.filename + '.md';
        //concatenate metadata with content of template
        var content = metadata + data;
        //create new tutorial
        fs.writeFile(file, content, (err) => {
            if (err) {
                callback(err);
            } else {
                callback("tutorial created");
            }
        });
    });

}
