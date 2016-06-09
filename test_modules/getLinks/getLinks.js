'use strict';

module.exports = function(fileContent) {

    //regular expressions to find links
    var expressions = [
        /\[[^\]]*\]\((http[s]?:\/\/[^\) ]+)/g, //[...](<url>)
        /\[[^\]]*\]\s*:\s*(http[s]?:\/\/.*)/g, //[...]: <url>
    ];

    var links = [];

    //get all links
    expressions.forEach(function(expression) {
        var match = expression.exec(fileContent);
        while (match !== null) {
            links.push(match[1]);
            match = expression.exec(fileContent);
        }
    }, this);

    return links;

}
