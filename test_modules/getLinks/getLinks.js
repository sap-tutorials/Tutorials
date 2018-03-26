'use strict';

module.exports = function(fileContent) {

    //regular expressions to find links
    var expressions = [
        /\[[^\]]*\]\((http[s]?:\/\/[^\) ]+)/g, //[...](<url>)
        /\[[^\]]*\]\s*:\s*(http[s]?:\/\/.*)/g, //[...]: <url>
    ];

    var links = [];

    //get all links
    fileContent.forEach(line => {
        expressions.forEach(exp => {
            let match = exp.exec(line);
            while (match !== null) {
                links.push(match[1]);
                match = exp.exec(line);
            } 
        });
    });

    return links;

}
