'use strict';

const expressions = [
    /\[[^\]]*\]\((http[s]?:\/\/.+?)\)/g, //[...](<url>)
    /\[[^\]]*\]\s*?:\s*?<(http[s]?:\/\/.+?)>/g,//[...]: <url>
    /<(http[s]?:\/\/.*?)>/g
];

module.exports = function(fileContent) {

    const links = [];

    //get all links
    expressions.forEach(exp => {
        let match = exp.exec(fileContent);
        while (match !== null) {
            links.push(match[1]);
            match = exp.exec(fileContent);
        } 
    });

    return links;
}
