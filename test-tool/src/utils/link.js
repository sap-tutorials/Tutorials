const { Writable } = require('stream');

const { regexp } = require('../constants');

const extractLinks = (content) => {
    const { link: { markdown, pure } } = regexp;

    const links = markdown.map(regex => {
        const links = [];
        let match;
        while(match = regex.exec(content)) {
            links.push(match[1]);
        }
        return links;
    })
    .reduce((prev, curr) => prev.concat(curr), [])
    .map(mdlink => {
        const match = mdlink.match(pure);
        if(match) {
            return match.shift();
        } 
    })
    .filter(link => link);
    return [...(new Set(links))];
};

const is2xx = (statusCode) => /^2/.test('' + statusCode);

module.exports = {
    extractLinks,
    is2xx,
};
