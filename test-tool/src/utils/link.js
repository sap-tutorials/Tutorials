const { URL } = require('url');

const { regexp } = require('../constants');

const extractLinks = (content) => {
    const { link: { markdown } } = regexp;

    const links = markdown.map((regex) => {
        const links = [];
        let match;
        while (match = regex.exec(content)) {
            links.push(match[1]);
        }
        return links;
    })
      .reduce((prev, curr) => prev.concat(curr), [])
      .map((mdLink) => {
          try {
              const { href } = new URL(mdLink);
              return href;
          } catch (e) {
              console.warn('Not a valid url', mdLink);
              return undefined;
          }
      })
      .filter(link => link);
    return [...(new Set(links))];
};

const is2xx = (statusCode) => /^2/.test('' + statusCode);
const isHttps = (url) => new URL(url).protocol.startsWith('https');

module.exports = {
    extractLinks,
    is2xx,
    isHttps,
};
