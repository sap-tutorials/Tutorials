const { URL } = require('url');
const { regexp, linkCheck: { EXCLUDED_HOSTS }  } = require('../constants');

function removeTrailingSign(string, sign) {
  if (string.endsWith(sign)) {
    return string.substring(0, string.length - 1);
  }

  return string;
}

function removeCodeEntries (content) {
  const { validation: { codeBlock, codeLine, metaData } } = regexp;

  return content
    .replace(metaData, '')
    .replace(codeBlock, '')
    .replace(codeLine, '');
}

const extractLinks = (content) => {
  const { link: { markdown } } = regexp;
  const clearContent = removeCodeEntries(content);

  const links = markdown.map((regex) => {
    const links = [];
    let match;
    while (match = regex.exec(clearContent)) {
      links.push(match[1]);
    }

    return links;
  })
    .reduce((prev, curr) => prev.concat(curr), [])
    .map((mdLink) => {
      try {
        const urlObject = new URL(mdLink);

        if (EXCLUDED_HOSTS.includes(urlObject.hostname)) {
          return;
        }
        const url = urlObject.toString();
        return removeTrailingSign(url, '/');
      } catch (e) {
        console.warn('Not a valid url', mdLink);
        return undefined;
      }
    })
    .filter(link => link);
  return [...(new Set(links))];
};

const isErrorStatusCode = statusCode => /^(4|5)/.test(`${statusCode}`);
const isHttps = url => new URL(url).protocol.startsWith('https');

module.exports = {
  extractLinks,
  isHttps,
  isErrorStatusCode,
};
