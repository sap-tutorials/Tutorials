const { URL } = require('url');
const { regexp, linkCheck: { EXCLUDED_HOSTS }  } = require('../constants');

function removeTrailingSign(string, sign) {
  if (string.endsWith(sign)) {
    return string.substring(0, string.length - 1);
  }

  return string;
}


function removeCodeEntries(content) {
  const { validation: { codeBlock, codeLine, metaData } } = regexp;

  return content
    .replace(metaData, '')
    .replace(codeBlock, '')
    .replace(codeLine, '');
}

function isExcluded(link) {
  try {
    const urlObject = new URL(link);

    return EXCLUDED_HOSTS.includes(urlObject.hostname);
  } catch (e) {
    return EXCLUDED_HOSTS.some(l => link.includes(l));
  }
}

const extractLinks = (content) => {
  const { link: { markdown } } = regexp;
  const clearContent = removeCodeEntries(content);

  const links = markdown.map((regex) => {
    const links = [];
    let match;
    // eslint-disable-next-line no-cond-assign
    while (match = regex.exec(clearContent)) {
      links.push(match[1]);
    }
    return links;
  })
    .reduce((prev, curr) => prev.concat(curr), [])
    .reduce((result, mdLink) => {
      const isIgnored = isExcluded(mdLink);

      if (isIgnored) {
        return result;
      }

      return result.concat(removeTrailingSign(mdLink, '/'));
    }, []);
  return [...(new Set(links))];
};

module.exports = {
  extractLinks,
};
