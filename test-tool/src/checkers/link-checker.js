const checkLinks = require('check-links');
const { getStatusText, NOT_ACCEPTABLE, BAD_REQUEST, TOO_MANY_REQUESTS } = require('http-status-codes');

const { regexp: { content: { internalLink, remoteImage } }, linkCheck } = require('../constants');
const { domains } = require('../../config/trusted.links.json');

process.env.UV_THREADPOOL_SIZE = linkCheck.UV_THREADPOOL_SIZE;

const isAlive = (status) => status === 'alive';

const verifyLinks = async (links) => {
  const processedResults = await checkLinks(links, {
    timeout: linkCheck.TIMEOUT,
    retry: {
      limit: linkCheck.MAX_RETRIES,
      calculateDelay: () => linkCheck.TIMEOUT,
    },
    hooks: {
      afterResponse: [(response) => {
        if (`${response.statusCode}` === `${TOO_MANY_REQUESTS}`) {
          console.log(response.statusCode, getStatusText(TOO_MANY_REQUESTS));
        }
        return response;
      }]
    }
  });
  return Object
    .entries(processedResults)
    .filter(([link, { status }]) => !isAlive(status))
    .map(([link, { statusCode }]) => {
      const isTrusted = domains.some(domain => link.includes(domain));
      const isInternal = internalLink.regexp.test(link);
      return {
        link,
        isTrusted,
        code: statusCode || 0,
        reason: isInternal ? internalLink.message : getStatusText(statusCode || BAD_REQUEST),
      };
    });
};

const checkImageLink = async (link) => {
  const result = await checkLinks([link], {
    hooks: {
      afterResponse: [(response) => {
        const contentType = (response.headers['Content-Type'] || response.headers['content-type'] || '');

        if (contentType.includes('image/')) {
          return response;
        }

        response.statusCode = NOT_ACCEPTABLE;
        return response;
      }]
    }
  });
  const linkResult = result[link];
  const { status, statusCode = BAD_REQUEST } = linkResult;

  if (`${statusCode}` === `${NOT_ACCEPTABLE}`) {
    result.error = remoteImage.message;
    return result;
  }

  linkResult.error = isAlive(status) ? undefined : getStatusText(statusCode || BAD_REQUEST);
  if (linkResult.error) {
    // ignore, in case of error link checker will throw it
    delete linkResult.error;
    return linkResult;
  }

  return result;
};

const checkLink = async (link) => {
  const result = await checkLinks([link]);
  const linkResult = result[link];
  const { status, statusCode } = linkResult;
  linkResult.error = isAlive(status) ? undefined : getStatusText(statusCode || BAD_REQUEST);
  linkResult.code = statusCode;
  linkResult.link = link;

  return linkResult;
};

module.exports = {
  checkLink,
  checkImageLink,
  checkLinks: verifyLinks,
};
