const checkLinks = require('check-links');
const { getStatusText, NOT_ACCEPTABLE, BAD_REQUEST, TOO_MANY_REQUESTS } = require('http-status-codes');

const { regexp: { content: { internalLink, remoteImage } }, linkCheck } = require('../constants');
const { domains } = require('../../config/trusted.links.json');

process.env.UV_THREADPOOL_SIZE = linkCheck.UV_THREADPOOL_SIZE;

const isAlive = (status) => status === 'alive';

const getErrorMessage = (statusCode) => {
  try {
    return getStatusText(statusCode);
  } catch (e) {
    return 'Unreachable link';
  }
};

const verifyLinks = async (links) => {
  const processedResults = await checkLinks(links, {
    retry: {
      timeout: linkCheck.TIMEOUT,
      statusCodes: [TOO_MANY_REQUESTS],
      retries: (iterations, error) => {
        const shouldRetry = error.statusCode === TOO_MANY_REQUESTS;
        if (linkCheck.MAX_RETRIES < iterations || !shouldRetry) {
          return 0;
        }

        return linkCheck.TIMEOUT;
      }
    },
    hooks: {
      beforeRetry: [(options, error, retryCount) => {
        console.log(
          'Before retry: error',
          (error.statusCode || 'Unknown') || error.code,
          ', retryCount is ',
          retryCount,
          ', URL is ',
          error.url
        );
      }],
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
        reason: isInternal ? internalLink.message : getErrorMessage(statusCode),
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

  linkResult.error = isAlive(status) ? undefined : getErrorMessage(statusCode);
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
  linkResult.error = isAlive(status) ? undefined : getErrorMessage(statusCode);
  linkResult.code = statusCode;
  linkResult.link = link;

  return linkResult;
};

module.exports = {
  checkLink,
  checkImageLink,
  checkLinks: verifyLinks,
};
