const requestp = require('request-promise-native');
const request = require('request');

const { regexp, linkCheck } = require('../constants');
const { domains } = require('../../config/trusted.links.json');
const { linkUtils } = require('../utils');

process.env.UV_THREADPOOL_SIZE = linkCheck.UV_THREADPOOL_SIZE;

const configured = {
    requestp: requestp.defaults(linkCheck.defaultClientOptions),
    request: request.defaults(linkCheck.defaultClientOptions),
};

const checkLinks = async (links, onCheck) => {
    let processingLinks = links.map(link => checkLink(link));
    if(onCheck) {
        processingLinks = processingLinks.map(checkPromise => checkPromise.then(result => {
            onCheck(result);
            return result;
        }));
    }
    const processedResults = await Promise.all(processingLinks);
    const results = processedResults.filter(({ err, code }) => err || !linkUtils.is2xx(code)).map(({ link, code, err }) => {
            const isTrusted = !!domains.find(domain => link.includes(domain))
            return {
                link,
                code,
                isTrusted,
            };
        });
    return results;
};

const checkLink = (link, reqOptions = {}, maxAttempts = 2) => {
    return checkAttempt({ ...reqOptions, uri: link }, 1, maxAttempts);
};

const checkAttempt = async (options, attempt, maxAttempts) => {
    const { FakeWritable } = linkUtils;
    try {
        const result = await configured.requestp.head(options);
        return {
            link: options.uri,
            code: result.statusCode
        };
    } catch(e) {
        const promise = new Promise((resolve, reject) => {
          const stream = configured.request
            .get(options)
            .on('data', () => {
                // prevents from reading whole response body
              stream.abort();
              if(!linkUtils.is2xx(stream.response.statusCode)) {
                reject({
                  err: stream.response.statusMessage,
                  res: stream.response,
                });
              } else {
                resolve(stream.response);
              }
            });
          });
        try {
            const result = await promise;
            return {
                link: options.uri,
                code: result.statusCode
            };
        } catch ({ err, res = {} }) {
            if(attempt <= maxAttempts && !res.statusCode) {
                return checkAttempt(options, ++attempt, maxAttempts);
            }
            return {
                link: options.uri,
                code: res.statusCode || 0,
                err,
            };
        }
    }
};

module.exports = {
    checkLinks,
    checkLink,
};
