const ms = require('ms');

module.exports = {
    defaultClientOptions: {
        headers: {
            'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_11_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/47.0.2526.106 Safari/537.36', 
        },
        maxRedirects: 8,
        strictSSL: false,
        timeout: ms('90s'),
        resolveWithFullResponse: true,
        forever: true, 
        pool: {
            maxSockets: 10000,
        },
    },
    UV_THREADPOOL_SIZE: 64,
};