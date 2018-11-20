const ms = require('ms');

module.exports = {
    defaultClientOptions: {
        headers: {
            'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.102 Safari/537.36',
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
