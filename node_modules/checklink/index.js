/**
 * check deadlink
 */

var http = require('http');
var https = require('https');
var url = require('url');
var Promise = require('bluebird');
var _ = require('lodash');
var async = require('async');
var debug = require('debug')('checklink');

/**
 * 检查一组链接是否为死链接
 *
 * @param  {Array/String}  links
 * @param  {Number}        timeout
 * @return {Object}
 *         {
 *           isPassed: true/false,
 *           deadlinks: [{
 *             url: 'xxxx',
 *             code: 404,
 *             message: 'res.statusMessage'
 *           }]
 *         }
 */
module.exports = function(links, timeout) {

  if (!_.isArray(links)) {
    links = [links];
  }

  timeout = timeout || 30 * 1000;

  var uniqLinks = _.uniq(links);
  var queueCount = 200;

  if (process.platform === 'win32') {

    // https urls
    var httpsUrls = _.filter(uniqLinks, function(item) {
      return /^https:/.test(item);
    });

    if (httpsUrls.length > 10) {
      // windows 下 https 并发数不能超过 10
      queueCount = 10;
    }

  }

  return new Promise(function(resolve) {

    var results = {
      isPassed: true,
      count: links.length,
      deadlinks: []
    };

    var queue = async.queue(function(link, callback) {
      checkUrl(link, timeout, callback);
    }, queueCount);

    queue.push(uniqLinks, function(result) {
      if (!result.isPassed) {
        delete result.isPassed;
        results.isPassed = false;
        results.deadlinks.push(result);
      }
    });

    // 任务结束
    queue.drain = function() {
      debug(results);
      resolve(results);
    };

  });

};


/**
 * 检查死链接
 *
 * @param  {String} link
 * @param  {Number} timeout
 * @return {Object} result: {url: '', isPassed: false, code: 400, message: ''}
 */
function checkUrl(link, timeout, callback) {

  var result = {url: link};
  var httpObj = /^https:/.test(link) ? https : http;
  var timer;
  var USER_AGENT = 'Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/41.0.2272.118 Safari/537.36';

  // 过滤的错误信息: 无法验证的签名 | 证书
  var filterErrors = [
    'UNABLE TO VERIFY THE FIRST CERTIFICATE',
    'UNABLE_TO_VERIFY_LEAF_SIGNATURE',
    'CERT_HAS_EXPIRED',
    'CERTIFICATE HAS EXPIRED',
    'DEPTH_ZERO_SELF_SIGNED_CERT',
    'FORBIDDEN'
  ];

  var options = _.defaults(url.parse(link), {
    headers: {
      'User-Agent': USER_AGENT
    }
  });

  var endCallback = function(request) {
    clearTimeout(timer);
    callback && callback(result);
    callback = null;
    request.abort();
  };

  var request = httpObj.get(options, function(res) {

    result.code = res.statusCode;
    result.isPassed = res.statusCode < 400 || res.statusCode > 520;
    result.message = res.statusMessage || '';

    if (filterErrors.indexOf(result.message) !== -1) {
      result.isPassed = true;
    }

    endCallback(request);

  });

  request.on('error', function(err) {

    result.message = err.message || err.code || '';
    result.isPassed = filterErrors.indexOf(result.message.toUpperCase()) !== -1;
    result.code = err.statusCode || err.status || 500;

    endCallback(request);

  });

  timer = setTimeout(function() {

    result.code = 404;
    result.isPassed = false;
    result.message = 'timeout: ' + (timeout / 1000).toFixed(2) + 's';

    endCallback(request);

  }, timeout);
}
