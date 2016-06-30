'use strict';

var _ = require('lodash');
var async = require('async');
var linkCheck = require('link-check');
var markdownLinkExtractor = require('markdown-link-extractor');

module.exports = function markdownLinkCheck(markdown, callback) {
    async.map(_.uniq(markdownLinkExtractor(markdown)), linkCheck, callback);
};
