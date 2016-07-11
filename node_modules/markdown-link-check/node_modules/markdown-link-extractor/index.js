'use strict';

var marked = require('marked');

module.exports = function markdownLinkExtractor(markdown) {
    var links = [];

    var renderer = new marked.Renderer();
    renderer.link = function (href, title, text) {
        links.push(href);
	return marked.Renderer.prototype.link.apply(this, arguments);
    };
    marked(markdown, { renderer: renderer });

    return links;
};
