'use strict';

var expect = require('expect.js');
var markdownLinkExtractor = require('../');

describe('markdown-link-extractor', function () {

    it('should return an empty array when no links are present', function () {
        var links = markdownLinkExtractor('No links here');
        expect(links).to.be.an('array');
        expect(links).to.have.length(0);
    });
    
    it('should extract a link', function () {
        var links = markdownLinkExtractor('[example](http://www.example.com)');
        expect(links).to.be.an('array');
        expect(links).to.have.length(1);
        expect(links[0]).to.be('http://www.example.com');
    });

    it('should extract multiple links', function () {
        var links = markdownLinkExtractor('This is an [example](http://www.example.com). Hope it [works](http://www.example.com/works)');
        expect(links).to.be.an('array');
        expect(links).to.have.length(2);
        expect(links[0]).to.be('http://www.example.com');
        expect(links[1]).to.be('http://www.example.com/works');
    });

});
