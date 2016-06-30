'use strict';

var fs = require('fs');
var path = require('path');
var expect = require('expect.js');
var http = require('http');
var express = require('express');
var markdownLinkCheck = require('../');

describe('markdown-link-check', function () {

    var baseUrl;
    
    before(function (done) {
        var app = express();

        app.head('/nohead', function (req, res) {
            res.sendStatus(405); // method not allowed
        });
        app.get('/nohead', function (req, res) {
            res.sendStatus(200);
        });

        app.get('/foo/redirect', function (req, res) {
            res.redirect('/foo/bar');
        });
        app.get('/foo/bar', function (req, res) {
            res.json({foo:'bar'});
        });
        
        var server = http.createServer(app);
        server.listen(0 /* random open port */, 'localhost', function serverListen(err) {
            if (err) {
                done(err);
                return;
            }
            baseUrl = 'http://' + server.address().address + ':' + server.address().port;
            done();
        });
    });

    it('should check the links in sample.md', function (done) {
        markdownLinkCheck(fs.readFileSync(path.join(__dirname, 'sample.md')).toString().replace(/%%BASE_URL%%/g, baseUrl), function (err, results) {
            expect(err).to.be(null);
            expect(results).to.be.an('array');
            expect(results.length).to.be(5);

            expect(results[0].statusCode).to.be(200);
            expect(results[0].status).to.be('alive');

            expect(results[1].statusCode).to.be(404);
            expect(results[1].status).to.be('dead');

            expect(results[2].statusCode).to.be(0);
            expect(results[2].status).to.be('dead');

            expect(results[3].statusCode).to.be(200);
            expect(results[3].status).to.be('alive');

            expect(results[4].statusCode).to.be(200);
            expect(results[4].status).to.be('alive');

            done();
        });
    });
});
