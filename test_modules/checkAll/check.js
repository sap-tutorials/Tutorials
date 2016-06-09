'use strict';

//load modules
var checkFilename = require('../checkFilename/checkFilename.js');
var readLines = require('../readLines/readLines.js');
var checkContent = require('../checkContent/checkContent.js');
var checkDeadLink = require('../checkDeadLink/checkDeadLink.js');
var getLinks = require('../getLinks/getLinks.js');
var mdspell = require('../mdspell/mdspell.js');
var readWhiteList = require('../readWhiteList/readWhiteList.js')
var log = require('color-log');
var path = require('path');
var ProgressBar = require('progress');

module.exports = function(files, showprogressbar, callback) {
    console.log("\n");

    //initialize ProgressBar
    var bar = new ProgressBar('testing [:bar] :percent :elapsed', {
        complete: "=",
        width: 50,
        total: (4 * files.length) + 2
    });



    //initialize log for every test
    var logFilename = "";
    var logMdSpell = "";
    var logContent = "";
    var logDeadLink = "";

    //initialize error counter for every test
    var cntFilename = 0;
    var cntMdspell = 0;
    var cntContent = 0;
    var cntDeadLink = 0;
    var checkedLinks = 0;

    //count files which are excluded from tests
    var cntNotCheckedContent = 0;
    var cntNotCheckedDeadlink = 0;

    var index = 0;
    if(showprogressbar){ bar.tick(); };

    //read .spelling file
    readWhiteList(function() {
        if(showprogressbar){ bar.tick(); };
        //check every file
        files.forEach(function(file) {

            //check Filenames
            var fname = path.basename(file);
            var filenameRes = checkFilename(fname, file);
            if(showprogressbar){ bar.tick(); };
            //build error log
            if (filenameRes !== null) {
                cntFilename++;
                logFilename += '\n\n    > Error: \n        file:   ' + fname +
                    '\n        path:   ' + file +
                    '\n        reason: ' + filenameRes;
            }

            //check spelling
            mdspell(file, function(report) {
                if(showprogressbar){ bar.tick(); };
                var splitReport = report.split(/\r?\n/);
                if (splitReport.length >= 3) {
                    //build error log
                    for (var i = 1; i <= splitReport.length - 2; i++) {
                        var splitReason = splitReport[i].split('|');
                        cntMdspell++;
                        logMdSpell += '\n\n    > Error: \n        file:    ' + fname +
                            "\n        line:    " + splitReason[0] +
                            "\n        reason:  " + splitReason[1];
                    }
                }
            })


            //readLines
            readLines(file, function(fileContent) {
                if(showprogressbar){ bar.tick(); };
                if (fileContent != null) {
                    //check file content
                    var lineIndex = 0;
                    fileContent.forEach(function(line, i) {
                        checkContent(file, line, function(matchResult) {
                            //build error log
                            if (matchResult !== null) {
                                cntContent++;
                                logContent += '\n\n    > Error: \n        line:    ' + (i + 1) +
                                    "\n        file:    " + fname +
                                    "\n        reason:  " + matchResult;
                            }
                        });
                        if (index === files.length && lineIndex === fileContent.length) {

                        }
                    });
                } else {
                    cntNotCheckedContent++;
                }

                //check links
                var x = checkDeadLink(fname, getLinks(fileContent), function(results) {
                    if(showprogressbar){ bar.tick(); };
                    index++;
                    if (results != null) {
                        checkedLinks += results.count;
                        //build error log
                        if (!results.isPassed) {
                            cntDeadLink += results.deadlinks.length;
                            var errmsg = [];
                            results.deadlinks.forEach(function(deadlink) {
                                errmsg.push('\n\n        url: ' + deadlink.url + '\n        code: ' + deadlink.code + '\n        message: ' + deadlink.message);
                            });
                            logDeadLink += '\n    > Deadlink(s) found in ' + fname + ':' + errmsg + '\n';
                        }
                    } else {
                        cntNotCheckedDeadlink++;
                    }
                    //show log after all files are checked
                    if (index === files.length) {
                        logResult();
                        //only for moving files to check if there are any errors
                        if (callback)
                            callback((cntFilename + cntMdspell + cntContent + cntDeadLink) == 0);
                    }

                });

            });



        })

        //log all results from testing with formatting
        function logResult() {
            //calc amount of files which were tested
            var cntCheckedContent = files.length - cntNotCheckedContent;
            var cntCheckeadDeadlink = files.length - cntNotCheckedContent - cntNotCheckedDeadlink;

            console.log("\n");

            //log for filenames
            if (logFilename == "") {
                log.info("CHECK FILENAMES: \n\n  >>> " + files.length + " File(s) checked without Error(s)\n");
            } else {
                log.error("CHECK FILENAMES: " + logFilename + "\n\n  >>> " + files.length + " File(s) checked with " + cntFilename + " Error(s)\n");
            }

            console.log(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");

            //log for spelling check
            if (logMdSpell == "") {
                log.info("CHECK SPELLING: \n\n  >>> " + files.length + " File(s) checked without Error(s)\n");
            } else {
                log.error("CHECK SPELLING: " + logMdSpell + "\n\n  >>> " + files.length + " File(s) checked with " + cntMdspell + " Error(s)\n");
            }

            console.log(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");

            //log for content check
            if (logContent == "") {
                log.info("CHECK CONTENT: \n\n  >>> " + cntCheckedContent + " File(s) checked without Error(s)\n");
            } else {
                log.error("CHECK CONTENT: " + logContent + "\n\n  >>> " + cntCheckedContent + " File(s) checked with " + cntContent + " Error(s)\n");
            }

            console.log(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");

            //log for deadlink check
            if (logDeadLink == "") {
                log.info("CHECK LINKS: \n\n  >>> " + checkedLinks + " Link(s) checked in " + cntCheckeadDeadlink + " File(s) without Error(s)\n");
            } else {
                log.error("CHECK LINKS: \n" + logDeadLink + "\n\n  >>> " + checkedLinks + " Link(s) checked in " + cntCheckeadDeadlink + " File(s) with " + cntDeadLink + " Error(s)\n");
            }
        }
    });


}
