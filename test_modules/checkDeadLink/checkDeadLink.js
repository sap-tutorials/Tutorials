'use strict';

var checkLink = require('markdown-link-check');

var { domains } = require('./white-list');

var options = {
  timeout: '90s',
};

const maxAttempts = 5;

const checkAndReportLink = (link, deadlinks, globalCounters, linkCounters, links, isPassed, callback) => {
  checkLink(link, options, function(err, results){
    var result = results[0];
    if(result.status == 'dead') {
      if(result.statusCode == 0 && linkCounters.attempts <= maxAttempts) {
        linkCounters.attempts++;
        return checkAndReportLink(link, deadlinks, globalCounters, linkCounters, links, isPassed, callback);
      } else {
        isPassed = false;
        const isTrusted = !!domains.find(domain => link.includes(domain));
        deadlinks.push({
          "url": result.link,
          "code": result.statusCode,
          isTrusted
         });
      }
    } 
    globalCounters.index++;
    if(globalCounters.index == links.length){
      callback({
        "isPassed": isPassed,
        "deadlinks": deadlinks
       });
    }
 });
};

module.exports = function(fname, links, callback) {

    if(links.length == 0) {
      callback({"isPassed":true, "deadlinks":[]});
    } else {
      //exclude readme files
      if (fname != "readme.md" && fname != "README.md") {
              var isPassed = true;
              var deadlinks = [];
              var index = 0;
              const globalCounters = {
                index: 0,
              }
          links.forEach(function(link){
            const linkCounters = {
              attempts: 0,
            }
            checkAndReportLink(link, deadlinks, globalCounters, linkCounters, links, isPassed, callback);
          })
      } else {
          callback(null);
      }
    } 
}
