'use strict';

var checkLink = require('markdown-link-check');

var { domains } = require('./white-list');

var options = {
  timeout: '60s',
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
          links.forEach(function(link){
            checkLink(link, options, function(err, results){
               var result = results[0];
               if(result.status == 'dead') {
                 isPassed = false;
                 const isTrusted = !!domains.find(domain => link.includes(domain));
                 deadlinks.push({
                   "url": result.link,
                   "code": result.statusCode,
                   isTrusted
                  });
               }
               index++;
               if(index == links.length){
                 callback({
                   "isPassed": isPassed,
                   "deadlinks": deadlinks
                  });
               }
            });
          })
      } else {
          callback(null);
      }
    } 
}
