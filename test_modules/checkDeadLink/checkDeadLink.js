'use strict';

var checklink = require('checklink');

module.exports = function(fname, links, callback) {

    //exclude readme files
    if (fname != "readme.md" && fname != "README.md") {
        //check array of links
        checklink(links)
            .then(function(results) {
                //return results (e.g. {isPassed: true, count: 18, deadlinks: []})
                callback(results);
            });
    } else {
        callback(null);
    }


}
