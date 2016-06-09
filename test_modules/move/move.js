'use strict';

var fs = require('fs');
var check = require('../checkAll/check.js');
var path = require('path');

module.exports = function(file, callback) {
    //check file before moving
    check(["./work-in-progress/" + file + "/" + file + ".md"], function(result) {
        if (result) {
            //rename path of folder
            fs.rename("./work-in-progress/" + file, "./tutorials/" + file, function() {
                callback("tutorial moved");
            });
        } else {
            callback("\ntutorials with errors can not be moved!\nCheck your file with \"node test.js -i " + file + ".md" + "\"");
        }
    })


}
