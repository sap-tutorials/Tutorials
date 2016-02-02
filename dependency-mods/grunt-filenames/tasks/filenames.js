/*
 * grunt-filenames
 * https://github.com/bahmutov/grunt-filenames
 *
 * Copyright (c) 2014 Gleb Bahmutov
 * Licensed under the MIT license.
 */

'use strict';

var basename = require('path').basename;

module.exports = function(grunt) {
  var dashed = /^[a-z\-]+\./;
  var camelCase = /^[a-z][a-zA-Z]*\./;

  function getExceptions(options) {
    var isException = function () {
      return false;
    };
    if (typeof options.except === 'function') {
      isException = options.except;
    } else if (typeof options.except === 'string') {
      isException = function (name) {
        return name === options.except;
      };
    } else if (Array.isArray(options.except)) {
      isException = function (name) {
        return options.except.indexOf(name) !== -1;
      };
    }
    return isException;
  }

  grunt.registerMultiTask('filenames', 'Validates source filenames', function () {
    var options = this.options({
      valid: camelCase
    });
    if (options.valid === 'dashed' ||
      options.valid === 'dashes') {
      options.valid = dashed;
    } else if (options.valid === 'camelCase') {
      options.valid = camelCase;
    } else if (typeof options.valid === 'string') {
      options.valid = new RegExp(options.valid);
    }

    var check;
    if (typeof options.valid === 'function') {
      grunt.verbose.writeln('Validating filenames using function\n' + options.valid.toString());
      check = options.valid;
    } else {
      grunt.verbose.writeln('Validating filenames using RegExp', options.valid);
      check = function (filename) {
        return options.valid.test(filename);
      };
    }

    if (typeof options.error !== 'string') {
      options.error = 'file {filename} does not pass check {valid}';
    }

    var isException = getExceptions(options);

    var allValid = this.files.every(function (file) {
      return file.src.every(function (filename) {
        grunt.verbose.writeln('testing filename', filename);

        var name = basename(filename);
        // updated by agraebe: also provide the entire filepath
        var valid = check(name, filename);
        if (!valid) {
          if (isException(name)) {
            grunt.verbose.writeln('filename', filename, 'is an exception');
            valid = true;
          } else {
            grunt.log.error(
              options.error
                .replace(/{filename}/, filename, 'gi')
                .replace(/{valid}/, options.valid.toString(), 'gi')
            );
          }
        }
        return valid;
      });
    });

    if (allValid) {
      var n = this.files.reduce(function (total, file) {
        return total + file.src.length;
      }, 0);
      grunt.log.ok(n + ' file(s) without filename problems');
    }
    return allValid;
  });
};
