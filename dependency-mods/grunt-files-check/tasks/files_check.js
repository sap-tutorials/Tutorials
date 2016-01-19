/*
 * grunt-files-check
 * https://github.com/matthis-perrin/grunt-files-check
 *
 * Copyright (c) 2013 Matthis Perrin
 * Licensed under the MIT license.
 */

'use strict';

var colors = require('colors');

module.exports = function(grunt) {

  grunt.registerMultiTask('files_check', 'Grunt plugin to apply regular expressions on files and check their validity.', function() {

    // Merge task-specific and/or target-specific options with these defaults.
    var options = this.options({
      excluded: [],
      patterns: [ '^$' ],
      verbose: false,
      maxFileNameWidth: 40,
      output: null
    });

    // Iterate over all specified file groups.
    this.files.forEach(function(f) {

      // Filtering of the files described in the `src` option by
      // removing those specified in the `excluded` option and those
      // which are directories.
      var files = f.src.filter(function(filepath) {
        if (!grunt.file.exists(filepath)) {
          return false;
        }
        else if (grunt.file.isDir(filepath)) {
          return false;
        }
        else if (inArray(filepath, options.excluded)) {
          return false;
        }
        else {
          return true;
        }
      });



      // When calculate the max size of files name.
      // This is useful for the error display.
      var maxSize = 0;
      for (var i = 0; i < files.length; i++) {
        var lineNumber = files[i].split(/\r?\n/).length;
        var fileNameSize = files[i].length + 1 + lineNumber.toString().length; // max length of 'fileName:lineNumber'
        if (fileNameSize > maxSize) {
          maxSize = fileNameSize;
        }
      }

      function checkPattern(pattern) {
        re = new RegExp(pattern);
        matchResult = fileLines[j].match(re);

        // Check if there is a match
        if (matchResult !== null) {

          // Update the tracking variables
          fileCorrect = false;
          incorrect++;

          // Displaying the error in the console
          fileNameWithLineNumber = fileName + ':' + (j + 1);
          formattedFileName = formatFileName(fileNameWithLineNumber, options.maxFileNameWidth);
          grunt.log.error(formattedFileName + '   found \'' + matchResult[0] + '\' in the line \'' + matchResult.input.trim() + '\'');

        }
      }

      // Then we apply the regex on the files
      var incorrect     = 0,
          outputContent = '';

      for (i = 0; i < files.length; i++) {

        // Get the content of the file
        var fileName = files[i];
        var formattedFileName, re, matchResult, fileNameWithLineNumber;
        var fileContent = grunt.file.read(fileName);

        // Extract each line of the file
        var fileLines = fileContent.split(/\r?\n/);

        // Applying the regex on every line
        var fileCorrect = true;
        for (var j = 0; j < fileLines.length; j++) {

          // Apply all regex on the line

          options.patterns.forEach(checkPattern);
        }

        // If the file is correct, we display a message in the console (only in verbose mode)
        if (fileCorrect && options.verbose && options.output === null) {
          formattedFileName = formatFileName(fileName, options.maxFileNameWidth);
          grunt.log.ok(formattedFileName + '   OK'.green);
        }

      }


      // Display the final message
      if (incorrect > 0) {
        var message = incorrect + ' error' + (incorrect > 1 ? 's' : '') + ' found.';

        // if search task was successful, save log into file
        if (options.output) {
          grunt.file.write(options.output, outputContent, 'utf8');
        }

        grunt.fail.warn(message);
      }
      else {
        grunt.log.ok(files.length + ' file' + (files.length > 1 ? 's' : '') + ' checked.');
      }

    });
  });



  // ----------------------------------------------------------------
  // Return a formatted verison of the `fileName` string to make it
  // fit perfectly in the `maxSize` width. This format in a "smart"
  // way. It try first to display the file basename, then try to
  // display the first folder of the path, then try to display every
  // folder (starting by the end).
  // ----------------------------------------------------------------
  function formatFileName (fileName, maxSize) {

    // If not larger than the max size, no need to trim
    if (fileName.length <= maxSize) {
      fileName += new Array(maxSize - fileName.length + 1).join(" "); // Append remaining spaces
      return fileName;
    }


    // Otherwise we need to trim the fileName
    var fileNameParts = fileName.split('/');
    var firstPart = fileNameParts[0];
    var lastPart = fileNameParts[fileNameParts.length - 1]; // base name


    // If even the file basename (with ellipsis) is larger than the maxSize, we trim it
    if (lastPart.length + 3 > maxSize) {
      return '...' + lastPart.substring(lastPart.length - maxSize + 3, lastPart.length);
    }


    // If the first part and the last part are larger than the maxSize, we
    // just put the last part with ellipsis
    if (firstPart.length + lastPart.length + 5 > maxSize) { // The '+5' is for the '/.../' string between the first and the last part
      return '...' + lastPart.substring(lastPart.length - maxSize + 3, lastPart.length);
    }


    // If we have enough space to put the first part and the last part
    // we display the first part, the last part and as much other parts
    // as possible (starting by the end).
    var croppedFileNameStart = firstPart + '/...';
    var croppedFileNameEnd = '';
    var i = fileNameParts.length - 1;
    while (true) {
      var nextPart = fileNameParts[i--];
      if (croppedFileNameEnd.length + nextPart.length + 1 <= maxSize - croppedFileNameStart.length) { // The +1 is for the '/' we append
        croppedFileNameEnd = '/' + nextPart + croppedFileNameEnd;
      }
      else {
        break;
      }
    }

    // We return this cropped version
    fileName = croppedFileNameStart + croppedFileNameEnd;
    fileName += new Array(maxSize - fileName.length + 1).join(" "); // Append remaining spaces
    return fileName;

  }



  // ----------------------------------------
  // Return true if `value` is present in the
  // array `array`
  // ----------------------------------------
  function inArray (value, array) {

    if (typeof array === 'undefined') {
      return false;
    }

    for (var i = 0; i < array.length; i++) {
      if (value === array[i]) {
        return true;
      }
    }

    return false;

  }

};

return exports;
