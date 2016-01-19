(function() {
  module.exports = function(grunt) {
    var Logger, fs, lineSeparator, _;
    _ = grunt.util._;
    fs = require('fs');
    lineSeparator = require('os').EOL;
    Logger = (function() {
      Logger.prototype.linkCount = 0;

      Logger.prototype.okCount = 0;

      Logger.prototype.failCount = 0;

      Logger.prototype.passCount = 0;

      Logger.prototype.logFilename = "deadlink.log";

      Logger.prototype.progressBar = null;

      function Logger(_arg) {
        var logAll, logFilename, logToFile;
        logToFile = _arg.logToFile, logAll = _arg.logAll, logFilename = _arg.logFilename;
        if (logFilename != null) {
          this.logFilename = logFilename;
        }
        if (logToFile) {
          this.passCount = this.resultLogger = this.errorLogger = this.okLogger = function(msg) {
            return fs.appendFile(this.logFilename, msg + lineSeparator, function(err) {});
          };
        }
        if (!logAll) {
          this.okLogger = function(str) {};
          this.passLogger = function(str) {};
        }
      }

      Logger.prototype.okLogger = grunt.verbose.ok;

      Logger.prototype.errorLogger = grunt.verbose.error;

      Logger.prototype.resultLogger = grunt.log.subhead;

      Logger.prototype.passLogger = grunt.verbose.ok;

      Logger.prototype.pass = function(msg) {
        this.passCount++;
        if (this.progressBar != null) {
          this.progressBar.tick();
        }
        return this.passLogger(msg);
      };

      Logger.prototype.ok = function(msg) {
        this.okCount++;
        if (this.progressBar != null) {
          this.progressBar.tick();
        }
        return this.okLogger(msg);
      };

      Logger.prototype.error = function(msg) {
        this.failCount++;
        if (this.progressBar != null) {
          this.progressBar.tick();
        }
        return this.errorLogger(msg);
      };

      Logger.prototype.progress = function() {
        var ProgressBar;
        if (grunt.option('verbose')) {
          return;
        }
        ProgressBar = require('progress');
        return this.progressBar = new ProgressBar('[:bar]:percent :elapsed', {
          total: 0,
          complete: '#',
          incomplete: ' ',
          width: 40
        });
      };

      Logger.prototype.increaseLinkCount = function() {
        this.linkCount++;
        if (this.progressBar != null) {
          return this.progressBar.total = this.linkCount;
        }
      };

      Logger.prototype.printResult = function(after) {
        var st,
          _this = this;
        return st = setInterval(function() {
          if (_this.linkCount === (_this.okCount + _this.failCount + _this.passCount)) {
            _this.resultLogger("ok : " + _this.okCount + ", error : " + _this.failCount + ", pass : " + _this.passCount);
            clearInterval(st);

            // fail if errors appear
            return after((_this.failCount === 0));
          }
        }, 500);
      };

      return Logger;

    })();
    return Logger;
  };

}).call(this);
