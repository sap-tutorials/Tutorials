var loggers = require('./loggers.js');

var multi_line = new loggers.MultiLine();
var single_line = new loggers.SingleLine();

/**
 * Print green message in new line
 * @param msg
 */
exports.info = function(msg)
{
    multi_line.info(arguments);
};

/**
 * Print lite green message in new line
 * @param msg
 */
exports.mark = function(msg)
{
    multi_line.mark(arguments);
};

/**
 * Print red message in new line
 * @param msg
 */
exports.error = function(msg)
{
    multi_line.error(arguments);
};

/**
 * Print yellow message in new line
 * @param msg
 */
exports.warn = function(msg)
{
    multi_line.warn(arguments);
};

/**
 * Print message in single line (clears line before printing)
 * @type {loggers.SingleLine}
 */
exports.single = single_line;