var util = require('util');

// colours
var RED = '\033[31m';
var YELLOW = '\033[0;33m';
var GREEN = '\x1b[32m';
var LIGHT_GREEN = '\033[1;32m';
var RESET = '\033[0m';

/**
 * Print every message in new line
 * @constructor
 */
function MultiLine()
{

    MultiLine.prototype.parse_and_print = function(msg, colour)
    {
        var out = [];
        for (var i = 0; i < msg.length; i++)
        {
            var msg_item = msg[i];
            if (typeof msg_item == 'object')
                out.push(colour + RESET + util.inspect(msg_item, false, 50, true) + RESET + colour);
            else
                out.push(msg_item);
        }
        this.print(colour + out.join(' ') + colour + " " + RESET);
    };

    MultiLine.prototype.info = function(msg)
    {
        this.parse_and_print(msg, GREEN);
    };

    MultiLine.prototype.mark = function(msg)
    {
        this.parse_and_print(msg, LIGHT_GREEN);
    };

    MultiLine.prototype.error = function(msg)
    {
        this.parse_and_print(msg, RED);
    };

    MultiLine.prototype.warn = function(msg)
    {
        this.parse_and_print(msg, YELLOW);
    };

    MultiLine.prototype.print = function(msg)
    {
        console.log(msg);
    };
}

/**
 * Print every message in single line
 * @constructor
 */
function SingleLine()
{
    SingleLine.prototype.print = function(msg)
    {
        var result = check();
        if (result.success)
        {
            process.stdout.clearLine();
            process.stdout.cursorTo(0);
            process.stdout.write(msg);
        }
        else
            MultiLine.prototype.print('missed method: ' + result.required_method + ' ' + msg);
    };

    SingleLine.prototype.info = function(msg)
    {
        this.parse_and_print(arguments, GREEN);
    };

    SingleLine.prototype.mark = function(msg)
    {
        this.parse_and_print(arguments, LIGHT_GREEN);
    };

    SingleLine.prototype.error = function(msg)
    {
        this.parse_and_print(arguments, RED);
    };

    SingleLine.prototype.warn = function(msg)
    {
        this.parse_and_print(arguments, YELLOW);
    };

    function check()
    {
        var check = ['clearLine', 'cursorTo', 'write'];
        for (var i = 0; i < check.length; i++)
        {
            var method = check[i];
            if (!(method in process.stdout))
                return {required_method: method};
        }
        return {success: true};
    }
}

SingleLine.prototype = new MultiLine();

/**
 * @type {SingleLine}
 */
exports.SingleLine = SingleLine;

/**
 *
 * @type {MultiLine}
 */
exports.MultiLine = MultiLine;