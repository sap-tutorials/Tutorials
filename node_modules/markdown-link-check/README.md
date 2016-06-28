# markdown-link-check

Extracts links from markdown texts and checks whether each link is
alive (`200 OK`) or dead.

## Installation

To add the module to your project, run:

    npm install --save markdown-link-check

To install the command line tool globally, run:

    npm install -g markdown-link-check

## API

### markdownLinkCheck(markdown, callback)

Given a string containing `markdown` formatted text and a `callback`,
extract all of the links and check if they're alive or dead. Call the
`callback` with `(err, results)`

Parameters:

* `markdown` string containing markdown formatted text.
* `callback` function which accepts `(err, results)`.
 * `err` an Error object when the operation cannot be completed, otherwise `null`.
 * `results` an array of objects with the following properties:
  * `link` the `link` provided as input
  * `status` a string set to either `alive` or `dead`.
  * `statusCode` the HTTP status code. Set to `0` if no HTTP status code was returned (e.g. when the server is down).
  * `err` any connection error that occurred, otherwise `null`.

## Examples

### Module

    'use strict';

    var markdownLinkCheck = require('markdown-link-check');
    
    markdownLinkCheck('[example](http://example.com)', function (err, results) {
        if (err) {
            console.error('Error', err);
            return;
        }
        results.forEach(function (result) {
            console.log('%s is %s', result.link, result.status);
        });
    });

### Command Line Tool

The command line tool optionally takes 1 argument, the file name or http/https URL.
If not supplied, the tool reads from standard input.

#### Check links from a local markdown file

    markdown-link-check ./README.md

#### Check links from a markdown file hosted on the web

    markdown-link-check https://github.com/tcort/markdown-link-check/blob/master/README.md

#### Check links from standard input

    cat *.md | markdown-link-check

## Testing

    npm test

## License

See [LICENSE.md](https://github.com/tcort/markdown-link-check/blob/master/LICENSE.md)
