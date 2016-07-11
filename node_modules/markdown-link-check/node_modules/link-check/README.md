# link-check

Checks whether a hyperlink is alive (`200 OK`) or dead.

## Installation

    npm install --save link-check

## Specification

A link is said to be 'alive' if an HTTP HEAD or HTTP GET for the given URL
eventually ends in a `200 OK` response. To minimize bandwidth, an HTTP HEAD
is performed. If that fails (e.g. with a `405 Method Not Allowed`), an HTTP
GET is performed. Redirects are followed.

## API

### linkCheck(link, callback)

Given a `link` and a `callback`, attempt an HTTP HEAD and possibly an HTTP GET.

Parameters:

* `url` string containing a URL.
* `callback` function which accepts `(err, result)`.
 * `err` an Error object when the operation cannot be completed, otherwise `null`.
 * `result` an object with the following properties:
  * `link` the `link` provided as input
  * `status` a string set to either `alive` or `dead`.
  * `statusCode` the HTTP status code. Set to `0` if no HTTP status code was returned (e.g. when the server is down).
  * `err` any connection error that occurred, otherwise `null`.

## Examples

    'use strict';

    var linkCheck = require('link-check');
    
    linkCheck('http://example.com', function (err, result) {
        if (err) {
            console.error('Error', err);
            return;
        }
        console.log('%s is %s', result.link, result.status);
    });

## Testing

    npm test

## License

See [LICENSE.md](https://github.com/tcort/link-check/blob/master/LICENSE.md)
