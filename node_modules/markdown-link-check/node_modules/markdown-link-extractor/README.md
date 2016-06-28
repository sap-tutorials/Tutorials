# markdown-link-extractor

Extracts links from markdown texts.

## Installation

    npm install --save markdown-link-extractor

## API

### markdownLinkExtractor(markdown)

Parameters:

* `markdown` text in markdown format.

Returns:

* an array containing the URLs from the links found.

## Examples

    "use strict";

    var fs = require('fs');
    var markdownLinkExtractor = require('markdown-link-extractor');

    var markdown = fs.readFileSync('README.md').toString();

    var links = markdownLinkExtractor(markdown);

    links.forEach(function (link) {
        console.log(link);
    });

## Testing

    npm test

## License

See [LICENSE.md](https://github.com/tcort/markdown-link-extractor/blob/master/LICENSE.md)
