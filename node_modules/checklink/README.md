# checklink [![Build Status](https://travis-ci.org/imsobear/checklink.svg?branch=master)](https://travis-ci.org/imsobear/checklink)

> check deadlink by nodejs.

## Install

```bash
npm install --save checklink
```

## Example

```javascript
var checklink = require('checklink');

checklink('https://taobao.com')
  .then(function(results) {
    // {isPassed: true, count: 1, deadlinks: []}
    console.log(results);
  });

checklink(['https://taobao.com', 'http://xxxhhhhhh.com'])
  .then(function(results) {
    // {isPassed: false, count: 4, deadlinks:[{url: 'hxhxhhxhx.com.cn', code: 400, message: 'Bad Request'}]}
    console.log(results);
  });
```

## Test

```bash
npm test
```

## License

MIT &copy; 2015 sobear
