sinon-as-promised [![Build Status](https://travis-ci.org/bendrucker/sinon-as-promised.svg?branch=master)](https://travis-ci.org/bendrucker/sinon-as-promised)
=================

> Extend [Sinon](https://github.com/cjohansen/sinon.js) stubs with promise stubbing methods.

## Installing
```sh
npm install sinon-as-promised
```

If you're using sinon-as-promised in the browser and are not using Browserify/Webpack, use [3.x](https://github.com/bendrucker/sinon-as-promised/tree/v3.0.1) or earlier.

## Usage

```js
var sinon  = require('sinon')
require('sinon-as-promised')

sinon.stub().resolves('foo')().then(function (value) {
  assert.equal(value, 'foo')
})
```

You'll only need to require sinon-as-promised once. It attaches the appropriate stubbing functions which will then be available anywhere else you require sinon. It defaults to using native ES6 Promise [(or provides a polyfill)](https://github.com/getify/native-promise-only), but you can use another promise library if you'd like, as long as it exposes a constructor:

```js
// Using Bluebird
var Bluebird = require('bluebird')
require('sinon-as-promised')(Bluebird)
```

## API

#### `stub.resolves(value)` -> `stub`


##### value

*Required*  
Type: `any`

When called, the stub will return a "thenable" object which will return a promise for the provided `value`. Any [Promises/A+](https://promisesaplus.com/) compliant library will handle this object properly.

```js
var stub = sinon.stub();
stub.resolves('foo');

stub().then(function (value) {
    // value === 'foo'
});

stub.onCall(0).resolves('bar')
stub().then(function (value) {
    // value === 'bar'
});
```
---

#### `stub.rejects(err)` -> `stub`

##### err

*Required*  
Type: `error` / `string`

When called, the stub will return a thenable which will return a reject promise with the provided `err`. If `err` is a string, it will be set as the message on an `Error` object.

```js
stub.rejects(new Error('foo'))().catch(function (error) {
    // error.message === 'foo'
});
stub.rejects('foo')().catch(function (error) {
    // error.message === 'foo'
});

stub.onCall(0).rejects('bar');
stub().catch(function (error) {
    // error.message === 'bar'
});
```

## Examples

* [angular](https://github.com/bendrucker/sinon-as-promised/tree/master/examples/angular)
* [Bluebird](https://github.com/bendrucker/sinon-as-promised/tree/master/examples/bluebird)
* [Node or Browserify](https://github.com/bendrucker/sinon-as-promised/tree/master/examples/node-browserify)

## License

MIT © [Ben Drucker](http://bendrucker.me)
