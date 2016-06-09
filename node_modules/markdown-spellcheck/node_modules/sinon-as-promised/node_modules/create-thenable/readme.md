# create-thenable [![Build Status](https://travis-ci.org/bendrucker/create-thenable.svg?branch=master)](https://travis-ci.org/bendrucker/create-thenable)

> Create a thenable for a given Promise

## Install

```
$ npm install --save create-thenable
```


## Usage

```js
var thenable = require('create-thenable')(require('bluebird'))
return thenable.catch(errHandlers).then(handler)
```

The returned `thenable` has a `then` method that creates a new promise. It also proxies methods from the prototype and guarantees the presence of `catch` and `finally`. When these proxied methods are called, they call `then` to create the promise and then call the method on the result. That way you can do this:

```js
var thenable = createThenable(require('bluebird'), function () {
  resolve('foo')
})


thenable.tap(function () {
  console.log('succeeded')
})
.then(function (value) {
  assert(value, 'foo')
})
```

`thenable` isn't a true Bluebird `Promise` but you can still trigger its methods as if it were. 

## API

#### `createThenable(Promise, resolver)` -> `thenable`

#### Promise

*Required*  
Type: `function`

A Promise constructor

##### resolver

*Required*  
Type: `function`

The resolver function to pass to the Promise constructor

## License

MIT © [Ben Drucker](http://bendrucker.me)
