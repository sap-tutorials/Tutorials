'use strict'

var concat = require('unique-concat')
var omit = require('object-omit')

module.exports = function createThenable (Promise, resolver) {
  return methods(Promise).reduce(createMethod, {then: then})
  function createMethod (thenable, name) {
    thenable[name] = method(name)
    return thenable
  }
  function method (name) {
    return function () {
      var promise = this.then()
      return promise[name].apply(promise, arguments)
    }
  }
  function then (/*onFulfill, onReject*/) {
    var promise = new Promise(resolver)
    return promise.then.apply(promise, arguments)
  }
}

function methods (Promise) {
  return concat(['catch', 'finally'], Object.keys(omit(Promise.prototype, 'then')))
}
