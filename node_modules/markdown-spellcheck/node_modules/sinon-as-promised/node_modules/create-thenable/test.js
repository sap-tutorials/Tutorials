'use strict'

var test = require('tape')
var Bluebird = require('bluebird')
var Q = require('q')
var createThenable = require('./')

test(function (t) {
  t.test('Bluebird', function (t) {
    return createThenable(Bluebird, function (resolve) {
      resolve('foo')
    })
    .tap()
    .then(function (value) {
      t.equal(value, 'foo')
      t.end()
    })
  })
  t.test('Q', function (t) {
    return createThenable(Q.Promise, function (resolve) {
      resolve('foo')
    })
    .finally(function () {})
    .then(function (value) {
      t.equal(value, 'foo')
      t.end()
    })
  })
})
