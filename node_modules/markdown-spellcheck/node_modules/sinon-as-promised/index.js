'use strict'

var Promise = require('native-promise-only')
var sinon = require('sinon')
var createThenable = require('create-thenable')

function resolves (value) {
  return this.returns(createThenable(Promise, function (resolve) {
    resolve(value)
  }))
}

sinon.stub.resolves = resolves
sinon.behavior.resolves = resolves

function rejects (err) {
  if (typeof err === 'string') {
    err = new Error(err)
  }
  return this.returns(createThenable(Promise, function (resolve, reject) {
    reject(err)
  }))
}

sinon.stub.rejects = rejects
sinon.behavior.rejects = rejects

module.exports = function (_Promise_) {
  if (typeof _Promise_ !== 'function') {
    throw new Error('A Promise constructor must be provided')
  } else {
    Promise = _Promise_
  }
  return sinon
}
