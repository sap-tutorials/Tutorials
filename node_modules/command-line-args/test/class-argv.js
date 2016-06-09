var test = require('tape')
var detect = require('feature-detect-es6')
var Argv
var Definitions

if (detect.all('class', 'arrowFunction', 'newArrayFeatures')) {
  Argv = require('../lib/argv')
  Definitions = require('../lib/definitions')
} else {
  require('core-js/es6/array')
  Argv = require('../es5/argv')
  Definitions = require('../es5/definitions')
}

test('.expandOptionEqualsNotation()', function (t) {
  var argv = new Argv([ '--one=1', '--two', '2', '--three=3', '4' ])
  argv.expandOptionEqualsNotation()
  t.deepEqual(argv.list, [
    '--one', '1', '--two', '2', '--three', '3', '4'
  ])
  t.end()
})

test('.expandGetoptNotation()', function (t) {
  var argv = new Argv([ '-abc' ])
  argv.expandGetoptNotation()
  t.deepEqual(argv.list, [
    '-a', '-b', '-c'
  ])
  t.end()
})

test('.expandGetoptNotation() with values', function (t) {
  var argv = new Argv([ '-abc', '1', '-a', '2', '-bc' ])
  argv.expandGetoptNotation()
  t.deepEqual(argv.list, [
    '-a', '-b', '-c', '1', '-a', '2', '-b', '-c'
  ])
  t.end()
})

test('.validate()', function (t) {
  var definitions = new Definitions([
    { name: 'one', type: Number }
  ])

  t.doesNotThrow(function () {
    var argv = new Argv([ '--one', '1' ])
    argv.validate(definitions)
  })

  t.throws(function () {
    var argv = new Argv([ '--one', '--two' ])
    argv.validate(definitions)
  })

  t.throws(function () {
    var argv = new Argv([ '--one', '2', '--two', 'two' ])
    argv.validate(definitions)
  })

  t.throws(function () {
    var argv = new Argv([ '-a', '2' ])
    argv.validate(definitions)
  })

  t.end()
})
