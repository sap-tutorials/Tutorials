var test = require('tape')
var detect = require('feature-detect-es6')
var Definitions

if (detect.all('class', 'arrowFunction', 'newArrayFeatures')) {
  Definitions = require('../lib/definitions')
} else {
  require('core-js/es6/array')
  Definitions = require('../es5/definitions')
}

test('.createOutput()', function (t) {
  var definitions = new Definitions([ { name: 'one', defaultValue: 'eins' } ])
  t.deepEqual(definitions.createOutput(), { one: 'eins' })
  t.end()
})

test('.get()', function (t) {
  var definitions = new Definitions([ { name: 'one', defaultValue: 'eins' } ])
  t.strictEqual(definitions.get('--one').name, 'one')
  t.end()
})

test('.validate()', function (t) {
  t.throws(function () {
    var definitions = new Definitions([ { name: 'one' }, { name: 'one' } ])
  })
  t.end()
})
