var test = require('tape')
var cliArgs = require('../')

test('getOpt short notation: two flags, one option', function (t) {
  var optionDefinitions = [
    { name: 'flagA', alias: 'a' },
    { name: 'flagB', alias: 'b' },
    { name: 'three', alias: 'c' }
  ]

  var argv = [ '-abc', 'yeah' ]
  t.deepEqual(cliArgs(optionDefinitions, argv), {
    flagA: true,
    flagB: true,
    three: 'yeah'
  })
  t.end()
})

test('option=value notation: two plus a regular notation', function (t) {
  var optionDefinitions = [
    { name: 'one' },
    { name: 'two' },
    { name: 'three' }
  ]

  var argv = [ '--one=1', '--two', '2', '--three=3' ]
  var result = cliArgs(optionDefinitions, argv)
  t.strictEqual(result.one, '1')
  t.strictEqual(result.two, '2')
  t.strictEqual(result.three, '3')
  t.end()
})
