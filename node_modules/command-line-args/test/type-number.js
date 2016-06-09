var test = require('tape')
var cliArgs = require('../')

var optionDefinitions = [
  { name: 'one', type: Number }
]

test('type-number: different values', function (t) {
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', '1' ]),
    { one: 1 }
  )
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one' ]),
    { one: null }
  )
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', '-1' ]),
    { one: -1 }
  )
  var result = cliArgs(optionDefinitions, [ '--one', 'asdf' ])
  t.ok(isNaN(result.one))

  t.end()
})
