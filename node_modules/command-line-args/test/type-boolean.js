var test = require('tape')
var cliArgs = require('../')

var optionDefinitions = [
  { name: 'one', type: Boolean }
]

test('type-boolean: different values', function (t) {
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one' ]),
    { one: true }
  )
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', 'true' ]),
    { one: true }
  )
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', 'false' ]),
    { one: true }
  )
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', 'sfsgf' ]),
    { one: true }
  )

  t.end()
})
