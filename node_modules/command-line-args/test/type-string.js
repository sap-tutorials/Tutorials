var test = require('tape')
var cliArgs = require('../')

var optionDefinitions = [
  { name: 'one', type: String }
]

test('type-string: different values', function (t) {
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', 'yeah' ]),
    { one: 'yeah' }
  )
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one' ]),
    { one: null }
  )
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', '3' ]),
    { one: '3' }
  )

  t.end()
})

/* currently not supported, it would complain --yeah is an invalid option */
test.skip('type-string: pass a value resembling an option', function (t) {
  t.deepEqual(
    cliArgs(optionDefinitions, [ '--one', '--yeah' ]),
    { one: '--yeah' }
  )
  t.end()
})
