switch (process.env.VERSION) {
  case '6':
    console.log('Running v6 tests')
    require('./v6')
    break
  case '5':
    console.log('Running v5 tests')
    require('./v5')
    break
  case '4':
    console.log('Running v4 tests')
    require('./v4')
    break
  case 'iojs':
    console.log('Running iojs tests')
    require('./iojs')
    break
  case '0.12':
    console.log('Running v0.12 tests')
    require('./v0.12')
    break
  case '0.10':
    console.log('Running v0.10 tests')
    require('./v0.10')
    break
}
