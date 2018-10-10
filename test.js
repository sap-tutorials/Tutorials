var testTool = require('./test-tool/src');
var checkNodeV = require('./check-node-v');

var NODE_ENV = process.env.NODE_ENV;
var isProduction = NODE_ENV == 'production';

checkNodeV().then(() => {
  testTool.run(__dirname, isProduction).then(result => {
    if (result.passed) {
      process.exit(0);
    } else {
      process.exit(1);
    }
  }).catch((error) => {
    console.log(error);
    process.exit(1);
  });
});
