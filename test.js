var checkNodeV = require('./check-node-v');

var NODE_ENV = process.env.NODE_ENV;
var isProduction = NODE_ENV == 'production';

checkNodeV().then(function (){
  var testTool = require('./test-tool/src');

  testTool.run(__dirname, isProduction).then(result => {
    if (result.passed) {
      process.exit(0);
    } else {
      process.exit(1);
    }
  }).catch(function (error) {
    console.log(error);
    process.exit(1);
  });
});
