const testTool = require('./test-tool/src');

const { NODE_ENV } = process.env;
const isProduction = NODE_ENV == 'production';

testTool.run(__dirname, isProduction).then(result => {
  if(result.passed) {
    process.exit(0);
  } else {
    process.exit(1);
  }
});