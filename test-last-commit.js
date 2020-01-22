'use strict';

const gitLog = require('gitlog');
const testTool = require('./test-tool/src');

const { NODE_ENV } = process.env;
const isProduction = NODE_ENV === 'production';

const options = {
  repo: __dirname,
  number: 1,
  execOptions: {
    maxBuffer: 1024 * 1024,
  },
};

gitLog(options, function (error, commits) {
  if (error) {
    console.log(error);
    process.exit(1);
  }

  if (commits.length === 0) {
    console.log('No commits. Exiting.');
    process.exit(0);
  }

  const [lastCommit] = commits;
  const { files } = lastCommit;
  const mdFiles = files.filter(f =>
    f.toLowerCase().endsWith('.md')
    && !f.includes('work-in-progress')
    && f.includes('tutorials')
  );

  if (mdFiles.length > 0) {
    return testTool
      .runSpecific(mdFiles, __dirname, isProduction)
      .then(result => {
        if (result.passed) {
          process.exit(0);
        } else {
          process.exit(1);
        }
      }).catch((error) => {
        console.log(error);
        process.exit(1);
      });
  } else {
    console.log('No .md files were changed in the last commit. Exiting.');
    process.exit(0);
  }
});
