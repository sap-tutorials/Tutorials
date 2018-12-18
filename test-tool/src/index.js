const path = require('path');
const ProgressBar = require('progress');

const { consoleEntry } = require('./entries');
const { consoleReporter } = require('./reporters');
const { globalChecker } = require('./checkers');
const { logTemplates } = require('./constants');

globalChecker.initialise();

const run = async (projectPath, isProdRunMode) => {
  const interceptors = {
    onAction: null,
    onStart: null,
  };

  const { filePaths, withProgressTracker } = await consoleEntry.interact(projectPath);
  const filteredPaths = filePaths.filter(filePath => path.basename(filePath).toLowerCase() !== 'readme.md');
  if (withProgressTracker) {
    interceptors.onStart = ({ actionsCount }) => {
      const bar = new ProgressBar(logTemplates.progressBar, { total: actionsCount, width: 60 });
      interceptors.onAction = () => bar.tick();
    };
  }
  const checkResult = await globalChecker.check(filteredPaths, projectPath, isProdRunMode, interceptors);
  const report = consoleReporter.generateReport(checkResult, filePaths.length);
  consoleReporter.outputReportToConsole(report, filePaths.length);
  return {
    passed: checkResult.passed,
  };
};

const runSpecific = async (filePaths, projectPath, isProdRunMode, interceptors = {}) => {
  await globalChecker.initialise();
  const checkResult = await globalChecker.check(filePaths, projectPath, isProdRunMode, interceptors);
  const report = consoleReporter.generateReport(checkResult, filePaths.length);

  consoleReporter.outputReportToConsole(report, filePaths.length);
  return {
    passed: checkResult.passed,
  };
};

module.exports = {
  run,
  runSpecific,
};
