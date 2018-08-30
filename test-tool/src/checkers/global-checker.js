const path = require('path');
const fs = require('fs-extra');

const spellChecker = require('./spell-checker');
const contentChecker = require('./content-checker');
const linkChecker = require('./link-checker');
const fileNameChecker = require('./file-name-checker');
const tagsChecker = require('./tags-checker');
const validationChecker = require('./validations-checker');
const { common, linkUtils } = require('../utils');

function initialise() {
  return spellChecker.initialise();
}

/**
 * tutorial-grouping.md should be checked for links only
 * */
const checkTutorialGrouping = async ({ projectPath, interceptors, checkResult, results }) => {
  const filePath = `${projectPath}${path.sep}tutorial-grouping.md`;

  if (!fs.existsSync(filePath)) {
    return;
  }

  const { files, uniqueLinksToFiles } = await common.parseFiles([filePath]);
  const uniqueLinks = Array.from(uniqueLinksToFiles.keys());
  const linkCheckResults = await linkChecker.checkLinks(uniqueLinks, interceptors.onAction);
  results.set(filePath, {
    filePath,
    fileName: path.basename(filePath),
    linksCount: files.get(filePath).linksCount,
    fileNameCheckResult: null,
    spellCheckResult: null,
    contentCheckResult: null,
    tagsCheckResult: null,
    validationsCheckResult: null,
    linkCheckResult: [],
  });
  linkCheckResults.forEach(result => {
    return setLinkCheckResult({
      linkCheckResult: result,
      checkResult,
      results,
      files,
      uniqueLinksToFiles,
    });
  });
};

const setLinkCheckResult = ({ linkCheckResult, checkResult, uniqueLinksToFiles, files, results }) => {
  const filesPaths = uniqueLinksToFiles.get(linkCheckResult.link);
  if (filesPaths) {
    filesPaths.forEach(filePath => {
      const { contentLines } = files.get(filePath);
      const isTutorialDoc = common.isTutorialDoc(filePath);
      const isTrusted = isTutorialDoc || linkCheckResult.isTrusted;
      const fileLinkResult = { ...linkCheckResult, isTrusted };
      if (checkResult.passed && !isTrusted) {
        checkResult.passed = isTrusted;
      }
      contentLines.some((line, ind) => {
        if (line.includes(linkCheckResult.link)) {
          fileLinkResult.line = ind + 1;
          return true;
        }
      });
      const fileResult = results.get(filePath);
      fileResult.linkCheckResult.push(fileLinkResult);
    });
  }
};

const check = async (filePaths, projectPath, isProduction = false, interceptors = {}) => {
  const checkResult = {
    passed: true,
  };
  const results = new Map();

  const { files, uniqueLinksToFiles } = await common.parseFiles(filePaths);

  const uniqueLinks = Array.from(uniqueLinksToFiles.keys());

  if (interceptors.onStart) interceptors.onStart({ actionsCount: uniqueLinks.length + filePaths.length });

  const linkCheckPromise = linkChecker.checkLinks(uniqueLinks, interceptors.onAction);

  for (let filePath of filePaths) {
    const { content, contentLines, linksCount } = files.get(filePath);
    const fileName = path.basename(filePath);
    const fileProjectPath = filePath.replace(`${projectPath}${path.sep}`, '');

    const fileNameCheckResult = fileNameChecker.checkFilePath(fileName, fileProjectPath);
    const spellCheckResult = spellChecker.checkSpellingSrc(content);
    const validationsCheckResult = validationChecker.check(filePath, content, isProduction);
    const {
      contentCheckResult,
      tagsCheckResult,
      stepSpellCheckResult,
    } = contentChecker.check(filePath, contentLines);

    if (checkResult.passed) {
      checkResult.passed = !(fileNameCheckResult
        || spellCheckResult.length
        || stepSpellCheckResult.length
        || contentCheckResult.length
        || tagsCheckResult.length
        || validationsCheckResult.length);
    }

    results.set(filePath, {
      fileName,
      filePath,
      linksCount,
      fileNameCheckResult,
      contentCheckResult,
      tagsCheckResult: tagsCheckResult.length > 0 ? tagsCheckResult : null,
      validationsCheckResult,
      spellCheckResult: spellCheckResult.concat(stepSpellCheckResult),
      linkCheckResult: [],
    });
    if (interceptors.onAction) {
      interceptors.onAction();
    }
  }

  const linksCheckResults = await linkCheckPromise;

  linksCheckResults.forEach(result => {
    return setLinkCheckResult({
      linkCheckResult: result,
      checkResult,
      results,
      files,
      uniqueLinksToFiles,
    });
  });

  if (isProduction) {
    await checkTutorialGrouping({
      projectPath,
      interceptors,
      checkResult,
      results,
    });
  }

  return {
    results: Array.from(results.values()),
    passed: checkResult.passed,
  };
};

module.exports = {
  initialise,
  check,
};
