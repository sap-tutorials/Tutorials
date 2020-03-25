const path = require('path');
const fsDefault = require('fs');
const util = require('util');
const fs = require('fs-extra');

const spellChecker = require('./spell-checker');
const contentChecker = require('./content-checker');
const linkChecker = require('./link-checker');
const optionsChecker = require('./options-checker');
const fileNameChecker = require('./file-name-checker');
const validationChecker = require('./validations-checker');
const { common } = require('../utils');

function initialise() {
  return spellChecker.initialise();
}

const tutorialsDirName = 'tutorials';
const readDirAsync = util.promisify(fsDefault.readdir.bind(fsDefault));

const getAllTutorials = (folderPath) => {
  return readDirAsync(path.join(folderPath, tutorialsDirName));
};

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

const setLinkCheckResult = (options) => {
  const { linkCheckResult, checkResult, uniqueLinksToFiles, files, results } = options;
  const filesPaths = uniqueLinksToFiles.get(linkCheckResult.link);
  if (filesPaths) {
    filesPaths.forEach((filePath) => {
      const { noCodeContentLines: contentLines } = files.get(filePath);
      const isTutorialDoc = common.isTutorialDoc(filePath);
      const isTrusted = isTutorialDoc || linkCheckResult.isTrusted;
      const fileLinkResult = {
        ...linkCheckResult,
        isTrusted
      };
      if (checkResult.passed && !isTrusted) {
        checkResult.passed = isTrusted;
      }
      const foundLines = contentLines.reduce((result, line, ind) => {
        if (line.includes(linkCheckResult.link)) {
          result.push({ ...fileLinkResult, line: ind + 1 });
        }

        return result;
      }, []);
      const fileResult = results.get(filePath);
      fileResult.linkCheckResult.push(...foundLines);
    });
  }
};

const check = async (filePaths, projectPath, isProduction = false, interceptors = {}) => {
  const checkResult = {
    passed: true,
  };

  // remove not existing files, case: tool ran on the last commit where some file was renamed / deleted
  for (let filePath of filePaths) {
    if (!fs.existsSync(filePath)) {
      filePaths = filePaths.filter(f => f!== filePath);
    }
  }

  const results = new Map();

  const { files, uniqueLinksToFiles } = await common.parseFiles(filePaths);

  const uniqueLinks = Array.from(uniqueLinksToFiles.keys());

  if (interceptors.onStart) interceptors.onStart({ actionsCount: uniqueLinks.length + filePaths.length });

  const linkCheckPromise = linkChecker.checkLinks(uniqueLinks, interceptors.onAction);

  const allTutorials = await getAllTutorials(projectPath);

  await Promise.all(filePaths.map(async (filePath) => {
    const { content, contentLines, linksCount } = files.get(filePath);
    const fileName = path.basename(filePath);
    const fileProjectPath = filePath.replace(`${projectPath}${path.sep}`, '');

    const fileNameCheckResult = fileNameChecker.checkFilePath(fileName, fileProjectPath);
    const spellCheckResult = spellChecker.checkSpellingSrc(content);
    const validationsCheckResult = validationChecker.check(filePath, content, isProduction);
    const optionsCheckResult = optionsChecker.check(filePath, content);

    const {
      contentCheckResult,
      tagsCheckResult,
      stepSpellCheckResult,
      syntaxCheckResult,
      linkCheckResult: contentLinkCheckResult,
    } = await contentChecker.check(filePath, contentLines, allTutorials);

    if (checkResult.passed) {
      checkResult.passed = !(fileNameCheckResult
        || spellCheckResult.length
        || stepSpellCheckResult.length
        || contentCheckResult.length
        || tagsCheckResult.length
        || syntaxCheckResult.length
        || contentLinkCheckResult.length
        || optionsCheckResult.length
        || validationsCheckResult.length);
    }

    results.set(filePath, {
      fileName,
      filePath,
      linksCount,
      fileNameCheckResult,
      contentCheckResult: [...contentCheckResult, ...optionsCheckResult],
      tagsCheckResult: tagsCheckResult.length > 0 ? tagsCheckResult : null,
      validationsCheckResult,
      syntaxCheckResult,
      spellCheckResult: spellCheckResult.concat(stepSpellCheckResult),
      linkCheckResult: contentLinkCheckResult,
    });
    if (interceptors.onAction) {
      interceptors.onAction();
    }
  }));

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

  if (!isProduction) {
    await checkTutorialGrouping({
      projectPath,
      interceptors,
      checkResult,
      results,
    });
  }

  return {
    results: common.orderByAlphabet(Array.from(results.values())),
    passed: checkResult.passed,
  };
};

module.exports = {
  initialise,
  check,
};
