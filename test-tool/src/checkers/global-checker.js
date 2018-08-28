const path = require('path');
const fs = require('fs-extra');

const spellChecker = require('./spell-checker');
const contentChecker = require('./content-checker');
const linkChecker = require('./link-checker');
const fileNameChecker = require('./file-name-checker');
const tagsChecker = require('./tags-checker');
const validationChecker = require('./validations-checker');
const { common, linkUtils } = require('../utils');

const check = async (filePaths, projectPath, isProduction = false, interceptors = {}) => {
    let passed = true;
    const results = new Map();

    const { files, uniqueLinksToFiles } = await common.parseFiles(filePaths);

    const uniqueLinks = Array.from(uniqueLinksToFiles.keys());

    if(interceptors.onStart) interceptors.onStart({ actionsCount: uniqueLinks.length + filePaths.length });

    const linkCheckPromise = linkChecker.checkLinks(uniqueLinks, interceptors.onAction);

    for(let filePath of filePaths) {
        const { content, contentLines, linksCount } = files.get(filePath);
        const fileName = path.basename(filePath);
        const fileProjectPath = filePath.replace(`${projectPath}${path.sep}`, '');

        const fileNameCheckResult = fileNameChecker.checkFilePath(fileName, fileProjectPath);
        const spellCheckResult = spellChecker.checkSpellingSrc(content);
        const contentCheckResult = filePath.includes('tutorials') ? contentChecker.check(filePath, contentLines) : [];
        const validationsCheckResult = validationChecker.check(filePath, content, isProduction);
        const primaryTagsCheckResult = tagsChecker.checkPrimaryTag(content);
        const xpTagsCheckResult = tagsChecker.checkExperienceTag(content);
        let tagsCheckResult;
        if (xpTagsCheckResult) {
            tagsCheckResult = [xpTagsCheckResult];
        }

        if (primaryTagsCheckResult) {
            if (!Array.isArray(tagsCheckResult)) {
              tagsCheckResult = [];
            }
            tagsCheckResult.push(primaryTagsCheckResult);
        }

        if(passed) {
            passed = !(fileNameCheckResult || spellCheckResult.length || contentCheckResult.length || tagsCheckResult || validationsCheckResult.length);
        }
        results.set(filePath, {
            fileName,
            filePath,
            linksCount,
            fileNameCheckResult,
            spellCheckResult,
            contentCheckResult,
            tagsCheckResult,
            validationsCheckResult,
            linkCheckResult: [],
        });
        if(interceptors.onAction) interceptors.onAction();
    }

    const linksCheckResults = await linkCheckPromise;

    linksCheckResults.forEach(result => {
        const filesPaths = uniqueLinksToFiles.get(result.link);
        if(filesPaths) {
            filesPaths.forEach(filePath => {
                const { contentLines } = files.get(filePath);
                const isTutorialDoc = common.isTutorialDoc(filePath);
                const isTrusted = isTutorialDoc || result.isTrusted;
                const fileLinkResult = { ...result, isTrusted };
                if(passed && !isTrusted) {
                    passed = isTrusted;
                }
                contentLines.some((line, ind) => {
                    if(line.includes(result.link)) {
                        fileLinkResult.line = ind + 1;
                        return true;
                    }
                });
                const fileResult = results.get(filePath);
                fileResult.linkCheckResult.push(fileLinkResult);
            });
        }
    });

    return {
        results: Array.from(results.values()),
        passed,
    };
};

module.exports = {
    check,
};
