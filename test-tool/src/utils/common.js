const fs = require('fs-extra');
const path = require('path');
const recursive = require('recursive-readdir');

const linkUtils = require('./link');
const { regexp: { validation: { codeBlock, codeLine } } } = require('../constants');

const isTutorialDoc = filePath => ['contributing.md', 'docs-tutorial'].reduce((flag, exclusion) => flag || filePath.includes(exclusion), false);

const readFilesFactory = (projectPath, relativePath, exclusions) => () => recursive(path.join(projectPath, relativePath), exclusions);

const getContentLines = content => content.replace(/\r\n|\n\r|\n|\r/g, '\n').split('\n');

/**
 * @description Replaces code lines with empty strings
 * code blocks with empty lines according to code block size
 * */
const getNoCodeContentLines = (content) => {
    let clearContent = content;
    let result;

    while ((result = codeBlock.exec(content)) !== null) {
        const newLines = result[0]
          .split('\n')
          .slice(1)
          .fill('\n', 0)
          .join('');
        clearContent = clearContent.replace(result[0], newLines);
    }

    clearContent = removeCodeLines(clearContent);

    return getContentLines(clearContent);
};

const removeCodeLines = (content) => {
    let result;

    let clearContent = content;
    while ((result = codeLine.exec(content)) !== null) {
        const match = result[2];
        const spaces = match.split('').fill(' ').join('');
        clearContent = clearContent.replace(match, spaces);
    }

    return clearContent;
};

const parseFiles = async (filePaths) => {
    const files = new Map();
    const uniqueLinksToFiles = new Map();
    for (let filePath of filePaths) {
        const content = await fs.readFile(filePath, 'utf8');
        const contentLines = getContentLines(content);
        const noCodeContentLines = getNoCodeContentLines(content);

        const links = linkUtils.extractLinks(content);
        links.forEach(link => {
            if (uniqueLinksToFiles.has(link)) {
                const filePathsToLink = uniqueLinksToFiles.get(link);
                filePathsToLink.push(filePath);
            } else {
                uniqueLinksToFiles.set(link, [filePath]);
            }
        });

        files.set(filePath, {
            content,
            contentLines,
            noCodeContentLines,
            linksCount: links.length,
        });
    }

    return {
        files,
        uniqueLinksToFiles,
    };
};

const findDuplicates = (arr) => {
    const map = new Map();
    const result = [];
    arr.forEach(el => {
        let counter = map.has(el) ? map.get(el) : 0;
        map.set(el, ++counter);
    });
    map.forEach((value, key) => {
        if (value > 1) {
            result.push(key);
        }
    });
    return result;
};

function orderByAlphabet(errors) {
    return errors.sort((a, b) => a.fileName.localeCompare(b.fileName))
}

module.exports = {
    parseFiles,
    findDuplicates,
    isTutorialDoc,
    readFilesFactory,
    orderByAlphabet,
    removeCodeLines,
};
