const fs = require('fs-extra');
const path = require('path');

const linkUtils = require('./link');

const isTutorialDoc = filePath => filePath.includes('docs-tutorial') || filePath.includes('contributing.md');

const parseFiles = async (filePaths) => {
    const files = new Map();
    const uniqueLinksToFiles = new Map();
    for(filePath of filePaths) {
        const fileName = path.basename(filePath);
        const content = await fs.readFile(filePath, 'utf8');
        const contentLines = content.split(/\r?\n/);

        const links = linkUtils.extractLinks(content);
        links.forEach(link => {
            if(uniqueLinksToFiles.has(link)) {
                const filePathsToLink = uniqueLinksToFiles.get(link);
                filePathsToLink.push(filePath);
            } else {
                uniqueLinksToFiles.set(link, [filePath]);
            }
        });

        files.set(filePath, {
            content,
            contentLines,
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
        let counter = map.has(el) ? map.get(el) : 0
        map.set(el, ++counter);
    });
    map.forEach((value, key) => {
        if(value > 1) {
            result.push(key);
        }
    });
    return result;
};

module.exports = {
    parseFiles,
    findDuplicates,
    isTutorialDoc,
};