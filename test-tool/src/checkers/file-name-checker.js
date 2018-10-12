const path = require('path');

const { constraints, regexp } = require('../constants');
const { words } = require('../../config/file.name.stop.words.json');

const checkFilePath = (filename, fileProjectPath) => {
    const { filePath: { maxFoldersDepth, includedFolders }, fileName: { maxChunks, maxLength } } = constraints;

    const folders = fileProjectPath.split(path.sep);
    const fileNameFromPath = folders.pop().replace('.md', '');
    const clearFileName = filename.replace('.md', '');
    var fileNameChunks = clearFileName.split('-');

    const containsIncludedFolder = !!includedFolders.find(includedFolder => folders.includes(includedFolder));

    if(folders.length > maxFoldersDepth && containsIncludedFolder) {
        return 'there is no subfolder for tutorials allowed';
    }

    if(folders.length === maxFoldersDepth && containsIncludedFolder) {
        const fileFolder = folders[folders.length - 1];
        if(fileFolder !== clearFileName || clearFileName !== fileNameFromPath) {
            return 'foldername unlike filename';
        }
    }

    return checkFileName(clearFileName);
}

checkFileName = (fileName, existingFileNames = []) => {
    const { fileName: { maxChunks, maxLength } } = constraints;

    const fileNameChunks = fileName.split('-');

    if (fileName.endsWith('-')) {
        return 'file name cannot end with the dash';
    }

    if (fileName.match(regexp.fileName.restrictedSymbols)) {
        return 'no underscores or umlauts or uppercase letters allowed';
    }

    if (fileName.length > maxLength) {
        return `no filenames with > ${maxLength} chars allowed`;
    }

    if (fileNameChunks.length > maxChunks) {
        return `allow only up to ${maxChunks} unqiue keywords`;
    }

    if(words.find(word => fileNameChunks.includes(word))) {
        return "no common English stop words allowed";
    }

    if(existingFileNames.includes(fileName)) {
        return 'filename already exists in wip or tutorials folder';
    }

    return null;
}

module.exports = {
    checkFilePath,
    checkFileName,
};
