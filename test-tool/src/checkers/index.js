const contentChecker = require('./content-checker');
const linkChecker = require('./link-checker');
const spellChecker = require('./spell-checker');
const fileNameChecker = require('./file-name-checker');
const validationsChecker = require('./validations-checker');
const tagsChecker = require('./tags-checker');
const globalChecker = require('./global-checker');

module.exports = {
    contentChecker,
    linkChecker,
    spellChecker,
    fileNameChecker,
    validationsChecker,
    tagsChecker,
    globalChecker,
};