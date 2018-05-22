const path = require('path');
const md = require('markdown-spellcheck').default;

const { list } = require('../../config/spelling.white.list.json');

const options = {
    ignoreAcronyms: true,
    ignoreNumbers: true,
    suggestions: false,
    dictionary: {
        file: path.join(__dirname, '../../config/dictionary/en-us'),
    }
};

md.spellcheck.initialise(options);
list.forEach(word => md.spellcheck.addWord(word));

const generateReport = result => result.split(/\r?\n/)
                                       .map(err => err.trim())
                                       .filter(err => err)
                                       .map(err => err.split('|').map(el => el.trim()))
                                       .map(([line, reason]) => ({ line, reason }));

const checkSpelling = (filePath) => {
    const errors = md.spellFile(filePath, options);
    const result = md.generateFileReport('', errors);
    return generateReport(result);
};

const checkSpellingSrc = (src) => {
    const result = {
        errors: md.spell(src, options),
        src: src
    };
    return generateReport(md.generateFileReport('', result));
}

module.exports = {
    checkSpelling,
    checkSpellingSrc,
};
