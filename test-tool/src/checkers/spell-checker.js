const md = require('markdown-spellcheck').default;

const { list } = require('../../config/spelling.white.list.json');

const { options: { spellCheckOptions } } = require('../constants');

function initialise() {
  md.spellcheck.initialise(spellCheckOptions);
  list.forEach(word => md.spellcheck.addWord(word));
}

const generateReport = (result, errors) => result.split(/\r?\n/)
  .filter(err => !!err.trim())
  .map((err, index) => {
    err = err.trim();
    console.log(err);
    if (err) {
      return {
        line: err.split('|')[0].trim(),
        reason: errors[index].word,
      };
    }

    return null;
  });


const checkSpelling = (filePath) => {
  const result = md.spellFile(filePath, spellCheckOptions);
  const report = md.generateFileReport('', result);
  return generateReport(report, result.errors);
};

const checkSpellingSrc = (src) => {
  const result = {
    errors: md.spell(src, spellCheckOptions),
    src,
  };
  return generateReport(md.generateFileReport('', result), result.errors);
};

module.exports = {
  initialise,
  checkSpelling,
  checkSpellingSrc,
};
