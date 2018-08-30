'use strict';

const path = require('path');
const fs = require('fs');
const md = require('markdown-spellcheck').default;

const tagChecker = require('./tags-checker');
const { regexp, constraints, options: { spellCheckOptions } } = require('../constants');

const fileExistsSyncCS = (filePath) => {
  const dir = path.dirname(filePath);
  const fileNames = fs.readdirSync(dir);
  return fileNames.includes(path.basename(filePath));
};

const checkLocalImage = (absImgPath, imgName) => {
  const result = [];
  const { content: { mdnImg } } = regexp;
  const exists = fileExistsSyncCS(absImgPath);
  if (exists) {
    const { size } = fs.statSync(absImgPath);
    if (size > constraints.img.MB) {
      result.push(`${mdnImg.messages.size} -> ${imgName}`);
    }
  } else {
    result.push(`${mdnImg.messages.existence} -> ${imgName}`);
  }
  return result;
};

const checkStepSpelling = (line) => {
  const content = line.replace(/[[]()]?/g, '');

  return md.spell(content, spellCheckOptions);
};

module.exports = {
  check: (filePath, lines) => {
    let isCodeBlock = false;
    const result = {
      contentCheckResult: [],
      tagsCheckResult: [],
      stepSpellCheckResult: [],
    };
    const dir = path.dirname(filePath);
    const { content: { common, link, h1, mdnImg } } = regexp;
    // true because meta is in the very beginning
    let isMeta = true;
    let metaBoundaries = 0;

    lines.forEach((line, index) => {
      if (isMeta) {
        if (line.replace(/\n/g, '') === '---') {
          metaBoundaries += 1;
        }
        if (metaBoundaries > 2) {
          isMeta = false;
        }

        const primaryTagError = tagChecker.checkPrimaryTag(line, index);
        const xpTagError = tagChecker.checkExperienceTag(line, index);

        if (primaryTagError) {
          result.tagsCheckResult.push({
            msg: primaryTagError,
            line: index + 1,
          });
        }

        if (xpTagError) {
          result.tagsCheckResult.push({
            msg: xpTagError,
            line: index + 1,
          });
        }
      }

      if (line.includes('```')) {
        isCodeBlock = !isCodeBlock;
      }

      common.forEach(({ regexp, message }) => {
        const match = line.match(regexp);
        if (match) {
          result.contentCheckResult.push({
            line: index + 1,
            msg: `${message} -> ${match[0]}`,
          });
        }
      });

      const match = line.match(mdnImg.regexp);

      if (match) {
        const [, imgName] = match;
        const filePath = path.join(dir, imgName);
        const errors = checkLocalImage(filePath, imgName);
        result.contentCheckResult.push(...errors.map(err => ({
          line: index + 1,
          msg: err,
        })));
      }

      if (!isCodeBlock) {
        if (!isMeta) {
          // plain text URLs are allowed in meta
          const match = line.match(link.regexp);
          if (match) {
            result.contentCheckResult.push({
              line: index + 1,
              msg: `${link.message} -> ${match[0]}`,
            });
          }
        }
        const h1Match = line.match(h1.regexp);
        if (h1Match) {
          result.contentCheckResult.push({
            line: index + 1,
            msg: `${h1.message} -> ${h1Match[0]}`,
          });
        }
      }

      const { step } = regexp;
      const stepMatch = line.match(step);

      if (stepMatch) {
        const errors = checkStepSpelling(stepMatch[0]);

        result.stepSpellCheckResult.push(...errors.map(err => ({
          line: index + 1,
          reason: err.word,
        })));
      }
    });

    return result;
  },

};
