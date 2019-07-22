'use strict';

const path = require('path');
const fs = require('fs');

const tagChecker = require('./tags-checker');
const { regexp, constraints } = require('../constants');

const fileExistsSyncCS = (filePath) => {
  const dir = path.dirname(filePath);
  const fileNames = fs.readdirSync(dir);
  return fileNames.includes(path.basename(filePath));
};

const checkLocalTutorial = (name, allTutorials) => allTutorials.includes(name);

const checkLocalImage = (absImgPath, imgName) => {
  const result = [];
  const { content: { mdnImg } } = regexp;

  try {
    const exists = fileExistsSyncCS(absImgPath);

    if (exists) {
      const { size } = fs.statSync(absImgPath);
      if (size > constraints.img.MB) {
        result.push(`${mdnImg.messages.size} -> ${imgName}`);
      }
    } else {
      result.push(`${mdnImg.messages.existence} -> ${imgName}`);
    }
  } catch (e) {
    result.push(e.message);
  }

  return result;
};

module.exports = {
  check: async (filePath, lines, allTutorials) => {
    let isCodeBlock = false;
    const result = {
      contentCheckResult: [],
      tagsCheckResult: [],
      stepSpellCheckResult: [],
    };
    const dir = path.dirname(filePath);
    const {
      content: {
        common,
        link,
        emptyLink,
        h1,
        mdnImg,
        localFileLink,
        tutorialLink,
        tutorialLinkInvalid,
      },
      validation: { accordions, codeLine },
    } = regexp;
    // true because meta is in the very beginning
    let isMeta = true;
    let metaBoundaries = 0;

    lines.forEach((line, index) => {
      const isCodeLine = (line.match(codeLine) || []).length > 0;

      if (isMeta) {
        if (line.replace(/\n/g, '') === '---') {
          metaBoundaries += 1;
        }
        if (metaBoundaries >= 2) {
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

      if (line.trim()
        .startsWith('```')) {
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

      if (!isCodeBlock) {
        const imageMatch = line.match(mdnImg.regexp);
        const localFileMatch = line.match(localFileLink.regexp);
        const tutorialLinkInvalidMatch = line.match(tutorialLinkInvalid.regexp);
        const tutorialLinkMatch = line.match(tutorialLink.regexp);
        const accordionMatch = line.match(accordions);
        if (!isMeta) {
          // plain text URLs are allowed in meta
          const match = line.match(link.regexp);
          if (match) {
            result.contentCheckResult.push({
              line: index + 1,
              msg: `${link.message} -> ${match[0]} -> ${link.description}`,
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

        if (!isCodeLine) {
          emptyLink.forEach((i) => {
            const match = line.match(i.regexp);
            if (match) {
              result.contentCheckResult.push({
                line: index + 1,
                msg: `${i.message} -> ${match[0]} -> ${i.description}`,
              });
            }
          });

          const stepName = line.includes('[ACCORDION');

          if (tutorialLinkMatch && !stepName) {
            const [, tutorialName] = tutorialLinkMatch[0]
              .replace(/\)/g, '')
              .split('](');
            const exists = checkLocalTutorial(tutorialName, allTutorials);

            if (!exists) {
              result.contentCheckResult.push({
                line: index + 1,
                msg: `${tutorialLink.message} (${tutorialName})`,
              });
            }
          }

          if (tutorialLinkInvalidMatch) {
            result.contentCheckResult.push({
              line: index + 1,
              msg: tutorialLinkInvalid.message,
            });
          }

          if (imageMatch) {
            const [, imgName] = imageMatch;
            const filePath = path.join(dir, imgName);
            const errors = checkLocalImage(filePath, imgName);
            result.contentCheckResult.push(...errors.map(err => ({
              line: index + 1,
              msg: err,
            })));
          }

          if (localFileMatch && !imageMatch && !accordionMatch && !tutorialLinkInvalidMatch) {
            result.contentCheckResult.push({
              line: index + 1,
              msg: localFileLink.message,
            });
          }
        }
      }
    });

    return result;
  },
};
