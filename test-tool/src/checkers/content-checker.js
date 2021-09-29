'use strict';

const path = require('path');
const fs = require('fs');

const tagChecker = require('./tags-checker');
const linkChecker = require('./link-checker');
const syntaxChecker = require('./syntax-checker');
const metadataChecker = require('./metadata-checker');
const commonUtils = require('../utils/common');

const { regexp, constraints } = require('../constants');
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
    remoteImage,
    codeBlockInNote,
  },
  validation: { accordions, codeLine, inlineCodeBlock },
  link: {
    pure: pureLink,
  },
} = regexp;

const fileExistsSyncCS = (filePath) => {
  const dir = path.dirname(filePath);
  const fileNames = fs.readdirSync(dir);
  return fileNames.includes(path.basename(filePath));
};

const checkLocalTutorial = (name, allTutorials) => allTutorials.includes(name);

const checkLocalImage = (absImgPath, altText) => {
  const result = [];
  const { content: { mdnImg } } = regexp;

  const imgName = path.basename(absImgPath);

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

    const hasWrongName = altText === imgName;

    if (hasWrongName) {
      result.push(`${mdnImg.messages.wrongAlt} -> ${imgName}`);
    }
  } catch (e) {
    result.push(e.message);
  }

  return result;
};

const replaceCodeLines = (text) => {
  let matches;
  let result = text;
  // eslint-disable-next-line no-cond-assign
  while ((matches = codeLine.exec(text)) !== null) {
    const [, , word] = matches;
    const pureWord = word.replace(/`/g, '');
    result = result.replace(word, pureWord);
  }

  return result;
};

async function makeAsyncChecks({ lines, isMeta, isCodeBlock, index, result }) {
  const originalLine = lines[index];
  const line = replaceCodeLines(originalLine);

  if (!isCodeBlock) {
    const remoteImageMatches = line.match(remoteImage.regexp);

    if (remoteImageMatches) {
      await Promise.all(remoteImageMatches.map(async (item) => {
        // using new RegExp to reset g flag
        const [imageUrl] = item.match(new RegExp(pureLink, 'i'));
        const checkResult = await linkChecker.checkImageLink(imageUrl);

        if (checkResult.error) {
          result.contentCheckResult.push({
            line: index + 1,
            msg: checkResult.error,
          });
        }
      }));
    }
  }

  if (isMeta) {
    const authorProfileMatch = line.match(regexp.link.authorProfile);
    if (authorProfileMatch && authorProfileMatch.length > 0) {
      const authorUrlMatch = line.replace(authorProfileMatch[0], '')
        .trim();

      const authorUrlCheckResult = await metadataChecker.checkAuthorProfile(authorUrlMatch);
      if (authorUrlCheckResult) {
        result.linkCheckResult.push({
          line: index + 1,
          ...authorUrlCheckResult,
        });
      }
    }
  }
}

module.exports = {
  check: async (filePath, lines, allTutorials) => {
    let isCodeBlock = false;
    const result = {
      contentCheckResult: [],
      linkCheckResult: [],
      tagsCheckResult: [],
      stepSpellCheckResult: [],
      syntaxCheckResult: [],
    };
    const dir = path.dirname(filePath);
    // true because meta is in the very beginning
    let isMeta = true;
    let metaBoundaries = 0;
    const metaLines = [];

    await Promise.all(lines.map(async (line, index) => {
      const lineNoCode = replaceCodeLines(line);

      if (lineNoCode.replace(/\n/g, '') === '---') {
        metaBoundaries += 1;
      }
      if (metaBoundaries >= 2) {
        isMeta = false;
        const metaValidationResult = metadataChecker.check(metaLines);
        result.contentCheckResult.push(...metaValidationResult);
      }

      const trimmedLine = line.trim();
      const isCodeInNote = trimmedLine.match(codeBlockInNote);
      const isInlineCodeBlock = trimmedLine.match(inlineCodeBlock);
      if (!isInlineCodeBlock && (trimmedLine.startsWith('```') || isCodeInNote)) {
        isCodeBlock = !isCodeBlock;
      }

      common.forEach(({ regexp, message }) => {
        const match = lineNoCode.match(regexp);
        if (match) {
          result.contentCheckResult.push({
            line: index + 1,
            msg: `${message} -> ${match[0]}`,
          });
        }
      });

      if (!isCodeBlock) {
        const imageMatches = line.match(mdnImg.regexp);
        const localFileMatch = line.match(localFileLink.regexp);
        const tutorialLinkInvalidMatch = line.match(tutorialLinkInvalid.regexp);
        const tutorialLinkMatch = line.match(tutorialLink.regexp);
        const accordionMatch = line.match(accordions);
        if (!isMeta) {
          // plain text URLs are allowed in meta
          const noValidLinksLine = regexp.link.markdown.reduce(re => line.replace(re, ''));
          const match = noValidLinksLine.match(link.regexp);
          if (match) {
            result.contentCheckResult.push({
              line: index + 1,
              msg: `${link.message} -> ${match[0]} -> ${link.description}`,
            });
          }
        }
        const h1Match = lineNoCode.match(h1.regexp);
        if (h1Match) {
          result.contentCheckResult.push({
            line: index + 1,
            msg: `${h1.message} -> ${h1Match[0]}`,
          });
        }

        emptyLink.forEach((i) => {
          let clearContent = commonUtils.removeCodeLines(line);

          const match = clearContent.match(i.regexp);
          if (match) {
            result.contentCheckResult.push({
              line: index + 1,
              msg: `${i.message} -> ${match[0]} -> ${i.description}`,
            });
          }
        });

        const stepName = lineNoCode.includes('[ACCORDION');

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

        if (imageMatches) {
          imageMatches.forEach((item) => {
            // using new RegExp to reset g flag
            const [, imgName] = item.match(new RegExp(mdnImg.regexp, 'i'));

            const altText = item
            // eslint-disable-next-line no-useless-escape
              .replace(/!?[\[\]]/g, '')
              .trim()
              .replace(`(${imgName})`, '');
            const filePath = path.join(dir, imgName);
            const errors = checkLocalImage(filePath, altText);
            result.contentCheckResult.push(...errors.map(err => ({
              line: index + 1,
              msg: err,
            })));
          });
        }

        if (localFileMatch && !imageMatches && !accordionMatch && !tutorialLinkInvalidMatch) {
          result.contentCheckResult.push({
            line: index + 1,
            msg: localFileLink.message,
          });
        }

        const syntaxCheckResult = syntaxChecker.check(lines, index);
        if (syntaxCheckResult) {
          result.syntaxCheckResult.push(...syntaxCheckResult);
        }
      }

      if (!isCodeInNote) {
        const backTicksCheckResult = syntaxChecker.checkBackticks(lines, index);
        result.syntaxCheckResult.push(...backTicksCheckResult);
      }

      if (isMeta) {
        metaLines.push(line);

        const primaryTagError = tagChecker.checkPrimaryTag(lineNoCode, index);
        const xpTagError = tagChecker.checkExperienceTag(lineNoCode, index);

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

      return makeAsyncChecks({
        lines,
        isMeta,
        isCodeBlock,
        index,
        result,
      });
    }));

    return result;
  },
};
