'use strict';

const { regexp } = require('../constants');

const {
    validation: {
        accordions,
    },
    options: {
        regexps,
        messages,
    },
} = regexp;

function lineOf({ content, searchText, searchAfterLine = 0, lineStart: line = 0 }) {
    let matchedChars = 0;

    for (let i = 0; i < content.length; i += 1) {
        if (content[i] === searchText[matchedChars]) {
            matchedChars += 1;
        } else {
            matchedChars = 0;
        }

        if (matchedChars === searchText.length) {
            if (line > searchAfterLine) {
                return line + 1;
            } else {
                // start over
                // since we can have the same content in the different accordions
                matchedChars = 0;
            }
        }
        if (content[i] === '\n') {
            line += 1;
        }
    }

    return -1;
}

function makeMatchSafe(matchResult) {
    return matchResult || [];
}

function buildErrorObject({ errorData, content, accordionLine }) {
    const { errorMessage, searchText, lineStart = 0 } = errorData;

    return {
        msg: errorMessage,
        line: lineOf({
            content,
            searchText,
            searchAfterLine: accordionLine,
            lineStart,
        }),
    };
}

const checkers = {
    contentBetween(data) {
        const { accordion } = data;
        const contentBetweenLines = makeMatchSafe(accordion.match(regexps.contentBetween))
          .reduce((result, matchStr) => [...result, ...matchStr.split('\n')], []);

        const contentBetween = contentBetweenLines
          .map((line) => {
              // ignore option syntax components
              if (line.startsWith('[OPTION BEGIN')) {
                  line = '';
              }

              return line
                .replace('[OPTION END]', '')
                .trim();
          })
          .filter(result => !!result);

        if (contentBetween.length > 0) {
            return contentBetween.map(content => ({
                errorMessage: messages.contentBetween,
                searchText: content,
            }));
        }
    },
    mess(data) {
        const { optionStarts, optionEnds } = data;

        if (optionEnds.length !== optionStarts.length) {
            return {
                errorMessage: messages.mess,
                searchText: optionStarts[0] || optionEnds[0],
                // regexp for option-end will not return option-end in the substring
                lineStart: (!optionStarts[0] && optionEnds[0]) ? 1 : 0,
            };
        }
    },
    redundantUsage(data) {
        const { options, optionStarts } = data;
        if (options.length === 1) {
            return {
                errorMessage: messages.oneOption,
                searchText: optionStarts[0],
            };
        }
    },
    duplicates(data) {
        const { options } = data;
        const titles = [];

        options.forEach((optionMatch) => {
            titles.push(...makeMatchSafe(optionMatch.match(regexps.title))
              .map(title => title.replace('[OPTION BEGIN [', '')));
        });

        const uniqueTitles = Array.from(new Set(titles));
        const counts = {};

        titles.forEach((title) => {
            counts[title] = (counts[title] || 0) + 1;
            return counts;
        });

        if (titles.length !== uniqueTitles.length) {
            const errors = [];

            Object
              .entries(counts)
              .forEach(([title, count]) => {
                  if (count > 1) {
                      errors.push({
                          errorMessage: messages.duplicate,
                          searchText: title,
                      });
                  }
              });
            return errors;
        }
    },
};

module.exports = {
    check: (filePath, content) => {
        const errors = [];
        const accordionMatch = makeMatchSafe(content.match(accordions));

        accordionMatch.forEach((str) => {
            const options = makeMatchSafe(str.match(regexps.full));
            const optionEnds = makeMatchSafe(str.match(regexps.end))
              .filter(result => !!result);
            const optionStarts = makeMatchSafe(str.match(regexps.start))
              .filter(result => !!result);

            const accordionLine = lineOf({
                content,
                searchText: str.split('\n')[0],
            });

            Object
              .values(checkers)
              .forEach((handler) => {
                  const result = handler({
                      accordion: str,
                      options,
                      optionStarts,
                      optionEnds,
                  });

                  if (!result) {
                      return;
                  }


                  if (Array.isArray(result)) {
                      result.forEach((error) => {
                          const errorObject = buildErrorObject({
                              accordionLine,
                              content,
                              errorData: error,
                          });

                          errors.push(errorObject);
                      });
                  } else {
                      const errorObject = buildErrorObject({
                          accordionLine,
                          content,
                          errorData: result,
                      });

                      errors.push(errorObject);
                  }
              });
        });

        return errors;
    },
};
