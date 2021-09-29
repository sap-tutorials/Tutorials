const constants = require('../constants');
const common = require('../utils/common');

const { regexp: { validation: { done, validate, inlineCodeBlock } } } = constants;

const closingPairs = {
  '}': '{',
  ']': '[',
  ')': '(',
};

const openingPairs = {
  '{': '}',
  '[': ']',
  '(': ')',
};

const isClosing = {
  '}': true,
  ']': true,
  ')': true,
};

function createPair(item) {
  const pairForClosing = closingPairs[item];
  const pairForOpening = openingPairs[item];

  if (pairForClosing) {
    return `${pairForClosing}${item}`;
  } else if (pairForOpening) {
    return `${item}${pairForOpening}`;
  }

  return '';
}

function getUnmatched(content) {
  const stack = [];
  const unmatched = new Set();

  for (let i = 0; i < content.length; i += 1) {
    const char = content[i];

    if (openingPairs[char]) {
      stack.push(char);
    } else if (isClosing[char]) {
      if (openingPairs[stack.pop()] !== char) {
        unmatched.add(char);
      }
    }
  }

  if (stack.length > 0) {
    stack.forEach(unmatched.add.bind(unmatched));
  }

  return unmatched;
}

function checkBalanced(content = '', lineNumber) {
  const unmatched = getUnmatched(content);

  if (unmatched.size > 0) {
    const uniqPairs = Array.from(unmatched)
      .map(createPair);

    return [{
      line: lineNumber + 1,
      msg: `${uniqPairs.join(', ')} not balanced`,
    }];
  }

  return [];
}

function checkDoneValidate(lines, lineNumber) {
  const line = lines[lineNumber];
  const doneMatch = line.match(done);
  const validateMatch = line.match(validate);
  const result = [];

  if (doneMatch && line.startsWith(' ')) {
    result.push({
      line: lineNumber + 1,
      msg: 'Do not indent [DONE] button',
    });
  }
  const prevLine = lines[lineNumber - 1];

  if (prevLine && ((doneMatch || validateMatch) && prevLine.trim() !== '')) {
    const template = doneMatch ? '[DONE] button' : '[VALIDATE] element';

    result.push({
      line: lineNumber + 1,
      msg: `Blank line needed before ${template}`,
    });
  }
  return result;
}

function checkBackticks(lines, lineNumber) {
  const line = lines[lineNumber];
  const result = [];
  const trimmedLine = line.trim();
  const backticks = '```';
  const isInlineCodeBlock = line.match(inlineCodeBlock);

  if (isInlineCodeBlock) {
    return result;
  }

  if (trimmedLine.includes(backticks) && !trimmedLine.startsWith(backticks)) {
    result.push({
      line: lineNumber + 1,
      msg: 'Code block tag (```) must be on its own line',
    });
  }

  return result;
}

function check(lines, lineNumber) {
  const line = lines[lineNumber];
  const pureLine = common.removeCodeLines(line);
  const balancedCheckResult = checkBalanced(pureLine, lineNumber);
  const doneValidateCheckResult = checkDoneValidate(lines, lineNumber);

  return []
    .concat(balancedCheckResult)
    .concat(doneValidateCheckResult);
}

module.exports = {
  check,
  checkBackticks,
};
