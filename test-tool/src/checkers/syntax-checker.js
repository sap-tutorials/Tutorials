'use strict';

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

function check(content = '', line) {
  const unmatched = getUnmatched(content);

  if (unmatched.size > 0) {
    const uniqPairs = Array.from(unmatched).map(createPair);

    return {
      line,
      msg: `${uniqPairs.join(', ')} not balanced`,
    };
  }
}

module.exports = {
  check,
};
