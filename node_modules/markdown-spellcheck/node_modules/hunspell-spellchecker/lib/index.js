
var Dictionary = require("./dictionary");

var Spellchecker = function(dictionary) {
    this.dict = null;

    if (dictionary) this.use(dictionary);
};

// Use a parsed dictionary
Spellchecker.prototype.use = function(dictionary) {
    this.dict = new Dictionary(dictionary);
};

// Parse a dicitonary
Spellchecker.prototype.parse = function(dictionary) {
    var dict = new Dictionary();
    dict.parse(dictionary);

    this.use(dict);

    return dict.toJSON();
};

/**
 * Checks whether a word or a capitalization variant exists in the current dictionary.
 * The word is trimmed and several variations of capitalizations are checked.
 * If you want to check a word without any changes made to it, call checkExact()
 *
 * @see http://blog.stevenlevithan.com/archives/faster-trim-javascript re:trimming function
 *
 * @param {String} aWord The word to check.
 * @returns {Boolean}
 */

Spellchecker.prototype.check = function (aWord) {
    // Remove leading and trailing whitespace
    var trimmedWord = aWord.replace(/^\s\s*/, '').replace(/\s\s*$/, '');

    if (this.checkExact(trimmedWord)) {
        return true;
    }

    // The exact word is not in the dictionary.
    if (trimmedWord.toUpperCase() === trimmedWord) {
        // The word was supplied in all uppercase.
        // Check for a capitalized form of the word.
        var capitalizedWord = trimmedWord[0] + trimmedWord.substring(1).toLowerCase();

        if (this.hasFlag(capitalizedWord, "KEEPCASE")) {
            // Capitalization variants are not allowed for this word.
            return false;
        }

        if (this.checkExact(capitalizedWord)) {
            return true;
        }
    }

    var lowercaseWord = trimmedWord.toLowerCase();

    if (lowercaseWord !== trimmedWord) {
        if (this.hasFlag(lowercaseWord, "KEEPCASE")) {
            // Capitalization variants are not allowed for this word.
            return false;
        }

        // Check for a lowercase form
        if (this.checkExact(lowercaseWord)) {
            return true;
        }
    }

    return false;
};

/**
 * Checks whether a word exists in the current dictionary.
 *
 * @param {String} word The word to check.
 * @returns {Boolean}
 */

Spellchecker.prototype.checkExact = function (word) {
    var ruleCodes = this.dict.dictionaryTable[word];

    if (typeof ruleCodes === 'undefined') {
        // Check if this might be a compound word.
        if ("COMPOUNDMIN" in this.dict.flags && word.length >= this.dict.flags.COMPOUNDMIN) {
            for (var i = 0, _len = this.dict.compoundRules.length; i < _len; i++) {
                if (word.match(this.dict.compoundRules[i])) {
                    return true;
                }
            }
        }

        return false;
    }
    else {
        for (var i = 0, _len = ruleCodes.length; i < _len; i++) {
            if (!this.hasFlag(word, "ONLYINCOMPOUND", ruleCodes[i])) {
                return true;
            }
        }

        return false;
    }
};

/**
 * Looks up whether a given word is flagged with a given flag.
 *
 * @param {String} word The word in question.
 * @param {String} flag The flag in question.
 * @return {Boolean}
 */

Spellchecker.prototype.hasFlag = function (word, flag, wordFlags) {
    if (flag in this.dict.flags) {
        if (typeof wordFlags === 'undefined') {
            var wordFlags = Array.prototype.concat.apply([], this.dict.dictionaryTable[word]);
        }

        if (wordFlags && wordFlags.indexOf(this.dict.flags[flag]) !== -1) {
            return true;
        }
    }

    return false;
};

/**
 * Returns a list of suggestions for a misspelled word.
 *
 * @see http://www.norvig.com/spell-correct.html for the basis of this suggestor.
 * This suggestor is primitive, but it works.
 *
 * @param {String} word The misspelling.
 * @param {Number} [limit=5] The maximum number of suggestions to return.
 * @returns {String[]} The array of suggestions.
 */

Spellchecker.prototype.suggest = function (word, limit) {
    if (!limit) limit = 5;

    if (this.check(word)) return [];

    // Check the replacement table.
    for (var i = 0, _len = this.dict.replacementTable.length; i < _len; i++) {
        var replacementEntry = this.dict.replacementTable[i];

        if (word.indexOf(replacementEntry[0]) !== -1) {
            var correctedWord = word.replace(replacementEntry[0], replacementEntry[1]);

            if (this.check(correctedWord)) {
                return [ correctedWord ];
            }
        }
    }

    var self = this;
    self.dict.alphabet = "abcdefghijklmnopqrstuvwxyz";

    /*
    if (!self.alphabet) {
        // Use the alphabet as implicitly defined by the words in the dictionary.
        var alphaHash = {};

        for (var i in self.dictionaryTable) {
            for (var j = 0, _len = i.length; j < _len; j++) {
                alphaHash[i[j]] = true;
            }
        }

        for (var i in alphaHash) {
            self.alphabet += i;
        }

        var alphaArray = self.alphabet.split("");
        alphaArray.sort();
        self.alphabet = alphaArray.join("");
    }
    */

    function edits1(words) {
        var rv = [];

        for (var ii = 0, _iilen = words.length; ii < _iilen; ii++) {
            var word = words[ii];

            var splits = [];

            for (var i = 0, _len = word.length + 1; i < _len; i++) {
                splits.push([ word.substring(0, i), word.substring(i, word.length) ]);
            }

            var deletes = [];

            for (var i = 0, _len = splits.length; i < _len; i++) {
                var s = splits[i];

                if (s[1]) {
                    deletes.push(s[0] + s[1].substring(1));
                }
            }

            var transposes = [];

            for (var i = 0, _len = splits.length; i < _len; i++) {
                var s = splits[i];

                if (s[1].length > 1) {
                    transposes.push(s[0] + s[1][1] + s[1][0] + s[1].substring(2));
                }
            }

            var replaces = [];

            for (var i = 0, _len = splits.length; i < _len; i++) {
                var s = splits[i];

                if (s[1]) {
                    for (var j = 0, _jlen = self.dict.alphabet.length; j < _jlen; j++) {
                        replaces.push(s[0] + self.dict.alphabet[j] + s[1].substring(1));
                    }
                }
            }

            var inserts = [];

            for (var i = 0, _len = splits.length; i < _len; i++) {
                var s = splits[i];

                if (s[1]) {
                    for (var j = 0, _jlen = self.dict.alphabet.length; j < _jlen; j++) {
                        replaces.push(s[0] + self.dict.alphabet[j] + s[1]);
                    }
                }
            }

            rv = rv.concat(deletes);
            rv = rv.concat(transposes);
            rv = rv.concat(replaces);
            rv = rv.concat(inserts);
        }

        return rv;
    }

    function known(words) {
        var rv = [];

        for (var i = 0; i < words.length; i++) {
            if (self.check(words[i])) {
                rv.push(words[i]);
            }
        }

        return rv;
    }

    function correct(word) {
        // Get the edit-distance-1 and edit-distance-2 forms of this word.
        var ed1 = edits1([word]);
        var ed2 = edits1(ed1);

        var corrections = known(ed1).concat(known(ed2));

        // Sort the edits based on how many different ways they were created.
        var weighted_corrections = {};

        for (var i = 0, _len = corrections.length; i < _len; i++) {
            if (!(corrections[i] in weighted_corrections)) {
                weighted_corrections[corrections[i]] = 1;
            }
            else {
                weighted_corrections[corrections[i]] += 1;
            }
        }

        var sorted_corrections = [];

        for (var i in weighted_corrections) {
            sorted_corrections.push([ i, weighted_corrections[i] ]);
        }

        function sorter(a, b) {
            if (a[1] < b[1]) {
                return -1;
            }

            return 1;
        }

        sorted_corrections.sort(sorter).reverse();

        var rv = [];

        for (var i = 0, _len = Math.min(limit, sorted_corrections.length); i < _len; i++) {
            if (!self.hasFlag(sorted_corrections[i][0], "NOSUGGEST")) {
                rv.push(sorted_corrections[i][0]);
            }
        }

        return rv;
    }

    return correct(word);
};

module.exports = Spellchecker;
