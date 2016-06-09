
var Dictionary = function(dict) {
    this.rules = {};
    this.dictionaryTable = {};

    this.compoundRules = [];
    this.compoundRuleCodes = {};

    this.replacementTable = [];

    this.flags = {};

    if (dict) this.load(dict);
};

// Load from object
Dictionary.prototype.load = function (obj) {
    for (var i in obj) {
        this[i] = obj[i];
    }
};

// Return as JSON
Dictionary.prototype.toJSON = function(dictionary) {
    return {
        rules: this.rules,
        dictionaryTable: this.dictionaryTable,
        compoundRules: this.compoundRules,
        compoundRuleCodes: this.compoundRuleCodes,
        replacementTable: this.replacementTable,
        flags: this.flags
    };
};

// Parse a dictionary
Dictionary.prototype.parse = function(dictionary) {
    if (!dictionary.aff && !dictionary.dic) {
        throw "Invalid dictionary to parse";
    }


    this.rules = this._parseAFF(""+dictionary.aff);

    // Save the rule codes that are used in compound rules.
    this.compoundRuleCodes = {};

    for (var i = 0, _len = this.compoundRules.length; i < _len; i++) {
        var rule = this.compoundRules[i];

        for (var j = 0, _jlen = rule.length; j < _jlen; j++) {
            this.compoundRuleCodes[rule[j]] = [];
        }
    }

    // If we add this ONLYINCOMPOUND flag to this.compoundRuleCodes, then _parseDIC
    // will do the work of saving the list of words that are compound-only.
    if ("ONLYINCOMPOUND" in this.flags) {
        this.compoundRuleCodes[this.flags.ONLYINCOMPOUND] = [];
    }

    this.dictionaryTable = this._parseDIC(""+dictionary.dic);

    // Get rid of any codes from the compound rule codes that are never used
    // (or that were special regex characters).  Not especially necessary...
    for (var i in this.compoundRuleCodes) {
        if (this.compoundRuleCodes[i].length == 0) {
            delete this.compoundRuleCodes[i];
        }
    }

    // Build the full regular expressions for each compound rule.
    // I have a feeling (but no confirmation yet) that this method of
    // testing for compound words is probably slow.
    for (var i = 0, _len = this.compoundRules.length; i < _len; i++) {
        var ruleText = this.compoundRules[i];

        var expressionText = "";

        for (var j = 0, _jlen = ruleText.length; j < _jlen; j++) {
            var character = ruleText[j];

            if (character in this.compoundRuleCodes) {
                expressionText += "(" + this.compoundRuleCodes[character].join("|") + ")";
            }
            else {
                expressionText += character;
            }
        }

        this.compoundRules[i] = new RegExp(expressionText, "i");
    }
};

/**
 * Parse the rules out from a .aff file.
 *
 * @param {String} data The contents of the affix file.
 * @returns object The rules from the file.
 */
Dictionary.prototype._parseAFF = function (data) {
    var rules = {};

    // Remove comment lines
    data = this._removeAffixComments(data);

    var lines = data.split("\n");

    for (var i = 0, _len = lines.length; i < _len; i++) {
        var line = lines[i];

        var definitionParts = line.split(/\s+/);

        var ruleType = definitionParts[0];

        if (ruleType == "PFX" || ruleType == "SFX") {
            var ruleCode = definitionParts[1];
            var combineable = definitionParts[2];
            var numEntries = parseInt(definitionParts[3], 10);

            var entries = [];

            for (var j = i + 1, _jlen = i + 1 + numEntries; j < _jlen; j++) {
                var line = lines[j];

                var lineParts = line.split(/\s+/);
                var charactersToRemove = lineParts[2];

                var additionParts = lineParts[3].split("/");

                var charactersToAdd = additionParts[0];
                if (charactersToAdd === "0") charactersToAdd = "";

                var continuationClasses = this.parseRuleCodes(additionParts[1]);

                var regexToMatch = lineParts[4];

                var entry = {};
                entry.add = charactersToAdd;

                if (continuationClasses.length > 0) entry.continuationClasses = continuationClasses;

                if (regexToMatch !== ".") {
                    if (ruleType === "SFX") {
                        entry.match = new RegExp(regexToMatch + "$");
                    }
                    else {
                        entry.match = new RegExp("^" + regexToMatch);
                    }
                }

                if (charactersToRemove != "0") {
                    if (ruleType === "SFX") {
                        entry.remove = new RegExp(charactersToRemove  + "$");
                    }
                    else {
                        entry.remove = charactersToRemove;
                    }
                }

                entries.push(entry);
            }

            rules[ruleCode] = { "type" : ruleType, "combineable" : (combineable == "Y"), "entries" : entries };

            i += numEntries;
        }
        else if (ruleType === "COMPOUNDRULE") {
            var numEntries = parseInt(definitionParts[1], 10);

            for (var j = i + 1, _jlen = i + 1 + numEntries; j < _jlen; j++) {
                var line = lines[j];

                var lineParts = line.split(/\s+/);
                this.compoundRules.push(lineParts[1]);
            }

            i += numEntries;
        }
        else if (ruleType === "REP") {
            var lineParts = line.split(/\s+/);

            if (lineParts.length === 3) {
                this.replacementTable.push([ lineParts[1], lineParts[2] ]);
            }
        }
        else {
            // ONLYINCOMPOUND
            // COMPOUNDMIN
            // FLAG
            // KEEPCASE
            // NEEDAFFIX

            this.flags[ruleType] = definitionParts[1];
        }
    }

    return rules;
};

/**
 * Removes comment lines and then cleans up blank lines and trailing whitespace.
 *
 * @param {String} data The data from an affix file.
 * @return {String} The cleaned-up data.
 */
Dictionary.prototype._removeAffixComments = function (data) {
    // Remove comments
    data = data.replace(/#.*$/mg, "");

    // Trim each line
    data = data.replace(/^\s\s*/m, '').replace(/\s\s*$/m, '');

    // Remove blank lines.
    data = data.replace(/\n{2,}/g, "\n");

    // Trim the entire string
    data = data.replace(/^\s\s*/, '').replace(/\s\s*$/, '');

    return data;
};

/**
 * Parses the words out from the .dic file.
 *
 * @param {String} data The data from the dictionary file.
 * @returns object The lookup table containing all of the words and
 *                 word forms from the dictionary.
 */
Dictionary.prototype._parseDIC = function (data) {
    data = this._removeDicComments(data);

    var lines = data.split("\n");
    var dictionaryTable = {};

    function addWord(word, rules) {
        // Some dictionaries will list the same word multiple times with different rule sets.
        if (!(word in dictionaryTable) || typeof dictionaryTable[word] != 'object') {
            dictionaryTable[word] = [];
        }

        dictionaryTable[word].push(rules);
    }

    // The first line is the number of words in the dictionary.
    for (var i = 1, _len = lines.length; i < _len; i++) {
        var line = lines[i];

        var parts = line.split("/", 2);

        var word = parts[0];

        // Now for each affix rule, generate that form of the word.
        if (parts.length > 1) {
            var ruleCodesArray = this.parseRuleCodes(parts[1]);

            // Save the ruleCodes for compound word situations.
            if (!("NEEDAFFIX" in this.flags) || ruleCodesArray.indexOf(this.flags.NEEDAFFIX) == -1) {
                addWord(word, ruleCodesArray);
            }

            for (var j = 0, _jlen = ruleCodesArray.length; j < _jlen; j++) {
                var code = ruleCodesArray[j];

                var rule = this.rules[code];

                if (rule) {
                    var newWords = this._applyRule(word, rule);

                    for (var ii = 0, _iilen = newWords.length; ii < _iilen; ii++) {
                        var newWord = newWords[ii];

                        addWord(newWord, []);

                        if (rule.combineable) {
                            for (var k = j + 1; k < _jlen; k++) {
                                var combineCode = ruleCodesArray[k];

                                var combineRule = this.rules[combineCode];

                                if (combineRule) {
                                    if (combineRule.combineable && (rule.type != combineRule.type)) {
                                        var otherNewWords = this._applyRule(newWord, combineRule);

                                        for (var iii = 0, _iiilen = otherNewWords.length; iii < _iiilen; iii++) {
                                            var otherNewWord = otherNewWords[iii];
                                            addWord(otherNewWord, []);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }

                if (code in this.compoundRuleCodes) {
                    this.compoundRuleCodes[code].push(word);
                }
            }
        }
        else {
            addWord(word.trim(), []);
        }
    }

    return dictionaryTable;
};


/**
 * Removes comment lines and then cleans up blank lines and trailing whitespace.
 *
 * @param {String} data The data from a .dic file.
 * @return {String} The cleaned-up data.
 */
Dictionary.prototype._removeDicComments = function (data) {
    // I can't find any official documentation on it, but at least the de_DE
    // dictionary uses tab-indented lines as comments.

    // Remove comments
    data = data.replace(/^\t.*$/mg, "");

    return data;

    // Trim each line
    data = data.replace(/^\s\s*/m, '').replace(/\s\s*$/m, '');

    // Remove blank lines.
    data = data.replace(/\n{2,}/g, "\n");

    // Trim the entire string
    data = data.replace(/^\s\s*/, '').replace(/\s\s*$/, '');

    return data;
};

Dictionary.prototype.parseRuleCodes = function (textCodes) {
    if (!textCodes) {
        return [];
    }
    else if (!("FLAG" in this.flags)) {
        return textCodes.split("");
    }
    else if (this.flags.FLAG === "long") {
        var flags = [];

        for (var i = 0, _len = textCodes.length; i < _len; i += 2) {
            flags.push(textCodes.substr(i, 2));
        }

        return flags;
    }
    else if (this.flags.FLAG === "num") {
        return textCodes.split(",");
    }
};

/**
 * Applies an affix rule to a word.
 *
 * @param {String} word The base word.
 * @param {Object} rule The affix rule.
 * @returns {String[]} The new words generated by the rule.
 */

Dictionary.prototype._applyRule = function (word, rule) {
    var entries = rule.entries;
    var newWords = [];

    for (var i = 0, _len = entries.length; i < _len; i++) {
        var entry = entries[i];

        if (!entry.match || word.match(entry.match)) {
            var newWord = word;

            if (entry.remove) {
                newWord = newWord.replace(entry.remove, "");
            }

            if (rule.type === "SFX") {
                newWord = newWord + entry.add;
            }
            else {
                newWord = entry.add + newWord;
            }

            newWords.push(newWord);

            if ("continuationClasses" in entry) {
                for (var j = 0, _jlen = entry.continuationClasses.length; j < _jlen; j++) {
                    var continuationRule = this.rules[entry.continuationClasses[j]];

                    if (continuationRule) {
                        newWords = newWords.concat(this._applyRule(newWord, continuationRule));
                    }
                    /*
                    else {
                        // This shouldn't happen, but it does, at least in the de_DE dictionary.
                        // I think the author mistakenly supplied lower-case rule codes instead
                        // of upper-case.
                    }
                    */
                }
            }
        }
    }

    return newWords;
};


module.exports = Dictionary;
