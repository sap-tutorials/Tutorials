# Hunspell Spellchecker in Javascript

[![Build Status](https://travis-ci.org/GitbookIO/hunspell-spellchecker.png?branch=master)](https://travis-ci.org/GitbookIO/hunspell-spellchecker)
[![NPM version](https://badge.fury.io/js/hunspell-spellchecker.svg)](http://badge.fury.io/js/hunspell-spellchecker)

A lightweight spellchecker written in Javascript, it can be used in Node.JS and in the browser. It has been build to be pre-parse Hunspell dictionary to JSON.

### Installation

```
$ npm install hunspell-spellchecker
```

### API

Initialize a spellchecker instance:

```js
var Spellchecker = require("hunspell-spellchecker");

var spellchecker = new Spellchecker();
```

Parse and serialize a dictionary

```js
// Parse an hunspell dictionary that can be serialized as JSON
var DICT = spellchecker.parse({
    aff: fs.readFileSync("./en_EN.aff");
    dic: fs.readFileSync("./en_EN.dic")
});
```

Load a serialized dictionary

```js
// Load a dictionary
spellchecker.use(DICT);
```

Check a word:

```js
// Check a word
var isRight = spellchecker.check("tll");
```
