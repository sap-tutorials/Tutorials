var fs = require("fs");
var path = require("path");
var assert = require("assert");

var Dictionary = require("../lib/dictionary");

describe("Dictionary", function() {
    it("should parse correctly an hunspell dictionary", function() {
        var sp = new Dictionary();

        sp.parse({
            dic: fs.readFileSync(path.resolve(__dirname, "fixtures/test.dic")),
            aff: fs.readFileSync(path.resolve(__dirname, "fixtures/test.aff"))
        });

        var DICT = sp.toJSON();
        assert(DICT.rules);
        assert(DICT.dictionaryTable);
    });
});
