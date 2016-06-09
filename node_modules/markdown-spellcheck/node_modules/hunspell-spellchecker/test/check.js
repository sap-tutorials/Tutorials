var fs = require("fs");
var path = require("path");
var assert = require("assert");

var Spellcheck = require("../lib");

describe("Dictionary", function() {
    var sp = new Spellcheck();

    sp.parse({
        dic: fs.readFileSync(path.resolve(__dirname, "fixtures/test.dic")),
        aff: fs.readFileSync(path.resolve(__dirname, "fixtures/test.aff"))
    });


    it("should correctly signal a correct word", function() {
        assert(sp.check("hello"));
    });

    it("should correctly signal a correct word (UPPERCASE)", function() {
        assert(sp.check("HELLO"));
    });

    it("should correctly signal an invalid word", function() {
        assert(!sp.check("helo"));
    });
});
