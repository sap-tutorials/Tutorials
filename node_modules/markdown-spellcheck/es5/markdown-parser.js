"use strict";

exports.__esModule = true;

exports.default = function (src) {
  var textTokens = [];
  var currentIndex = 0;

  var tracker = (0, _trackingReplacement2.default)(src);

  // remove things we won't process so we can use simple next matching word logic
  // to calculate the index
  tracker.replaceAll(/\---\r?\n[\w\W]*?\r?\n---/, " "); // remove header
  tracker.removeAll(/```[\w\W]*?```/);
  tracker.removeAll(/~~~[\w\W]*?~~~/);
  tracker.removeAll(/``[\w\W]*?``/);
  tracker.removeAll(/`[^`]*`/);
  tracker.replaceAll(/<style[\w\W]*?<\/style>/, " "); // remove contents of style
  tracker.replaceAll(/<script[\w\W]*?<\/script>/, " "); // remove contents of scripts
  tracker.replaceAll(/\{%\s*highlight[\w\W]*?\{%\s*endhighlight\s*%\}/, " "); // remove contents code blocks
  tracker.replaceAll(/\{%.*%\}/, " ");
  tracker.replaceAll(/\{\{.*\}\}/, " ");
  tracker.replaceAll(/&[#a-z0-9]{1,5};/, " ");
  src = tracker.replaceAll(/<\/?[a-z0-9]+ ?([a-z]+="[^"]*" ?)*\/?>/i, " ");

  var options = {
    gfm: true,
    renderer: {
      strong: function strong() {},
      em: function em() {},
      codespan: function codespan() {},
      br: function br() {},
      del: function del() {},
      link: function link() {},
      image: function image() {},
      text: function text(_text) {
        _text = _text.replace(/&#39;/g, "'");
        var roughSplit = _text.split(/(https?|ftp):\/\/[^\s/$.?#].[^\s]*|[\s\xa0\r\n]|&[a-z#0-9]+;|[&<>]/);
        for (var i = 0; i < roughSplit.length; i++) {
          var split = roughSplit[i];
          if (split) {
            addToken(split);
          }
        }
      }
    }
  };

  function addToken(text) {
    var newIndex = src.indexOf(text, currentIndex);
    if (newIndex === -1) {
      throw new Error("Markdown Parser : Inline Lexer : Could not find index of text - \n" + text + "\n\n**In**\n\n" + src.substring(currentIndex, 30) + "\n");
    }
    currentIndex = newIndex + text.length;
    textTokens.push({ text: text, index: tracker.getOriginalIndex(newIndex) });
  }

  var tokens = _marked2.default.lexer(src, options);
  var inlineLexer = new _marked2.default.InlineLexer(tokens.links, options);

  for (var i = 0; i < tokens.length; i++) {
    var token = tokens[i];
    if (token.text && token.type !== "code") {
      inlineLexer.output(token.text);
    }
  }

  return textTokens;
};

var _marked = require("marked");

var _marked2 = _interopRequireDefault(_marked);

var _trackingReplacement = require("./tracking-replacement");

var _trackingReplacement2 = _interopRequireDefault(_trackingReplacement);

function _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }