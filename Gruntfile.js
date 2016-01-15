module.exports = function(grunt) {

  grunt.initConfig({
    clean: ["broken-links.log"],
    mdspell: {
      options: {
        ignoreAcronyms: true,
        ignoreNumbers: true
      },
      all: {
        src: [
          '**/*.md',
          '!node_modules/**/**'
        ]
      }
    },
    deadlink: {
      options: {
        logToFile: true,
        logFilename: 'broken-links.log',
        retryDelay: 5000, // retry after 5secs
        maxAttempts: 3, // try max 3 times
        filter: function(content) { // `function` or `regular expressions` to take a link. default is markdown.
          var expressions = [
            /\[[^\]]*\]\((http[s]?:\/\/[^\) ]+)/g, //[...](<url>)
            /\[[^\]]*\]\s*:\s*(http[s]?:\/\/.*)/g, //[...]: <url>
          ];
          var result = [];
          expressions.forEach(expression => {
            var match = expression.exec(content);
            while (match !== null) {
              result.push(match[1]);
              match = expression.exec(content);
            }
          });
          return result; // Return array of link.
        }
      },
      all: {
        src: ['**/*.md', '!node_modules/**/**'] // glob pattern. files path that include links to checking.
      }
    }
  });

  grunt.loadNpmTasks('grunt-mdspell');
  grunt.loadNpmTasks('grunt-deadlink');
  grunt.loadNpmTasks('grunt-contrib-clean');

  grunt.registerTask('test', ['clean', 'mdspell', 'deadlink']);

};
