module.exports = function(grunt) {

  grunt.initConfig({
    copy: {
      depchanges: {
        src: 'dependency-mods/files_check.js',
        dest: 'node_modules/grunt-files-check/tasks/files_check.js'
      }
    },
    files_check: {
      images: {
        options: {
          patterns: [
            '\\!\\[\\]\\((.*?)\\)', // require alt tag for images
            '\\>###', // avoid message box typo
            '(.exe)(\\b)' // prohibit suspicious filetypes
          ],
          verbose: true,
          maxFileNameWidth: 100
        },
        src: [
          '**/*.md',
          '!node_modules/**/**'
        ]
      }
    },
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
        retryDelay: 100000,
        maxAttempts: 5,
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
  grunt.loadNpmTasks('grunt-files-check');
  grunt.loadNpmTasks('grunt-contrib-copy');

  grunt.registerTask('test', ['copy', 'files_check', 'mdspell', 'deadlink']);

};
