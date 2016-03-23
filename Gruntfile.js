module.exports = function(grunt) {

  grunt.initConfig({
    filenames: {
      options: {
        valid: function(filename, wholeName) {
          var fName = filename.replace('.md', '');

          // check first if filename equals folder name
          var folders = wholeName.split('/');

          if (folders.length === 3 && folders[0] === 'tutorials') {
            // there is no subfolder for tutorials allowed
            if (folders[1] !== fName) {
              return false;
            }
          }

          // taken from http://xpo6.com/list-of-english-stop-words/
          var stopwords = ['a', 'about', 'above', 'across', 'after', 'afterwards', 'again', 'against', 'all', 'almost', 'alone', 'along', 'already', 'also', 'although', 'always', 'am', 'among', 'amongst', 'amount', 'an', 'and', 'another', 'any', 'anyhow', 'anyone', 'anything', 'anyway', 'anywhere', 'are', 'around', 'as', 'at', 'back', 'be', 'became', 'because', 'become', 'becomes', 'becoming', 'been', 'before', 'beforehand', 'behind', 'being', 'below', 'beside', 'besides', 'between', 'beyond', 'bill', 'both', 'bottom', 'but', 'by', 'call', 'can', 'cannot', 'cant', 'co', 'con', 'could', 'couldnt', 'cry', 'de', 'describe', 'detail', 'do', 'done', 'down', 'due', 'during', 'each', 'eg', 'eight', 'either', 'eleven', 'else', 'elsewhere', 'empty', 'enough', 'etc', 'even', 'ever', 'every', 'everyone', 'everything', 'everywhere', 'except', 'few', 'fifteen', 'fifty', 'fill', 'find', 'fire', 'first', 'five', 'for', 'former', 'formerly', 'forty', 'found', 'four', 'from', 'front', 'full', 'further', 'get', 'give', 'go', 'had', 'has', 'hasnt', 'have', 'he', 'hence', 'her', 'here', 'hereafter', 'hereby', 'herein', 'hereupon', 'hers', 'herself', 'him', 'himself', 'his', 'how', 'however', 'hundred', 'ie', 'if', 'in', 'inc', 'indeed', 'interest', 'into', 'is', 'it', 'its', 'itself', 'keep', 'last', 'latter', 'latterly', 'least', 'less', 'ltd', 'made', 'many', 'may', 'me', 'meanwhile', 'might', 'mill', 'mine', 'more', 'moreover', 'most', 'mostly', 'move', 'much', 'must', 'my', 'myself', 'name', 'namely', 'neither', 'never', 'nevertheless', 'next', 'nine', 'no', 'nobody', 'none', 'noone', 'nor', 'not', 'nothing', 'now', 'nowhere', 'of', 'off', 'often', 'on', 'once', 'one', 'only', 'onto', 'or', 'other', 'others', 'otherwise', 'our', 'ours', 'ourselves', 'out', 'over', 'own', 'part', 'per', 'perhaps', 'please', 'put', 'rather', 're', 'same', 'sap', 'see', 'seem', 'seemed', 'seeming', 'seems', 'serious', 'several', 'she', 'should', 'show', 'side', 'since', 'sincere', 'six', 'sixty', 'so', 'some', 'somehow', 'someone', 'something', 'sometime', 'sometimes', 'somewhere', 'still', 'such', 'system', 'take', 'ten', 'than', 'that', 'the', 'their', 'them', 'themselves', 'then', 'thence', 'there', 'thereafter', 'thereby', 'therefore', 'therein', 'thereupon', 'these', 'they', 'thickv', 'thin', 'third', 'this', 'those', 'though', 'three', 'through', 'throughout', 'thru', 'thus', 'to', 'together', 'too', 'top', 'toward', 'towards', 'twelve', 'twenty', 'two', 'un', 'under', 'until', 'up', 'upon', 'us', 'very', 'via', 'was', 'we', 'well', 'were', 'what', 'whatever', 'when', 'whence', 'whenever', 'where', 'whereafter', 'whereas', 'whereby', 'wherein', 'whereupon', 'wherever', 'whether', 'which', 'while', 'whither', 'who', 'whoever', 'whole', 'whom', 'whose', 'why', 'will', 'with', 'within', 'without', 'would', 'yet', 'you', 'your', 'yours', 'yourself', 'yourselves', 'the'];

          // no underscores or umlauts or uppercase letters allowed
          if (fName.match(/[^a-z0-9-]/)) {
            return false;
          }

          // no filenames with > 50 chars allowed
          if (fName.length > 50) {
            return false;
          }

          var chunks = fName.split('-');

          // allow only up to 5 unqiue keywords
          if (chunks.length > 6) {
            return false;
          }

          // check for common English stop words
          for (var i = 0; i < chunks.length; i++) {
            if (stopwords.indexOf(chunks[i]) > -1) {
              return false;
            }
          }

          return true;
        },
        error: 'Error: {filename} does not pass the filename conventions'
      },
      src: [
        '**/*.md',
        '!node_modules/**/**'
      ]
    },
    files_check: {
      images: {
        options: {
          patterns: [
            '\\!\\[\\]\\((.*?)\\)', // require alt tag for images
            '\\[.*?\\]\\(.?[^http].*?\\)', // require absolute paths for all references
            '\\s#\\s', // check for H1
            '\!\[.*?\]\((.*?)\?token=(.*?)\)', // check for prohibited tokens in IMG tags
            '[^!]\\[.*?(here|there|file|folder|this|page)\\]\\((.*?)\\)', // avoid useless url names
            '\\>###', // avoid message box typo
            '(.exe)(\\b)', // prohibit suspicious filetypes
            '^[#]+\\b' // spaces after header indicators
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
        src: ['**/*.md', '!node_modules/**/**', '!readme.md'] // glob pattern. files path that include links to checking.
      }
    },
    copy: {
      test: {
        files: [{
          expand: true,
          cwd: 'dependency-mods/grunt-files-check/',
          dest: 'node_modules/grunt-files-check/tasks/',
          src: '*/*',
          flatten: true,
          filter: 'isFile'
        }, {
          expand: true,
          cwd: 'dependency-mods/grunt-deadlink/',
          dest: 'node_modules/grunt-deadlink/tasks/',
          src: '*/*',
          flatten: true,
          filter: 'isFile'
        }, {
          expand: true,
          cwd: 'dependency-mods/grunt-filenames/',
          dest: 'node_modules/grunt-filenames/tasks/',
          src: '*/*',
          flatten: true,
          filter: 'isFile'
        }]
      }
    }
  });

  grunt.loadNpmTasks('grunt-mdspell');
  grunt.loadNpmTasks('grunt-deadlink');
  grunt.loadNpmTasks('grunt-files-check');
  grunt.loadNpmTasks('grunt-filenames');
  grunt.loadNpmTasks('grunt-contrib-copy');

  grunt.registerTask('setuptests', ['copy:test']);
  grunt.registerTask('test', ['filenames', 'files_check', 'mdspell', 'deadlink']);
};
