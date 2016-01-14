module.exports = function(grunt) {

  grunt.initConfig({
    mdspell: {
      options: {
        ignoreAcronyms: true,
        ignoreNumbers: true
      },
      files: {
        src: [
          '**/*.md',
          '!node_modules/**/**'
        ]
      },
    },
  });

  grunt.loadNpmTasks('grunt-mdspell');

  grunt.registerTask('default', ['mdspell']);

};
