module.exports = function (grunt) {
	"use strict";
	grunt.loadNpmTasks("@sap/grunt-sapui5-bestpractice-build");
	grunt.registerTask("default", [
		"clean",
		"lint",
		"build"
	]);
	grunt.loadNpmTasks("@sap/grunt-sapui5-bestpractice-test");
	grunt.registerTask("unit_and_integration_tests", ["test"]);
	grunt.config.merge({
		coverage_threshold: {
			statements: 0,
			branches: 100,
			functions: 0,
			lines: 0
		}
	});
};