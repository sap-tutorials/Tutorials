/******************************************************************************
main javaScript file to call in Node.js command prompt
defines the scope of the tutorials which should get tested
******************************************************************************/
"use strict"

var recursive = require('recursive-readdir');

var inquirer = require("inquirer");

var log = require('color-log');

var checkjs = require('./test_modules/checkAll/check.js');

var inquirerprompt = require('./test_modules/prompt/inquirerprompt.js');

var arraydeclaration = require('./test_modules/prompt/arraydeclaration.js');

var commandLineArgs = require('command-line-args')

var optionDefinitions = [
  { name: 'help', alias: 'h', type: Boolean },
  { name: 'all', alias: 'a', type: Boolean },
  { name: 'tutorials', alias: 't', type: Boolean },
  { name: 'wip', alias: 'w', type: Boolean },
  { name: 'specific', alias: 's', type: Boolean },
  { name: 'input', alias: 'i', type: String },
  { name: 'file', alias: 'f', type: Boolean},
  { name: 'progressbar', alias: 'p', type: Boolean}
]

var options = commandLineArgs(optionDefinitions)

/****    request to differentiate the 2 cases
          1. user writes more parameters after the "test.js"-call
          2. user only calls "test.js"
****/
console.log("reading input...")

var version = process.version;
console.log("used node version " + version);
if(version.substring(0,2) === "v4" || version.substring(0,2) === "v5" || version.substring(0,2) === "v6"){

  if (options.file){
    inquirerprompt.setShowfilename(true);
  }

  if (options.all || options.wip  || options.input || options.specific){
    console.log("There is no work-in-progress folder and no further selection. Please use -t");
  }
  else{

    //if p flag is set it turns off the progressbar
    if (options.progressbar){
      inquirerprompt.turnoffprogressbar(true);
    }

    if (options.help){
      log.info("HELP: Your possibilities for the input: \n      -h or --help for Help \n      -f: to log all filenames, which get tested \n      -t: for testing all files in the tutorials folder \n      -p: to turn off the progressbar ");
    }

    else if (options.all){
      //input flag a
      inquirerprompt.readtutorialnames(function (){
        inquirerprompt.readwipnames(function () {
          inquirerprompt.readallfilenames(function () {
            inquirerprompt.allfiles();
          });
        });
      });
    }

    else if (options.tutorials && options.wip && !options.input){
      //input flag w and t
      inquirerprompt.readtutorialnames(function (){
        inquirerprompt.readwipnames(function () {
          inquirerprompt.alltutorials();
        });
      });
    }

    else if (options.tutorials && !options.wip && !options.input){
      //input flag t
      inquirerprompt.readtutorialnames(function (){
        inquirerprompt.tutorials();
      });
    }

    else if (options.wip && !options.tutorials && !options.input){
    //input flag w
      inquirerprompt.readwipnames(function () {
        inquirerprompt.wip();
      });
    }

    else if (options.specific){
      //input flag s
      inquirerprompt.readtutorialnames(function (){
        inquirerprompt.readwipnames(function () {
          inquirerprompt.specific();
        });
      });
    }

    else if (options.input){
      //only input or wt and input
      if ((options.tutorials && options.wip) || (!options.tutorials && !options.wip)){
        inquirerprompt.readtutorialnames(function (){
          inquirerprompt.readwipnames(function () {
            inquirerprompt.allgrouptutorials(options.input);
          });
        });
      }

      //w and  input
      else if (options.wip){
        inquirerprompt.readwipnames(function () {
          inquirerprompt.wipgrouptutorials(options.input);
        });
      }
      //t and  input
      else if (options.tutorials){
        inquirerprompt.readtutorialnames(function (){
          inquirerprompt.tutorialsgrouptutorials(options.input);
        });
      }
    }

    else {
    //  (no user input parameters (except of f))
      if (!options.tutorials && !options.wip && !options.all && !options.help && !options.specific && !options.input)
        inquirerprompt.readtutorialnames(function (){
          inquirerprompt.tutorials();
        });
        //inquirerprompt.ask(arraydeclaration.choicearray);
      else{
        log.error("Error: no available command identified )\n       prompt --help or -h for more info");
      }
    }
  }
}
else {
  log.error("please use node version 4.x through 6.x")
}
