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

const commandLineArgs = require('command-line-args')

const optionDefinitions = [
  { name: 'help', alias: 'h', type: Boolean },
  { name: 'all', alias: 'a', type: Boolean },
  { name: 'tutorials', alias: 't', type: Boolean },
  { name: 'wip', alias: 'w', type: Boolean },
  { name: 'specific', alias: 's', type: Boolean },
  { name: 'input', alias: 'i', type: String },
  { name: 'file', alias: 'f', type: Boolean},
  { name: 'progressbar', alias: 'p', type: Boolean}
]

const options = commandLineArgs(optionDefinitions)

/****    request to differentiate the 2 cases
          1. user writes more parameters after the "test.js"-call
          2. user only calls "test.js"
****/
console.log("reading input...");

if (options.file){
  inquirerprompt.setShowfilename(true);
}

//if p flag is set it turns off the progressbar
if (options.progressbar){
  inquirerprompt.turnoffprogressbar(true);
}

if (options.help){
  log.info("HELP: Your possibilities for the input: \n      -h or --help for Help \n      -f: to log all filenames, which get tested \n      -a: for testing everything (including files, which aren't tutorials) \n      -w & -t: for testing every tutorial in both folders \n      -w: for testing all files in the work-in-progress folder \n      -t: for testing all files in the tutorials folder \n      -s: for dialog to choose specific tutorials \n      -i: to give an filename behind with *\n          to resctrict the scope: enter -w or/and -t \n      -p: to turn off the progressbar ");
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
    inquirerprompt.ask(arraydeclaration.choicearray);
  else{
    log.error("Error: no available command identified )\n       prompt --help or -h for more info");
  }
}
