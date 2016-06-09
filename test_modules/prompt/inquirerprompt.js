/******************************************************************************
contains the main function "ask" --> to prompt the interactive commands (differentiates different cases)
contains "help functions" to extend the interacive commands and to save the results (files which should get tested)
******************************************************************************/
"use strict"

var inquirer = require("inquirer");

var arraydeclaration = require('./arraydeclaration.js');

var recursive = require('recursive-readdir');

var checkjs = require('../checkAll/check.js');

var log = require('color-log');

var path = require('path');


var showfilenames = false;

var setShowfilename = function(value){
  showfilenames = value;
}

var showprogressbar = true;

var turnoffprogressbar = function(value){
  if (value){
    showprogressbar = false;
  }
}

//"main function" -> reacts on the user input from commands
var ask = function (array){
    inquirer.prompt(array).then(function(results) {

//differentiates the cases, if the user wants to select (after the first input decision for "input") tutorials from the tutorials folder or the work-in-progress folder
      switch(results.inputscopetutorial) {
        case "from wip":
            if (results.inputselection.length > 0 && results.inputselection.endsWith("*")){
              wipgrouptutorials(results.inputselection);
            }
            else{
              ask(arraydeclaration.choiceinputarray2);
            }
            break;
        case "from tutorials":
        //  //console.log("hallo tut");
          if (results.inputselection.length > 0 && results.inputselection.endsWith("*")){
            tutorialsgrouptutorials(results.inputselection);
          }
          else{
            ask(arraydeclaration.choiceinputarray2);
          }
            break;
        default: ;
          //  //console.log("default");
      }

  //switch statement to differentiate the selection of which tutorials the user wants to test (all, from specific folder wip oder tutorials, specific ones via extra selection or with an input)
  //->call functions for required further dialogs or/and to build up the array with the results of tutorials (to test)
      switch(results.scopetutorial) {
        case "all":
            readtutorialnames(function (){
              ////console.log("all selected");
              readwipnames(function () {
                readallfilenames(function () {
                  allfiles();
                });
              });
            });
            break;
        case "wip":
            readwipnames(function () {
              wip();
            });
            break;
        case "tutorials":
            readtutorialnames(function (){
              tutorials();
            });
            break;
        case "specific":
            readtutorialnames(function (){
              readwipnames(function () {
                specific();
              });
            });
            break;
        case "input":
            readtutorialnames(function (){
              readwipnames(function () {
                input();
              });
            });
            break;
        case "from wip":
              specificwip();
            break;
        case "from tutorials":
              specifictutorials();
            break;
        default: ;
          //  //console.log("default");
    }

///in case the user did a specific selection of tutorials (not the whole folder - user picked specific ones)
    if (results.specifictutorials.length > 0) {
      singletutorialsselected(results.specifictutorials);
    }
});
}



/**************** help functions*****************/


//function to read oll filenames from the tutorials folder into the object arraytutorial
var readtutorialnames = function (callback){
  var fname;
  var i=0;
  recursive('./tutorials', ['!*.md'],function (err, files) {
    files.forEach(function(file){
      arraydeclaration.arraytutorials[0].choices[i]= {name: file };
      i++;
    });
    callback();
  });
}



//function to read oll filenames from the wip folder into the object arraywip
var readwipnames = function (callback){
  var wfname;
  var j=0;
  recursive('./work-in-progress', ['!*.md', 'stopwords.md', 'readme.md'],function (err, files) {
    files.forEach(function(file){
      arraydeclaration.arraywip[0].choices[j]= {name:  file };
      j++;
    });
    callback();
  });
}

var allotherfilenames = [];

var readallfilenames = function (callback){
  var cnt=0;
  recursive('./', ['!*.md', 'tutorials\*', 'node_modules\*', 'work-in-progress\*', ""],function (err, files) {
    files.forEach(function(file){
      allotherfilenames[cnt] = file;
      cnt++;
    //resultarray.push(file);
    //  console.log(allotherfilenames[cnt]);
    });
    callback();
  });
}


var resultarray = [];

//function to build the results array with all files of the tutorial and wip folder in case the user wants to test all tutorials
var alltutorials = function (){
  ////console.log("alltutorials checked");
  for (var k=0; k < arraydeclaration.arraywip[0].choices.length; k++){
    resultarray.push(arraydeclaration.arraywip[0].choices[k].name);
  }
  for (var m = 0; m < arraydeclaration.arraytutorials[0].choices.length; m++){
    resultarray.push(arraydeclaration.arraytutorials[0].choices[m].name);
  }
  if (showfilenames){
    console.log(resultarray);
  }
  //console.log(resultarray.length);
  checkjs(resultarray, showprogressbar);
}

var allfiles = function (){
  //console.log("allfileschecked");
  for (var k=0; k < arraydeclaration.arraywip[0].choices.length; k++){
    resultarray.push(arraydeclaration.arraywip[0].choices[k].name);
  }
  for (var m = 0; m < arraydeclaration.arraytutorials[0].choices.length; m++){
    resultarray.push(arraydeclaration.arraytutorials[0].choices[m].name);
  }
  for (var n = 0; n < allotherfilenames.length; n++){
    resultarray.push(allotherfilenames[n]);
    //console.log(allotherfilenames[n]);
  }

  /*
  recursive('./', ['!*.md', 'tutorials\*', 'node_modules\*', 'work-in-progress\*'],function (err, files) {
    files.forEach(function(file){
      resultarray.push(file);
      console.log(file);
    });
  }); */
  if (showfilenames){
    console.log(resultarray);
  }

  //console.log(resultarray.length);
  checkjs(resultarray, showprogressbar);
}

//function to build the results array with all files of the wip folder in case the user wants to test all wip-tutorials
var wip = function (){
  //console.log("wip checked");
  for (var k=0; k < arraydeclaration.arraywip[0].choices.length; k++){
    resultarray.push(arraydeclaration.arraywip[0].choices[k].name);
  }
  if (showfilenames){
    console.log(resultarray);
  }
  checkjs(resultarray, showprogressbar);
}

//function to build the results array with all files of the tutorial folder in case the user wants to test all tutorials
var tutorials = function (){
  //console.log("tutorials checked");
  for (var k=0; k < arraydeclaration.arraytutorials[0].choices.length; k++){
    resultarray.push(arraydeclaration.arraytutorials[0].choices[k].name);
  }
  if (showfilenames){
    console.log(resultarray);
  }
  checkjs(resultarray, showprogressbar);
}

//function to build the next interacive command in case the user wants to type in the filenames of tutorials
var input = function (){
  //console.log("input checked");
  ask(arraydeclaration.choiceinputarray);
}

//function to build the next input dialog in case the user wants to test specific tutorials ->user has to decide from which folder (torials or wip) he wants the files displayed for selection
var specific = function (){
  //console.log("specific checked");
  ask(arraydeclaration.choice2array);
}

//function to build the next input dialog in case the user wants to test specific files from tutorials folder
var specifictutorials = function (){
  //console.log("specific tutorials checked");
  ask(arraydeclaration.arraytutorials);
}

//function to build the next input dialog in case the user wants to test specific files from wip folder
var specificwip = function (){
  //console.log("specific wip checked");
  ask(arraydeclaration.arraywip);
}

////function to build the resultarray in case there was a specific selection of files done by the user
var singletutorialsselected = function (results){
  resultarray = results;
  if (showfilenames){
    console.log(resultarray);
  }
  checkjs(resultarray, showprogressbar);
}


//function in case the user gives an input which is a substring of filenames (and decided before, to test only in folder tutorials)
// 2 cases -> the user input ends with a * -->select all files which start with the input
//        --> the user input starts and ends with a * -->select all files which contains the input
var tutorialsgrouptutorials = function (results){
  for (var k=0; k < arraydeclaration.arraytutorials[0].choices.length; k++){
    var file = arraydeclaration.arraytutorials[0].choices[k].name;
    var fname = path.basename(file);
///checken ob stern am anfang, dann abschneiden und ein contains
    if (results.toString().startsWith("*")){
      if (fname.indexOf(results.toString().substr(1, results.length-2)) > -1){
        resultarray.push(arraydeclaration.arraytutorials[0].choices[k].name);
      }
    }
    else{
      if (fname.startsWith(results.toString().substr(0, results.length-1))){
        resultarray.push(arraydeclaration.arraytutorials[0].choices[k].name);
      }
    }
  }
  if (showfilenames){
    console.log(resultarray);
  }
  checkjs(resultarray, showprogressbar);
}


//function in case the user gives an input which is a substring of filenames (and decided before, to test only in folder tutorials)
// 2 cases -> the user input ends with a * -->select all files which start with the input
//        --> the user input starts and ends with a * -->select all files which contains the input
var wipgrouptutorials = function (results){
  for (var k=0; k < arraydeclaration.arraywip[0].choices.length; k++){
    var file = arraydeclaration.arraywip[0].choices[k].name;
    var fname = path.basename(file);
    if (results.toString().startsWith("*")){
      if (fname.indexOf(results.toString().substr(1, results.length-2)) > -1){
        resultarray.push(arraydeclaration.arraywip[0].choices[k].name);
      }
    }
    else{
///checken ob stern am anfang, dann abschneiden und ein contains
      if (fname.startsWith(results.toString().substr(0, results.length-1))){
        resultarray.push(arraydeclaration.arraywip[0].choices[k].name);
      }
    }
  }
  if (showfilenames){
    console.log(resultarray);
  }
  checkjs(resultarray, showprogressbar);
}


//function in case the user gives an input which is a substring of filenames in both folders (tutorials and wip)
// 2 cases -> the user input ends with a * -->select all files which start with the input
//        --> the user input starts and ends with a * -->select all files which contains the input
var allgrouptutorials = function (results){

  var resultarraytutorials = [];
  var resultarraywip = [];

  for (var k=0; k < arraydeclaration.arraywip[0].choices.length; k++){
    var file = arraydeclaration.arraywip[0].choices[k].name;
    var fname = path.basename(file);
    if (results.toString().startsWith("*")){
      if (fname.indexOf(results.toString().substr(1, results.length-2)) > -1){
        resultarraytutorials.push(arraydeclaration.arraywip[0].choices[k].name);
      }
    }
    else{
///checken ob stern am anfang, dann abschneiden und ein contains
      if (fname.startsWith(results.toString().substr(0, results.length-1))){
        resultarraytutorials.push(arraydeclaration.arraywip[0].choices[k].name);
      }
    }
  }

  for (var k=0; k < arraydeclaration.arraytutorials[0].choices.length; k++){
    var file = arraydeclaration.arraytutorials[0].choices[k].name;
    var fname = path.basename(file);
///checken ob stern am anfang, dann abschneiden und ein contains
    if (results.toString().startsWith("*")){
      if (fname.indexOf(results.toString().substr(1, results.length-2)) > -1){
        resultarraywip.push(arraydeclaration.arraytutorials[0].choices[k].name);
      }
    }
    else{
      if (fname.startsWith(results.toString().substr(0, results.length-1))){
        resultarraywip.push(arraydeclaration.arraytutorials[0].choices[k].name);
      }
    }
  }

//concatenate the 2 arrays with the results of wip folder and tutorials folder
  resultarray = resultarraytutorials.concat(resultarraywip);
  if (showfilenames){
    console.log(resultarray);
  }
  checkjs(resultarray, showprogressbar);


}

module.exports = {
  ask: ask,
  allgrouptutorials: allgrouptutorials,
  readtutorialnames: readtutorialnames,
  readwipnames: readwipnames,
  alltutorials: alltutorials,
  allfiles: allfiles,
  wip: wip,
  tutorials: tutorials,
  input: input,
  specific: specific,
  specifictutorials: specifictutorials,
  specificwip: specificwip,
  singletutorialsselected: singletutorialsselected,
  tutorialsgrouptutorials: tutorialsgrouptutorials,
  wipgrouptutorials: wipgrouptutorials,
  readallfilenames: readallfilenames,
  setShowfilename: setShowfilename,
  turnoffprogressbar: turnoffprogressbar
}
