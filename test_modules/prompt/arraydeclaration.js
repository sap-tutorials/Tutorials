/******************************************************************************
declaration of arrays and json objects
to make the use of prompts via inquirer easier
******************************************************************************/
"use strict"

var exports = module.exports = {};

/****first json to build the first prompt via inquirer-> to make the first selection about which tutorials the user would like to test****/
exports.choicearray = [
  {
    type: "list",
    name: "scopetutorial",
    message: "Which tutorial would you like to test?",
    choices: [
      "all",
      "wip",
      "tutorials",
      "specific",
      "input"
    ]
  }
];

/**** json to build the second dialog/ prompt in case the user chooses sepecific selection in the first one
-> to make the next selection about which folder you would like to test****/
exports.choice2array = [
  {
    type: "list",
    name: "scopetutorial",
    message: "Which specific tutorials would you like to test?",
    choices: [
      "from wip",
      "from tutorials",
    ]
  }
];

/***json to build second dialog in case the user chooses input at first selection
->select wip or tutorial and then enter beginning of filenames */
exports.choiceinputarray = [
  {
    type: "list",
    name: "inputscopetutorial",
    message: "Which specific tutorials would you like to test?",
    choices: [
      "from wip",
      "from tutorials",
    ]
  },
  {
    type: 'input',
    name: 'inputselection',
    message: 'Which tutorials would you like to test? Enter beginning of filenames (you can start and or just end with *): '
  }
];

//help object - in case the user misses the * at the end of the input field he has to tip it in again
exports.choiceinputarray2 = [
  {
    type: "list",
    name: "inputscopetutorial",
    message: "Which specific tutorials would you like to test?",
    choices: [
      "from wip",
      "from tutorials",
    ]
  },{
    type: 'input',
    name: 'inputselection',
    message: 'input hast to end with * -> Which tutorials would you like to test? Enter beginning of filenames (you can start and or just end with *): '
  }
];

//declaration of the array with all tutorials (folder: tutorial) (the input with the files comes later in another funtion)
exports.arraytutorials = [
  {type: "checkbox",
  message: "Choose tutorials which you would like to test",
  name: "specifictutorials", //"specificselection",
  choices: [
    ]
  }
];

//declaration of the array with all work-in-progress tutorials (folder: work-in-progress) (the input with the files comes later in another funtion)
exports.arraywip = [
  {type: "checkbox",
  message: "Choose specific tutorials from wip",
  name: "specifictutorials", //"specificselection",
  choices: [
    ]
  }
];
