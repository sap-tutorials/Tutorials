const inquirer = require("inquirer");
const path = require('path');
const logger = require('color-log');
const commandLineArgs = require('command-line-args');
const recursive = require('recursive-readdir');

const { options, logTemplates } = require('../constants');

const readMethods = {};
const readMethodFactory = (projectPath, relativePath, exclusions) => () => recursive(path.join(projectPath, relativePath), exclusions);

const findByInput = (filePaths, input) => {
  const searchCriteria = input.replace('*', '');
  const comparator = input.startsWith('*') 
      ? filePath => path.basename(filePath).endsWith(searchCriteria)
      : filePath => path.basename(filePath).startsWith(searchCriteria);
  return filePaths.filter(comparator);
};

const handleRunWOOptions = async () => {
    const answer = await inquirer.prompt(options.inquirer.folderChoice);
    switch(answer.scopetutorial) {
      case options.inquirer.foldersOptions.all: 
        return handleAllChoice();
      case options.inquirer.foldersOptions.tutorials:
        return readMethods.readTutorialsPaths();
      case options.inquirer.foldersOptions.wip: 
        return readMethods.readWIPTutorialsPaths();
      case options.inquirer.foldersOptions.specific:
        return handleSpecificChoice();
      case options.inquirer.foldersOptions.input:
        return handleInput();
      default:
        return [];
    }
};

const handleInput = async () => {
  const folderAnswer = await inquirer.prompt(options.inquirer.tutorialsFolderChoice);
  let filesPaths = [];

  switch(folderAnswer.scopetutorial) {
    case options.inquirer.tutorialsFoldersOptions.wip: 
      filesPaths = await readMethods.readWIPTutorialsPaths(); break;
    case options.inquirer.tutorialsFoldersOptions.tutorials:
      filesPaths = await readMethods.readTutorialsPaths(); break;
    default:
      filesPaths = [];
  }

  let inputAnswer = await inquirer.prompt(options.inquirer.tutorialsChoiceByName);
  while(!inputAnswer.inputselection || !inputAnswer.inputselection.endsWith("*")) {
    inputAnswer = await inquirer.prompt(options.inquirer.tutorialsChoiceByNameHandleError);
  }
  return findByInput(filesPaths, inputAnswer.inputselection);
}

const handleSpecificChoice = async () => {
    const folderAnswer = await inquirer.prompt(options.inquirer.tutorialsFolderChoice);
    let questionConfig = [];
    let filesPaths = [];
    
    switch(folderAnswer.scopetutorial) {
      case options.inquirer.tutorialsFoldersOptions.wip: 
        filesPaths = await readMethods.readWIPTutorialsPaths();
        questionConfig =  options.inquirer.wipTutorialChoiceFromList;
        break;
      case options.inquirer.tutorialsFoldersOptions.tutorials:
        filesPaths = await readMethods.readTutorialsPaths();
        questionConfig = options.inquirer.tutorialChoiceFromList;
        break;
      default:
    }

    const [ questionsOption ] = questionConfig;
    const filesAnswer = await inquirer.prompt([ { ...questionsOption, choices: filesPaths } ]);
    return filesAnswer.specifictutorials;
};

const handleInputChoice = async (inputOptions) => {
    let filePaths = [];
    if ((inputOptions.tutorials && inputOptions.wip) || (!inputOptions.tutorials && !inputOptions.wip)) {
      const tutorialsPaths = await readMethods.readTutorialsPaths();
      const wipTutorialsPaths = await readMethods.readWIPTutorialsPaths();
      filePaths = [ ...tutorialsPaths, ...wipTutorialsPaths ];
    } else if (inputOptions.wip) {
      filePaths = await readMethods.readWIPTutorialsPaths();
    } else if (inputOptions.tutorials) {
      filePaths = await readMethods.readTutorialsPaths();
    }
    return findByInput(filePaths, inputOptions.input);
};

const handleAllChoice = async () => {
    const tutorialsPaths = await readMethods.readTutorialsPaths();
    const wipTutorialsPaths = await readMethods.readWIPTutorialsPaths();
    const restFilesPaths = await readMethods.readRestFilesPaths();
    return [ 
      ...tutorialsPaths,
      ...wipTutorialsPaths,
      ...restFilesPaths,
    ];
};

const interact = async (projectPath) => {
    readMethods.readTutorialsPaths = readMethodFactory(projectPath, './tutorials', ['!*.md']);
    readMethods.readWIPTutorialsPaths = readMethodFactory(projectPath, './work-in-progress', ['!*.md', 'stopwords.md', 'readme.md']);
    readMethods.readRestFilesPaths = readMethodFactory(projectPath, './', ['!*.md', 'tutorials\*', 'node_modules\*', 'templates\*', 'work-in-progress\*', ""]);

    const inputOptions = commandLineArgs(options.consoleInputOptionsDefinitions);
    let filePaths = [];
    if(inputOptions.help) {
        logger.info(logTemplates.optionsHelp);
    } else if(inputOptions.all){
        filePaths = await handleAllChoice();
    } else if(inputOptions.guides && inputOptions.tutorials) {
        const tutorialsPaths = await readMethods.readTutorialsPaths();
        const restFilesPaths = await readMethods.readRestFilesPaths();
        filePaths = [ 
          ...tutorialsPaths,
          ...restFilesPaths,
        ];
    } else if(inputOptions.tutorials && inputOptions.wip && !inputOptions.input) {
        const tutorialsPaths = await readMethods.readTutorialsPaths();
        const wipTutorialsPaths = await readMethods.readWIPTutorialsPaths();
        filePaths = [ 
          ...tutorialsPaths,
          ...wipTutorialsPaths,
        ];
    } else if(inputOptions.tutorials && !inputOptions.wip && !inputOptions.input) {
        filePaths = await readMethods.readTutorialsPaths();
    } else if(inputOptions.wip && !inputOptions.tutorials && !inputOptions.input) {
        filePaths = await readMethods.readWIPTutorialsPaths();
    } else if(inputOptions.specific) {
        filePaths = await handleSpecificChoice();
    } else if(inputOptions.input) {
        filePaths = await handleInputChoice(inputOptions);
    } else {
      if (!inputOptions.tutorials && !inputOptions.wip && !inputOptions.all && !inputOptions.help && !inputOptions.specific && !inputOptions.input) {
        filePaths = await handleRunWOOptions();
      }
      else {
        logger.error(logTemplates.optionsError);
      }
    }
    if(inputOptions.file) {
      logger.info('Files:', filePaths.map(filePath => path.basename(filePath)));
    }
    return {
      filePaths,
      withProgressTracker: inputOptions.progressbar,
    };
};

module.exports = {
  interact,
};