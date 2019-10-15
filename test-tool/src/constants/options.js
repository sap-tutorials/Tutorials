const path= require('path');

const foldersOptions = {
    all: 'all',
    tutorials: 'tutorials',
    specific: 'specific',
    input: 'input',
};


const tutorialsFoldersOptions = {
    tutorials: 'from tutorials',
};

module.exports = {
  spellCheckOptions: {
    ignoreAcronyms: true,
    ignoreNumbers: true,
    suggestions: false,
    dictionary: {
      file: path.join(__dirname, '../../config/dictionary/en-us'),
    },
  },
    consoleInputOptionsDefinitions: [
        { name: 'help',        alias: 'h', type: Boolean },
        { name: 'all',         alias: 'a', type: Boolean },
        { name: 'tutorials',   alias: 't', type: Boolean },
        { name: 'specific',    alias: 's', type: Boolean },
        { name: 'input',       alias: 'i', type: String  },
        { name: 'file',        alias: 'f', type: Boolean },
        { name: 'progressbar', alias: 'p', type: Boolean },
        { name: 'guides',      alias: 'g', type: Boolean },
    ],
    inquirer: {
        foldersOptions,
        tutorialsFoldersOptions,
        folderChoice: [
            {
                type: "list",
                name: "scopetutorial",
                message: "Which folder or specific file would you like to test?",
                choices: [ ...Object.values(foldersOptions) ],
            },
        ],
        tutorialsChoiceByName: [
            {
                type: 'input',
                name: 'inputselection',
                message: 'Which tutorials would you like to test? Enter beginning of filenames (you can start and or just end with *): '
            }
        ],
        tutorialsChoiceByNameHandleError: [
            {
                type: 'input',
                name: 'inputselection',
                message: 'Input has to end with * -> Which tutorials would you like to test? Enter beginning of filenames (you can start and or just end with *): '
            }
        ],
        tutorialChoiceFromList: [
            {
                type: "checkbox",
                message: "Choose tutorials which you would like to test",
                name: "specifictutorials",
                choices: [],
            },
        ],
    },
};
