const commonError = (reason, fileName) => `
    >Error:
        file: ${fileName}
        reason: ${reason}
`;

const fileNameError = (reason, fileName, filePath) => `
  >Error:
      file: ${fileName}
      path: ${filePath}
      reason: ${reason}
`;

const fileError = (reason, fileName, line) => `
  >Error:
      file: ${fileName}
      line: ${line}
      reason: ${reason}
`;

const linkError = (url, statusCode, line, reason) => `
        url: ${url}
        line: ${line}
        code: ${statusCode}${ reason ? '\n        reason: ' + reason : '' }
`;

const genericReport = (checkType, filesCount, errorsCount, log) => `
CHECK ${checkType}:
  ${log}
  >>> ${filesCount} File(s) checked ${errorsCount ? `with ${errorsCount} Error(s)` : 'without Error(s)'}

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
`;

const linksFileReport = (fileName, log) => `
    > Deadlink(s) found in ${fileName}: 
    ${log}
`;

const linksGenericReport = (type, log, checkedLinksCount, checkedFilesCount, deadLinksCount) => `
CHECK LINKS ${type}:
  ${log}
  >>> ${checkedLinksCount} Link(s) checked in ${checkedFilesCount} File(s) ${deadLinksCount ? `with ${deadLinksCount} Error(s)` : 'without Error(s)'}
`;

const reportStructure = {
  meta: {
    checkedLinksCount: 0,
  },
  props: {
    fileNames: {
      type: 'FILENAMES',
      messages: [],
    },
    spellCheck: {
      type: 'SPELLING',
      messages: [],
    },
    contentCheck: {
      type: 'CONTENT',
      messages: [],
    },
    tagCheck: {
      type: 'TAGS',
      messages: [],
    },
    validationsCheck: {
      type: 'VALIDATION',
      messages: [],
    },
    syntaxCheck: {
      type: 'SYNTAX',
      messages: [],
    },
    linkCheck: {
      type: 'LINKS',
      files: []
    },
  },
};

const optionsHelp = `
HELP: Your possibilities for the input:
      -h or --help for Help 
      -f: to log all filenames, which get tested 
      -a: for testing everything (including files, which aren't tutorials) 
      -g: only in combination with t: for testing everything except from the work-in-progress folder (including files, which aren't tutorials) 
      -w & -t: for testing every tutorial in both folders 
      -w: for testing all files in the work-in-progress folder 
      -t: for testing all files in the tutorials folder 
      -s: for dialog to choose specific tutorials 
      -i: to give an filename behind with *
          to resctrict the scope: enter -w or/and -t
      -p: to turn off the progressbar `;

const optionsError = `Error: no available command identified )
       prompt --help or -h for more info `;

const progressBar = 'Testing: [:bar] :percent | Execution time: :elapseds';

module.exports = {
  commonError,
  fileNameError,
  fileError,
  linkError,
  genericReport,
  linksFileReport,
  linksGenericReport,
  reportStructure,
  optionsHelp,
  optionsError,
  progressBar,
};
