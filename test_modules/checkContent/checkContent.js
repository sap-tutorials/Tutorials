'use strict';

var path = require('path');
var fs = require('fs');

var regexs = [
    new RegExp('\\[\\]\\((.*?)\\)'), //require alt tag for images
    new RegExp('\\!\\[(.*\\.png.*)\\]\\(.*\\)'), // check for .png namings
    new RegExp('[^!]\\[.*?(here|there|file|folder|this|page)\\]\\((.*?)\\)'), // avoid useless url names
    new RegExp('\\[(.{1,2}|\\s{1,})\\]\\(.*\\)'), // conventions of alt-text for images are not observed (at least 3 characters, not only spaces)
    new RegExp('\\>###'), // avoid message box typo //(^\s*(\>){1,})\s*(\w){1,}
    new RegExp('\\[.*\]\\(.*\\.exe\\)'), // prohibit suspicious filetypes
    new RegExp('\\[.*\\]\\(\\)'), //\[.*\]\(\)
    new RegExp('\u201C'), //left double quote
    new RegExp('\u201D'), //right double quote
    new RegExp('\u2018'), //left single quote
    new RegExp('\u2019'), //right single quote
];

//messages for regular expressions
var msg = [
    "empty alt-text tag for link/image",
    "no filenames in alt-text for images allowed",
    "no useless hyperlinked terms",
    "conventions of alt-text for link/image are not observed (at least 3 characters, not only spaces)",
    "no message box typo",
    "no suspicious file types in links",
    "empty URL field",
    "curly quotes found. change to straight using quotes key",
    "curly quotes found. change to straight using quotes key",
    "curly single quotes found. change to straight using apostrophe key",
    "curly single quotes found. change to straight using apostrophe key",
];


const h1 = new RegExp('^(# )\\w+');
const linkRegExp = new RegExp('(?<![`\\(\\[]|(href=")|(link=")|(src="))(http|ftp|https):\\/\\/([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:/~+#-]*[\\w@?^=%&/~+#-])\\/?(?=([^`]*`[^`]*`)*[^`]*$)(?!(([^\\[]*\\])|([^\\<]*\\>)|([^\\(]*\\))))');
const markdownImageRegExp = new RegExp('\\!\\[[^\\]]+\\]\\((?!http)(.+?)\\)');
const primaryTag = /primary_tag:\s?\[?(.*?)\]?\r?\n/i;
const autoValidation = /auto_validation:\s(.*)\r?\n/i;
const accordions = /(?<=ACCORDION-BEGIN).*?(?=ACCORDION-END)/sg;
const validate = /(?<=\[VALIDATE_)[0-9]+(?=\])/g;
const done = /\[DONE\]/g;
const codeBlock = /```.*?```/sgi;
const codeLine = /`.*?`/gi;


const MB = Math.pow(2, 20);

const fileExistsSyncCS = (filePath) => {
    var dir = path.dirname(filePath);
    var fileNames = fs.readdirSync(dir);
    return fileNames.includes(path.basename(filePath));
};

const findDuplicates = (arr) => {
    const map = new Map();
    const result = [];
    arr.forEach(el => {
        let counter = map.has(el) ? map.get(el) : 0
        map.set(el, ++counter);
    });
    map.forEach((value, key) => {
        if(value > 1) {
            result.push(key);
        }
    });
    return result;
}

module.exports = {
    check: (file, lines) => {
        if (path.parse(file).dir.split(path.sep)[0] == "tutorials" || path.parse(file).dir.split(path.sep)[0] == "work-in-progress") {
            let isCodeBlock = false;
            const result = [];
            const dir = path.dirname(file);
            lines.forEach((line, index) => {
                if(line.includes('```')) {
                    isCodeBlock = !isCodeBlock;
                }
                regexs.forEach((regex, id) => {
                    var matchResult = line.match(regex); 
                    if (matchResult !== null) {
                        result.push({
                            line: index + 1,
                            msg: `${msg[id]} -> ${id >= 1 && id <= 3 ? matchResult[0].split("[", 2)[1].split("]", 2)[0] : matchResult[0]}`
                        });
                    }
                });
                if(!isCodeBlock) {
                    const match = line.match(linkRegExp);
                    if(match) {
                        result.push({
                            line: index + 1,
                            msg: `plain text URL -> ${match[0]}`
                        });
                    }
                    const h1Match = line.match(h1);
                    if(h1Match) {
                        result.push({
                            line: index + 1,
                            msg: `no H1 (single #) allowed -> ${h1Match[0]}`
                        });
                    }
                }
                const match = line.match(markdownImageRegExp);
                if(match) {
                    const filePath = path.join(dir, match[1]);
                    const exists = fileExistsSyncCS(filePath);
                    if(exists) {
                        const { size } = fs.statSync(filePath);
                        if(size > MB) {
                            result.push({
                                line: index + 1,
                                msg: `file size is more than 1 MB -> ${match[1]}`,
                            });
                        }
                    } else {
                        result.push({
                            line: index + 1,
                            msg: `missed local image -> ${match[1]}`,
                        });
                    }
                }
            });
            return result;
        }     
    },
    checkTags: (fileSrc) => {
        const match = fileSrc.match(primaryTag);
        if(match) {
            const [keyWithTags, tagsString] = match;
            const tags = tagsString.split(/(?<!\\),/).map(tag => tag.trim());
            if(tags.length > 1) {
                return `More than one primary tag specified -> ${keyWithTags}`;
            }
        }
    },
    checkValidation: (fileSrc, filePath, isProduction) => {
        const err = [];
        const autoValidationMatch = fileSrc.match(autoValidation);
        const steps = fileSrc.replace(codeBlock, '').replace(codeLine, '').match(accordions);
        let validationExists, noAnyValidation;
        if(steps) {
            validationExists = steps.some(step => step.match(validate));
            noAnyValidation = steps.map((step, index) => ({ step, id: index + 1})).filter(({ step }) => !step.match(validate) && !step.match(done));
        }
        if(autoValidationMatch) {
            const [prop, value] = autoValidationMatch;
            const dirPath = path.dirname(filePath);
            const rulesPath = path.join(dirPath, 'rules.vr');
            const vrFileExists = fs.existsSync(rulesPath);
            if(isProduction) {
                if(vrFileExists) {
                    err.push({
                        err: 'VALIDATION: rules.vr file must not be presented in the production',
                    });
                }
                if(value !== 'true') {
                    err.push({
                        err: `VALIDATION: Value of auto_validation property must be set to 'true' in the production`,
                    });
                }
            }
            if(value && (value == 'true' || value == 'false')) {  
                if(!validationExists) {
                    err.push({
                        err: 'VALIDATION: Tutorial must have at least one validate element',
                    });
                } else {
                    const validationsMatch = steps
                        .map(step => step.match(validate))
                        .filter(match => match)
                        .reduce((arr, matches) => arr.concat(matches), []);
                    const duplicates = findDuplicates(validationsMatch);
                    if(duplicates.length) {
                        duplicates.forEach(duplicate => err.push({
                            err: `VALIDATION: Tutorial has duplicate VALIDATE_${duplicate} tags`,
                        }));
                    }
                    if(!isProduction) {
                        if (vrFileExists) {
                            const vrFile = fs.readFileSync(rulesPath, 'utf-8');
                            (new Set(validationsMatch)).forEach(validationNumber => {
                                const validateBlock = vrFile.match(new RegExp(`\\[VALIDATE_${validationNumber}\\].*?\\[VALIDATE_${validationNumber}\\]`, 'sig'));
                                if(!validateBlock) {
                                    err.push({
                                        err: `VALIDATION: VALIDATE_${validationNumber} not defined in rules.vr file`,
                                    });
                                } else if (validateBlock.length > 1) {
                                    err.push({
                                        err: `VALIDATION: Duplicate VALIDATE_${validationNumber} in rules.vr file`,
                                    });
                                }
                            });
                        } else {
                            err.push({
                                err: 'VALIDATION: Missing rules.vr file',
                            });
                        }
                    }
                }
                if(noAnyValidation.length) {
                    noAnyValidation.forEach(({ step, id }) => {
                        err.push({
                            err: `VALIDATION: Step number ${id} missing validate/done element`,
                        });
                    });
                }
            }
        } else if(validationExists) {
            err.push({
                err: `VALIDATION: Tutorial has validation but no auto_validation property`,
            });
        }
        return err;
    },
};

