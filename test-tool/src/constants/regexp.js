module.exports = {
    content: {
        common: [
            {
                regexp: new RegExp('\\[\\]\\((.*?)\\)'),
                message: 'empty alt-text tag for link/image',
            },
            {
                regexp: new RegExp('\\!\\[(.*\\.png.*)\\]\\(.*\\)'),
                message: 'no filenames in alt-text for images allowed',
            },
            {
                regexp: new RegExp('\\[(.{1,2}|\\s{1,})\\]\\(.*\\)'),
                message: 'conventions of alt-text for link/image are not observed (at least 3 characters, not only spaces)',
            },
            {
                regexp: new RegExp('\\>###'),
                message: 'no message box typo'
            },
            {
                regexp: new RegExp('\\[.*\]\\(.*\\.exe\\)'),
                message: 'no suspicious file types in links'
            },
            {
                regexp: new RegExp('\\[.*\\]\\(\\)'),
                message: 'empty URL field'
            },
            {
                regexp: new RegExp('\u201C'),
                message: 'curly quotes found. change to straight using quotes key'
            },
            {
                regexp: new RegExp('\u201D'),
                message: 'curly quotes found. change to straight using quotes key'
            },
            {
                regexp: new RegExp('\u2018'),
                message: 'curly single quotes found. change to straight using apostrophe key'
            },
            {
                regexp: new RegExp('\u2019'),
                message: 'curly single quotes found. change to straight using apostrophe key'
            },
        ],
        link: {
            regexp: new RegExp('(?<![`\\(\\[]|(href=")|(link=")|(src="))(http|ftp|https):\\/\\/([\\w_-]+(?:(?:\\.[\\w_-]+)+))([\\w.,@?^=%&:/~+#-]*[\\w@?^=%&/~+#-])\\/?(?=([^`]*`[^`]*`)*[^`]*$)(?!(([^\\[]*\\])|([^\\<]*\\>)|([^\\(]*\\))))'),
            message: 'plain text URL'
        },
        h1: {
            regexp: new RegExp('^(# )\\w+'),
            message: 'no H1 (single #) allowed',
        },
        mdnImg: {
            regexp: new RegExp('\\!\\[[^\\]]+\\]\\((?!http)(.+?)\\)'),
            messages: {
                size: 'file size is more than 1 MB',
                existence: 'missing image',
            }
        },
    },
    validation: {
        auto_validation: /auto_validation:\s(.*)\r?\n/i,
        accordions: /(?<=ACCORDION-BEGIN).*?(?=ACCORDION-END)/sg,
        validate: /(?<=\[VALIDATE_)[0-9]+(?=\])/g,
        validate_vr: validationNumber => new RegExp(`\\[VALIDATE_${validationNumber}\\].*?\\[VALIDATE_${validationNumber}\\]`, 'sig'),
        done: /\[DONE\]/g,
        codeBlock: /```.*?```/sgi,
        codeLine: /`.*?`/gi,
        messages: {
            production: {
                rules_vr: 'VALIDATION: rules.vr file must not be presented in the production',
                auto_validation: `VALIDATION: Value of auto_validation property must be set to 'true' in the production`,
            },
            validate_restrictions: {
                at_least_one: 'VALIDATION: Tutorial must have at least one validate element',
                duplicates: num => `VALIDATION: Tutorial has duplicate VALIDATE_${num} tags`,
                duplicates_vr: num => `VALIDATION: Duplicate VALIDATE_${num} in rules.vr file`,
                not_defined: num => `VALIDATION: VALIDATE_${num} not defined in rules.vr file`,
                missed_validation: stepNum => `VALIDATION: Step number ${stepNum} missing validate/done element`,
            },
            rules_vr_missed: 'VALIDATION: Missing rules.vr file',
            validate_wo_property: 'VALIDATION: Tutorial has validation but no auto_validation property',
        }
    },
    tags: {
        primary_tag: {
            regexp: /primary_tag:\s?\[?(.*?)\]?\r?\n/i,
            message: 'More than one primary tag specified',
        },
      experienceTag: {
        regexp: /tutorial>(beginner)|(intermediate)|(advanced)/g,
        message: 'experience tag is required',
      },
    },
    link: {
        absoluteURL: new RegExp('^[a-z][a-z0-9+.-]*:'),
        markdown: [
            /\[[^\]]*\]\((http[s]?:\/\/.+?)\)/g,
            /\[[^\]]*\]\s*?:\s*?<(http[s]?:\/\/.+?)>/g,
            /<(http[s]?:\/\/.*?)>/g
        ],
        pure: /[-a-zA-Z0-9@:%_\+.~#?&//=]{2,256}\.[a-z]{2,4}\b(\/[-a-zA-Z0-9@:%_\+.~#?&//=]*)?/gi
    },
    fileName: {
        restrictedSymbols: new RegExp('[^a-z0-9-]'),
    }
};
