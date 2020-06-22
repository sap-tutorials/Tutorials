module.exports = {
    content: {
        common: [
            {
                regexp: new RegExp('\\[\\]\\((.*?)\\)'),
                message: 'empty alt-text tag for link/image',
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
            regexp: new RegExp('(?<![`\\(\\[]|(href=")|(link=")|(src=")|(\<))(http|ftp|https):\\/\\/([\\w\d_-]+(?:(?:\\.?[\\w\d_-]+)+))([\\w\d.,@?^=%&:/~+#-]*[\\w\d@?^=%&/~+#-])\\/?(?=([^`]*`[^`]*`)*[^`]*$)(?!(([^\\[]*\\])|(\>)|([^\\<]*\\>)|([^\\(]*\\))))'),
            message: 'plain text URL',
            description: 'wrap URL in <> or format with [Link text](URL)',
        },
        emptyLink: [
            {
                regexp: /\[.*\]\(\)/,
                message: 'empty URL field'
            },
            {
                regexp: /href=["']["']/,
                message: 'empty URL field'
            },
        ],
        h1: {
            regexp: new RegExp('^(# )\\w+'),
            message: 'no H1 (single #) allowed',
        },
        mdnImg: {
            regexp: /\!?\[\s*[^\]]+\s*\]\s*?\(\s*(?![ <]*http)([\d\w\s_\-\.\/]+\.(jpg|jpeg|png|gif|svg|ico))\s*\)/gi,
            messages: {
                size: 'file size is more than 1 MB',
                existence: 'missing image',
                wrongAlt: 'no filenames in alt-text for images allowed'
            }
        },
        tutorialLinkInvalid: {
            regexp: /\[[^\]]+\]\((?![ ]*http)([\w\d\-]+\.html)\)/i,
            message: 'Incorrect link: If you want link to tutorial, use tutorial name without ".html". If you want external link, use full URL with "http/https"',
        },
        tutorialLink: {
            regexp: /\[[^\]]+\]\((?![ <]*http)[\w\d\-]+\)/i,
            message: 'Tutorial with this name doesn\'t exist',
        },
        localFileLink: {
            regexp: /\[[^\]]+\]\((?![ <]*(http|mission\.|group\.))([a-z\-_A-Z0-9]+?)\.[a-z]{2,10}\)/i,
            message: 'Incorrect link to local file, use full link to file on GitHub (starting https://raw.githubusercontent.com)',
        },
        internalLink: {
            regexp: new RegExp('(sap\.corp)'),
            message: 'Internal link'
        },
        remoteImage: {
            regexp: /!\[[^\]]+\]\(http[s]?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*)\)/gi,
            message: 'Bad image - image URL does not return image file',
        },
        codeBlockInNote: /^>\s*```/,
        metadata: {
          title: {
            regexp: /^\s*title:\s*.+$/,
            message: 'title is required',
          },
          description: {
            regexp: /^\s*description:\s*.+$/,
            message: 'description is required',
          },
          tags: {
            regexp: /^\s*tags:\s*\[(.+)>(.+),?\s*\]\s*$/,
            message: 'tags are required',
          },
          primaryTag: {
            regexp: /^\s*primary_tag:\s*(.+)>(.+)\s*$/,
            message: 'primary tag is required',
          },
      },
    },
    validation: {
        auto_validation: /auto_validation:\s(.*)\r?\n/i,
        accordions: /(?<=ACCORDION-BEGIN).*?(?=ACCORDION-END)/sg,
        validate: /(?<=\[VALIDATE_)[0-9]+(?=\])/g,
        validate_vr: validationNumber => new RegExp(`\\[VALIDATE_${validationNumber}\\].*?\\[VALIDATE_${validationNumber}\\]`, 'sig'),
        done: /\[DONE\]/g,
        codeBlock: /```.*?```/sgi,
        codeLine: /(^|[^`])?(`[^`]+`)([^`]|$)?/gi,
        inlineCodeBlock: /`?``.[^\n^\r]*`?``/gi,
        // not using g flag, to capture only the first --- .... --- entry
        metaData: /---[\0-\uFFFF]*?---/i,
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
    options: {
        regexps: {
            full: /(?:(\[OPTION BEGIN \[.+\]\]))[\0-\uFFFF]*?(?=(\[OPTION END\]))/g,
            start:/(?:(\[OPTION BEGIN \[.+\]\]))[\0-\uFFFF]*?/g,
            end: /[\0-\uFFFF]*?(?=\[OPTION END\])/g,
            title: /(?:(\[OPTION BEGIN \[)).+(?=(\]\]))/g,
            contentBetween: /(?:(\[OPTION END\]))[\w\d\s]+(?=(\[OPTION BEGIN \[.+\]\]))/g,
        },
        messages: {
            oneOption: 'If you use conditional tab, you must have at least 2 options',
            duplicate: '2 or more options with the same name',
            mess: 'You must have BEGIN and END tags for option tabs (syntax error) ',
            contentBetween: 'No content between options allowed',
        },
    },
    tags: {
        primary_tag: {
            regexp:  /(?<=primary_tag:)\s?\[?[\w\s>,\-]*\]?\r?\n?/i,
            message: 'More than one primary tag specified',
        },
        experienceTag: {
            regexp: /tutorial>(beginner)|(intermediate)|(advanced)/g,
            message: 'experience tag is required',
        },
    },
    link: {
        authorProfile: /^\s*author_profile:/,
        absoluteURL: new RegExp('^[a-z][a-z0-9+.-]*:'),
        markdown: [
            /\[[^\]]*\]\s*?\((http[s]?:\/\/.+?)\)/g,
            /\[[^\]]*\]\s*?:\s*?<(http[s]?:\/\/.+?)>/g,
            /<(http[s]?:\/\/.*?)>/g,
            /href=["'](http[s]?:\/\/(www\.)?[-a-zA-Z0-9@:%._\+~#=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%_\+.~#?&//=]*))["']/g,
        ],
        pure: /(http[s]?:\/\/.*?)[-a-zA-Z0-9@:%_\+.~#?&//=]{2,256}\.[a-z]{2,4}\b(\/[-a-zA-Z0-9@:%_\+.~#?&//=]*)?/gi
    },
    fileName: {
        restrictedSymbols: new RegExp('[^a-z0-9-]'),
    },
};
