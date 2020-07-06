const logger = require('color-log');

const { logTemplates } = require('../constants');

const generateReport = (checkResult, filesCount) => {
    const {
        commonError,
        fileNameError,
        fileError,
        linkError,
    } = logTemplates;

    const reportStructure = JSON.parse(JSON.stringify(logTemplates.reportStructure));

    checkResult.results.map(result => {
        const {
            fileName,
            filePath,
            linksCount,
            fileNameCheckResult,
            spellCheckResult,
            contentCheckResult,
            syntaxCheckResult,
            tagsCheckResult,
            validationsCheckResult,
            linkCheckResult,
        } = result;

        if(fileNameCheckResult) {
            reportStructure.props.fileNames.messages.push(fileNameError(fileNameCheckResult, fileName, filePath));
        }
        if(spellCheckResult) {
            const errors = spellCheckResult.map(err => fileError(err.reason, fileName, err.line));
            if(errors.length) {
                reportStructure.props.spellCheck.messages.push(errors.join(''));
            }
        }
        if(contentCheckResult) {
            const result = contentCheckResult.reduce((log, err) => log.concat(fileError(err.msg, fileName, err.line)), []);
            if(result.length) {
                reportStructure.props.contentCheck.messages.push(...result);
            }
        }
        if(tagsCheckResult) {
            reportStructure.props.tagCheck.messages.push(...tagsCheckResult.map(e => fileError(e.msg, fileName, e.line)));
        }

        if(syntaxCheckResult) {
            reportStructure.props.syntaxCheck.messages.push(...syntaxCheckResult.map(e => fileError(e.msg, fileName, e.line)));
        }

        if(validationsCheckResult) {
            validationsCheckResult.forEach(msg => reportStructure.props.validationsCheck.messages.push(commonError(msg, fileName)));
        }
        if(linkCheckResult) {
            const { linkCheck: { files } } = reportStructure.props;
            reportStructure.meta.checkedLinksCount += linksCount;
            if(linkCheckResult.length) {
                const critical = linkCheckResult.filter(result => result.code == 404 && !result.isTrusted);
                const common = linkCheckResult.filter(result => result.code != 404 && !result.isTrusted);
                const warn = linkCheckResult.filter(result => result.isTrusted);
                files.push({
                    fileName,
                    criticalDeadLink: critical.map(({ link, code, line, reason }) => linkError(link, code, line, reason)),
                    deadLink: common.map(({ link, code, line, reason }) => linkError(link, code, line, reason)),
                    warnDeadLink: warn.map(({ link, code, line, reason }) => linkError(link, code, line, reason)),
                });
            }
        }
    });

    reportStructure.passed = checkResult.passed;

    return reportStructure;
};

const outputReportToConsole = (report, filesCount) => {
    const {
        genericReport,
        linksFileReport,
        linksGenericReport,
    } = logTemplates;

    if (filesCount === 0) {
        return logger.warn('No files tested');
    }

    Object.values(report.props).forEach(logProp => {
        if(logProp.type !== 'LINKS') {
            const log = genericReport(logProp.type, filesCount, logProp.messages.length, logProp.messages.join(''));
            logProp.messages.length ? logger.error(log) : logger.info(log);
        } else {
            const { files } = logProp;
            const linksLogger = linksReportProp => files.map((file) => file[linksReportProp].length ? linksFileReport(file.fileName, file[linksReportProp].join('')) : null).filter(log => log).join('');
            const linksCounter = linksReportProp => files.filter((file) => file[linksReportProp].length).reduce((acc, file) => acc + file[linksReportProp].length, 0);
            const criticalLog = linksLogger('criticalDeadLink');
            const commonLog = linksLogger('deadLink');
            const warnLog = linksLogger('warnDeadLink');

            if(criticalLog || commonLog) {
                if(criticalLog) {
                    logger.error(linksGenericReport('(404)', criticalLog, report.meta.checkedLinksCount, filesCount, linksCounter('criticalDeadLink')));
                }
                if(commonLog) {
                    logger.error(linksGenericReport('(Other)', commonLog, report.meta.checkedLinksCount, filesCount, linksCounter('deadLink')));
                }
            } else {
                logger.info(linksGenericReport('', '', report.meta.checkedLinksCount, filesCount, 0));
            }
            if(warnLog) {
                logger.warn(linksGenericReport('(TRUSTED or Tutorials Docs)', warnLog, report.meta.checkedLinksCount, filesCount, linksCounter('warnDeadLink')));
            }
        }
    });
    if(report.passed) {
        logger.info('Successful testing');
    } else {
        logger.error('Failed testing');
    }
};

module.exports = {
    generateReport,
    outputReportToConsole,
};
