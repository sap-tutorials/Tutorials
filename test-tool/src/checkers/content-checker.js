'use strict';

const path = require('path');
const fs = require('fs');
const { regexp, constraints } = require('../constants');

const fileExistsSyncCS = (filePath) => {
    var dir = path.dirname(filePath);
    var fileNames = fs.readdirSync(dir);
    return fileNames.includes(path.basename(filePath));
};

const checkLocalImage = (absImgPath, imgName) => {
    const result = [];
    const { content: { mdnImg } } = regexp;
    const exists = fileExistsSyncCS(absImgPath);
    if(exists) {
        const { size } = fs.statSync(absImgPath);
        if(size > constraints.img.MB) {
            results.push(`${mdnImg.messages.size} -> ${imgName}`);
        }
    } else {
        result.push(`${mdnImg.messages.existence} -> ${imgName}`);
    }
    return result;
};

module.exports = {
    check: (filePath, lines) => {
            let isCodeBlock = false;
            const result = [];
            const dir = path.dirname(filePath);
            const { content: { common, link, h1, mdnImg } } = regexp;
            lines.forEach((line, index) => {
                if(line.includes('```')) {
                    isCodeBlock = !isCodeBlock;
                }
                common.forEach(({ regexp, message }) => {
                    const match = line.match(regexp);
                    if(match) {
                        result.push({
                            line: index + 1,
                            msg: `${message} -> ${match[0]}`,
                        });
                    }
                });
                if(!isCodeBlock) {
                    const match = line.match(link.regexp);
                    if(match) {
                        result.push({
                            line: index + 1,
                            msg: `${link.message} -> ${match[0]}`
                        });
                    }
                    const h1Match = line.match(h1.regexp);
                    if(h1Match) {
                        result.push({
                            line: index + 1,
                            msg: `${h1.message} -> ${h1Match[0]}`
                        });
                    }
                }
                const match = line.match(mdnImg.regexp);
                if(match) {
                    const [ _ , imgName ] = match;
                    const filePath = path.join(dir, imgName);
                    const errors = checkLocalImage(filePath, imgName);
                    result.push(...errors.map(err => ({ line: index + 1, msg: err })));
                }
            });
            return result;    
    },
    
}