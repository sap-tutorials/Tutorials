const { regexp } = require('../constants');

module.exports = {
    checkPrimaryTag: (fileSrc) => {
        const { tags: { primary_tag } } = regexp;
        const match = fileSrc.match(primary_tag.regexp);
        if(match) {
            const [keyWithTags, tagsString] = match;
            const tags = tagsString.split(/(?<!\\),/).map(tag => tag.trim());
            if(tags.length > 1) {
                return `${primary_tag.message} -> ${keyWithTags}`;
            }
        }
    },
  checkExperienceTag: (fileSrc) => {
    const { tags: { experienceTag } } = regexp;
    const result = fileSrc.match(experienceTag.regexp);

    if (!result) {
      return `${experienceTag.message}`;
    }
  },
};
