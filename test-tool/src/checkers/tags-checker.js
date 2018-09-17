const { regexp } = require('../constants');

module.exports =  {
  checkPrimaryTag: (line) => {
    const { tags: { primary_tag: primaryTag } } = regexp;
    const match = line.match(primaryTag.regexp);
    if (match) {
      const [tagsString] = match;
      const tags = tagsString.split(/(?<!\\),/).map(tag => tag.trim());

      if (tags.length > 1) {
        return `${primaryTag.message} -> ${tags.join(', ')}`;
      }
    }
  },

  checkExperienceTag: (line) => {
    const { tags: { experienceTag } } = regexp;

    if (line.startsWith('tags:')) {
      const result = line.match(experienceTag.regexp);

      if (!result) {
        // TODO: what if there are many tags ? multiline
        return `${experienceTag.message}`;
      }
    }
  },
};
