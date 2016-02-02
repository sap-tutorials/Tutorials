#!/bin/bash

cp dependency-mods/grunt-files-check/tasks/files_check.js node_modules/grunt-files-check/tasks/files_check.js
cp dependency-mods/grunt-deadlink/tasks/logger.js node_modules/grunt-deadlink/tasks/logger.js
cp dependency-mods/grunt-filenames/tasks/filenames.js node_modules/grunt-filenames/tasks/filenames.js
grunt test -v
