#!/bin/bash

cp dependency-mods/grunt-files-check/tasks/files_check.js node_modules/grunt-files-check/tasks/files_check.js
cp dependency-mods/grunt-deadlink/tasks/logger.js node_modules/grunt-deadlink/tasks/logger.js
grunt test -v
