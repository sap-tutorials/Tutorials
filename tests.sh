#!/bin/bash

cp dependency-mods/files_check.js node_modules/grunt-files-check/tasks/files_check.js
cp dependency-mods/logger.js node_modules/grunt-deadlink/tasks/logger.js
grunt test -v
