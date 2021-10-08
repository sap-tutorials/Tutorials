---
title: Build Skeleton React and UI5 Projects
description: Create skeleton React and UI5 projects and install Luigi.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>javascript]
primary_tag: topic>user-interface
---

## Prerequisites
 - It is recommended to try the simpler examples on [GitHub](https://github.com/SAP/luigi/tree/master/core/examples) or the [Getting Started page](https://docs.luigi-project.io/docs/getting-started/?section=examples) before this tutorial.
 - You need to install [Node.js](https://nodejs.org/en/download/current/). If you already have an old version installed on your machine, please run `npm install npm@latest -g`.
 - You need to install [SAP Fonts](https://experience.sap.com/fiori-design-web/downloads/#sap-icon-font).

## Details
### You will learn
  - How to create a skeleton React project
  - How to create a skeleton UI5 project
  - How to add Luigi and other dependencies to your project

---

[ACCORDION-BEGIN [Step 1:](Create React app)]

In this step, you will create a React skeleton project which will be used to create your Luigi app.

1. Open a Terminal or Command Prompt window and navigate to the space where you want to install the app. Then create a new folder:

    ```Shell
    mkdir luigi-react-ui5
    cd luigi-react-ui5
    ```

2. Run this command to install the React package for creating an app:

    ```Shell
    npm i create-react-app
    ```

3. Copy and paste this code into the Terminal/Command Prompt. This is a script which has been adapted from one of [Luigi's templates](https://docs.luigi-project.io/docs/application-setup?section=application-setup-for-react) to create a React app with sample views:

    ```Shell
    npx create-react-app my-react-app && cd my-react-app

    # eject project to customize webpack configs
    echo yes | npm run eject

    # install dependencies
    npm i -P @luigi-project/core @luigi-project/client fundamental-styles@0.11.0 @sap-theming/theming-base-content react-router-dom webcomponents-react
    npm i copy-webpack-plugin@5 webpack webpack-cli @babel/core @babel/preset-env babel-loader --save-dev

    # replace strings in some places
    sed "s/const HtmlWebpackPlugin = require('html-webpack-plugin');/const HtmlWebpackPlugin = require('html-webpack-plugin');const CopyWebpackPlugin = require('copy-webpack-plugin');/g" config/webpack.config.js > config/webpack.config.tmp.js && mv config/webpack.config.tmp.js config/webpack.config.js
    sed "s/new HtmlWebpackPlugin(/new CopyWebpackPlugin([\
    {context: 'public', to: 'index.html', from: 'index.html'  },\
    {context: 'node_modules\/@luigi-project\/core',to: '.\/luigi-core',from: {glob: '**',dot: true}}],\
    {ignore: ['.gitkeep', '**\/.DS_Store', '**\/Thumbs.db'],debug: 'warning'\
    }),\
    new HtmlWebpackPlugin(/g" config/webpack.config.js > config/webpack.config.tmp.js && mv config/webpack.config.tmp.js config/webpack.config.js
    sed "s/template: paths.appHtml,/template: paths.appHtml,\
    filename: 'sampleapp.html',/g" config/webpack.config.js > config/webpack.config.tmp.js && mv config/webpack.config.tmp.js config/webpack.config.js
    sed "s/public\/index.html/public\/sampleapp.html/g" config/paths.js > config/paths.tmp.js && mv config/paths.tmp.js config/paths.js
    sed "s/publicUrl + '\/index.html',/publicUrl + '\/sampleapp.html',/g" config/webpack.config.js > config/webpack.config.tmp.js && mv config/webpack.config.tmp.js config/webpack.config.js
    sed "s/const isWsl = require('is-wsl');//g" config/webpack.config.js > config/webpack.config.tmp.js && mv config/webpack.config.tmp.js config/webpack.config.js
    sed "s/!isWsl/true/g" config/webpack.config.js > config/webpack.config.tmp.js && mv config/webpack.config.tmp.js config/webpack.config.js

    echo "const path = require('path');

    module.exports = {
        entry: './src/luigi-config/luigi-config.es6.js',
        output: {
            filename: 'luigi-config.js',
            path: path.resolve(__dirname, 'public'),
        },
    };">webpack.config.js

    sed 's/"scripts": {/"scripts": {\
    \    "buildConfig":"webpack --config webpack.config.js",/1' package.json > p.tmp.json && mv p.tmp.json package.json

    echo '{
        "globals": {
            "Luigi": "readonly"
        }
    }'>.eslintrc.json

    # downloads
    mkdir -p src/luigi-config
    curl https://github.com/SAP-samples/luigi-micro-frontend-application/blob/main/react-core-mf/src/views/home.js > public/index.html
    curl https://github.com/SAP-samples/luigi-micro-frontend-application/blob/main/react-core-mf/src/views/product.js > public/sampleapp.html
    curl https://github.com/SAP-samples/luigi-micro-frontend-application/blob/main/react-core-mf/src/views/productDetail.js > src/luigi-config/luigi-config.es6.js


    # add index.js
    curl https://raw.githubusercontent.com/SAP/luigi/master/core/examples/luigi-example-react/src/index.js > src/index.js
    curl https://raw.githubusercontent.com/SAP/luigi/master/core/examples/luigi-example-react/src/index.css > src/index.css

    # add views
    mkdir src/views
    curl https://raw.githubusercontent.com/SAP/luigi/master/core/examples/luigi-example-react/src/views/home.js > src/views/home.js
    curl https://raw.githubusercontent.com/SAP/luigi/master/core/examples/luigi-example-react/src/views/sample1.js > src/views/sample1.js
    curl https://raw.githubusercontent.com/SAP/luigi/master/core/examples/luigi-example-react/src/views/sample2.js > src/views/sample2.js

    npm i
    npm run buildConfig
    npm start
    ```

    ```Shell
    npx create-react-app react-core-mf
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create UI5 micro-frontend)]

In this step, you will create a skeleton UI5 project for your UI5 micro-frontend.

1. Navigate to the root folder of your project.

2. Create a new folder:

    ```Shell
    mkdir ui5-mf
    cd ui5-mf
    ```

3. Install the UI5 project generator:

    ```Shell
    npm install -g yo generator-easy-ui5
    ```

4. Type `yo easy-ui5` in the terminal and choose `generator-ui5-project`. Use the following settings:

    ![UI5 Terminal](ui5-yo.png)


[VALIDATE_1]
[ACCORDION-END]




---
