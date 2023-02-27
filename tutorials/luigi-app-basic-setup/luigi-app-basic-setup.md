---
parser: v2
auto_validation: true
time: 5
tags: [ tutorial>beginner, programming-tool>javascript]
primary_tag: topic>user-interface
---

# Build Skeleton React and UI5 Projects
<!-- description --> Create skeleton React and UI5 projects and install Luigi.

## Prerequisites
 - It is recommended to try the simpler examples on [GitHub](https://github.com/SAP/luigi/tree/master/core/examples) or the [Getting Started page](https://docs.luigi-project.io/docs/getting-started/?section=examples) before this tutorial.
 - You need to install [Node.js](https://nodejs.org/en/download/current/). If you already have an old version installed on your machine, please run `npm install npm@latest -g`.
 - You need to install [SAP Fonts](https://experience.sap.com/fiori-design-web/downloads/#sap-icon-font).

## You will learn
  - How to create a skeleton React project
  - How to create a skeleton UI5 project
  - How to add Luigi and other dependencies to your project

---

### Create React app


In this step, you will create a React skeleton project which will be used to create your Luigi app.

1. Open a Terminal or Command Prompt window and navigate to the space where you want to install the app. Then create a new folder:

    ```Shell
    mkdir luigi-react-ui5
    cd luigi-react-ui5
    ```

2. Copy and paste this command into the Terminal/Command Prompt to create a React app. This may take a few minutes:

    ```Shell
    npx create-react-app react-core-mf && cd react-core-mf
    ```

3.  Eject the project to customize the `webpack` configuration. If you get an error, you need to commit any changes before running the command. More information can be found [here](https://stackoverflow.com/questions/45671057/how-to-run-eject-in-my-react-app).:

    ```Shell
    echo yes | npm run eject
    ```
    ​
4. Install dependencies:

    ```Shell
    npm i -P @luigi-project/core @luigi-project/client fundamental-styles@0.11.0 @sap-theming/theming-base-content react-router-dom @ui5/webcomponents @ui5/webcomponents-react
    npm i copy-webpack-plugin@5 webpack webpack-cli @babel/core @babel/preset-env babel-loader --save-dev
    ```
    ​
5. Replace strings (these commands have been adapted from the standard [Luigi React example](https://github.com/SAP/luigi/blob/master/scripts/setup/react.sh).) Note that you may get a warning such as `event not found`, but this can safely be ignored. Copy and paste the following:

    ```Shell
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

    #This can throw a warning, it can be ignored
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
    ```

6. Create a new folder:

    ```Shell
    mkdir -p src/luigi-config
    mkdir src/views
    ```
    ​
7. Download the Luigi React configuration:

    ```Shell
     curl https://raw.githubusercontent.com/SAP/luigi/master/core/examples/luigi-example-react/public/luigi-config.js > src/luigi-config/luigi-config.es6.js
    ```

8.  Ins​tall and run the configuration:

    ```Shell
    npm i
    npm run buildConfig
    ```
    ​
9.  Move back into the root directory:

    ```Shell
    cd ..
    ```


### Create UI5 micro-frontend


In this step, you will create a skeleton UI5 project for your UI5 micro-frontend.

1. If you didn't already, navigate to the root folder of your project in the terminal.

2. Create a new folder:

    ```Shell
    mkdir ui5-mf && cd ui5-mf
    ```

3. Install the UI5 project generator:

    ```Shell
    npm install -g yo generator-easy-ui5
    ```

4. Enter this command in the Terminal/Command Prompt:

    ```Shell
    yo easy-ui5
    ```

5. Choose `ts-app` and use the settings shown in the screenshot below. For the questions "Which framework do you want to use?" and "Who is the author of this application?", choose the default option and press Enter. 

    ![UI5 Terminal](ui5-yo.png)







---
