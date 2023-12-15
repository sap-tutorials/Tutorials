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

2. Create a new folder to host the React core app:

    ```Shell
    mkdir react-core-mf
    cd react-core-mf
    ```

 3. Download the React example package JSON file containing minimal dependencies required for a React app. Next, install styling libraries to handle CSS files, as well as Fundamentals and React web components packages.

    ```Shell
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/package.json > package.json
    npm install fundamental-styles @ui5/webcomponents-react --save
    npm install style-loader css-loader --save-dev
    ```
    ​
4. The React application will be bundled using Webpack. Download the Luigi Webpack configuration from an existing [Luigi example app](https://github.com/SAP/luigi/blob/main/core/examples/luigi-example-react/webpack.config.js) using the line below:

    ```Shell
     curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/webpack.config.js > webpack.config.js
    ```

5. Next, create the project structure for the React app. Create the following folders:

    ```Shell
    mkdir -p src/views
    mkdir -p src/assets
    mkdir -p public
    ```
    ​
6. Download the Luigi React app files:

    ```Shell
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/public/luigi-config.js > public/luigi-config.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/src/views/home.js > src/views/home.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/src/views/sample1.js > src/views/sample1.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/src/views/sample1.js > src/views/sample2.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/public/index.html > public/index.html
    curl https://github.com/SAP/luigi/blob/main/core/examples/luigi-example-react/public/sampleapp.html > public/sampleapp.html
    ```

7.  Ins​tall and run the configuration:

    ```Shell
    npm i
    npm run start
    ```
    ​
8.  Move back into the root directory:

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

5. Use the settings shown in the screenshot below. For "Select your generator", use the arrow keys to scroll down and select `app`. For the questions "Which framework do you want to use?" and "Who is the author of this application?", choose the default option and press Enter. 

    <!-- border -->![UI5 Terminal](ui5-yo.png)







---
