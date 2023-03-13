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

2. Create a new folder to host our react core app into:

    ```Shell
    mkdir react-core-mf
    cd react-core-mf
    ```

 ​4. Install dependencies: First download the react example package json file containing minimal dependencies required for a React app.
    Next we need to install styling libraries to handle css files as well as fundamentals and react web components packages.

    ```Shell
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/package.json > package.json
    npm install fundamental-styles @ui5/webcomponents-react --save
    npm install style-loader css-loader --save-dev
    ```
    ​
5. We need a simple webpack configuration to bundle our simple react application. For this we will use a readily available minimalistic webpack configuration from an existing [luigi example app](https://github.com/SAP/luigi/blob/main/core/examples/luigi-example-react/webpack.config.js). Download the webpack config using the line below:

    ```Shell
     curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/webpack.config.js > webpack.config.js
    ```

6. Next we need to create the project structure for our simple React app. Create the following folders:

    ```Shell
    mkdir src/views
    mkdir src/assets
    ```
    ​
7. Download the Luigi React simple app files:

    ```Shell
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/src/index.js > src/index.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/src/views/home.js > src/views/home.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/src/views/sample1.js > src/views/sample1.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/src/views/sample1.js > src/views/sample2.js
    curl https://raw.githubusercontent.com/SAP/luigi/main/core/examples/luigi-example-react/public/index.html > public/index.html
    curl https://github.com/SAP/luigi/blob/main/core/examples/luigi-example-react/public/sampleapp.html > public/sampleapp.html
    ```

8.  Ins​tall and run the configuration:

    ```Shell
    npm i
    npm run start
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

5. Choose `app` and use the settings shown in the screenshot below. For the questions "Which framework do you want to use?" and "Who is the author of this application?", choose the default option and press Enter. 

    ![UI5 Terminal](ui5-yo.png)







---
