---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-fiori]
primary_tag: programming-tool>html5
author_name: Lukas Harbarth
author_profile: https://github.com/Lukas742
---


# Get Started with UI5 Web Components for React
<!-- description --> Start building a React web application leveraging UI5 Web Components for React.

## Prerequisites
- [React](https://www.npmjs.com/package/react) and [React-DOM](https://www.npmjs.com/package/react-dom) ( **16.14.0 or higher** )
- [Node.js](https://nodejs.org/), **version 14 or later** (check the version with `node -v`)


## You will learn
-  How to create a new React application
-  How to connect the application with UI5 Web Components for React


## Intro
React is a great front-end development tool for building single-page applications (SPA). UI5 Web Components for React provides a SAP Fiori-compliant React implementation by leveraging the [UI5 web components](https://github.com/SAP/ui5-webcomponents).

SAP Fiori provides a consistent and holistic user experience for SAP software. By creating visually pleasing designs with a strong focus on ease of use, the experience is intuitive and simple, across all devices.

This first tutorial will start by creating a React application that is able to consume UI5 Web Components for React.

---

### Bootstrap the app with create-react-app


Navigate to a folder where you want to create your Web App and open a terminal there. Then use the following command:

```Shell
npx create-react-app my-app --template @ui5/cra-template-webcomponents-react
cd my-app
```

 This command leverages [create-react-app](https://facebook.github.io/create-react-app/) to create a React Application with all necessary dependencies for UI5 Web Components for React.


### Create your new root component


1. Open the current directory with an editor of your choice (e.g. Visual Studio Code).

2. Inside of your project folder, navigate to `src`. There, create a new file and name it `MyApp.jsx`.

3. Now, add the following lines of code to `MyApp.jsx`.

    ```JavaScript  / JSX
    import React from "react";

    export function MyApp() {
      return <div>My root component</div>;
    }
    ```

> ### Structure of a React component
This is a very simple component, but it already shows you the basic structure of all components. The file starts with the import statements in the first few lines. Then, the component will be defined as a function (or as a [class](https://reactjs.org/docs/react-component.html)). This function starts the definitions of the props and the logic, we'll add them in a later tutorial, and ends by returning JSX or HTML components in a return statement.

With this you created your first `React` component. To actually render the component you will have to add it to your `src/App.js`.

### Embed your new component


1. In `App.js` remove everything inside of the `<ThemeProvider>` (or [React.Fragment](https://reactjs.org/docs/fragments.html) `<></>`). You can also delete the `className` property and the `import` of the logo and `"./App.css"` as they are not needed anymore for this tutorial.

> If the `ThemeProvider` is not present in this file, please look in the `index.js` file to see if the component is there. If it is, you do not need to use it here as well, otherwise please continue as described.

2. Import your created component.

    ```JavaScript  / JSX
    import { MyApp } from "./MyApp";
    ```
3. Add the component to the `return` value of `App()`.

    ```JavaScript  / JSX
    function App() {
      return (
        <ThemeProvider>
          <MyApp />
        </ThemeProvider>
      );
    }
    ```

Your `App.js` file should now look like this:

```JavaScript  / JSX
import React from "react";
import { MyApp } from "./MyApp";
import { ThemeProvider } from "@ui5/webcomponents-react";

function App() {
  return (
    <ThemeProvider>
      <MyApp />
    </ThemeProvider>
  );
}

export default App;
```


### Launch the app to start developing

Now you can start the app in development mode. Execute the following command from the root directory of the project.

```Shell
npm start
```

Open <http://localhost:3000> to view it in the browser.

![Root Component](01_rootComponent.png)

The page will automatically reload if you make changes to the code. You will see the build errors and lint warnings in the console.

> **TIP:** You can put a browser window next to the editor in your screen to see the changes live in action.

><!-- border -->![split](./splitscreen.png)
>
>&nbsp;



---
