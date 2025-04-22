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
- [React](https://www.npmjs.com/package/react) and [React-DOM](https://www.npmjs.com/package/react-dom) ( **18.0.0 or higher** )
- [Node.js](https://nodejs.org/) - **[LTS](https://github.com/nodejs/release?tab=readme-ov-file#release-schedule) version** (check the version with `node -v`)


## You will learn
-  How to create a new React application
-  How to connect the application with UI5 Web Components for React


## Intro
React is a great front-end development tool for building single-page applications (SPA). [UI5 Web Components for React](https://github.com/SAP/ui5-webcomponents-react) provides a SAP Fiori-compliant React implementation by leveraging the [UI5 web components](https://github.com/SAP/ui5-webcomponents).

SAP Fiori provides a consistent and holistic user experience for SAP software. By creating visually pleasing designs with a strong focus on ease of use, the experience is intuitive and simple, across all devices.

This first tutorial will start by creating a React application that is able to consume UI5 Web Components for React.

---

### Bootstrap the app with the Vite template

UI5 Web Components for React provides various [templates](https://sap.github.io/ui5-webcomponents-react/?path=/docs/project-templates-examples--docs) for starting your project. In this tutorial, we'll use the Vite template.

Navigate to a folder where you want to create your Web App and open a terminal there. Then use the following command:

```Shell
npx degit SAP/ui5-webcomponents-react/templates/vite-ts#main my-app
cd my-app
npm i
```

These commands sets up a [Vite](https://vitejs.dev/) project, creating a React Application with [TypeScript](https://www.typescriptlang.org/) and incorporating all essential dependencies for UI5 Web Components for React.

> ### Why TypeScript?
> 
> TypeScript, a superset of JavaScript and brings a lot of advantages for developers:
> 
> - **Easy Debugging:** Helps catch errors early.
> - **Code Guidance:** Provides hints for better coding.
> - **Tool Support:** Offers autocompletion and navigation in editors.
> - **Readability:** Enhances code clarity with static typing.
> - **Gradual Adoption:** Can be added to existing JavaScript projects.



### Create your new root component


1. Open the current directory with an editor of your choice (e.g. Visual Studio Code).

2. Inside your project folder, navigate to `src`. There, create a new file and name it `MyApp.tsx`.

3. Now, add the following lines of code to `MyApp.tsx`.

    ```TypeScript / TSX
    export function MyApp() {
      return <div>My root component</div>;
    }
    ```

> ### Structure of a React component
This is a very simple component, but it already shows you the basic structure of all components. The file starts with the import statements in the first few lines. Then, the component will be defined as a function ([React docs](https://react.dev/learn/your-first-component#defining-a-component)) in PascalCase notation. This function starts the definitions of the props and the logic, we'll add them in a later tutorial, and ends by returning JSX or HTML components in a return statement.

With this you created your first `React` component. To actually render the component you will have to add it to your `src/App.tsx`.

### Embed your new component


1. In `App.tsx` remove everything except for the [React.Fragment](https://react.dev/reference/react/Fragment) (`<></>`) and the App component itself.

    ```TypeScript / TSX
    function App() {
      return (<></>);
    }

    export default App;
    ```

2. Import your created component.

    ```TypeScript / TSX
    import { MyApp } from "./MyApp";
    ```
3. Add the component to the `return` value of `App()`.

    ```TypeScript / TSX
    function App() {
      return (
        <>
          <MyApp />
        </>
      );
    }
    ```

    > Note that `<MyApp />` is using a self-closing syntax and is equivalent to `<MyApp></MyApp>`. All tags in JSX must be closed explicitly, this applies to HTML tags (like `img`) and custom JSX tags. [Here](https://react.dev/learn/writing-markup-with-jsx) you can find out more about JSX in general.

4. Open `main.tsx`. This is your entry file, let's take a look at the content in more detail.

    Your file should look like this:

    ```TypeScript / TSX
    import '@ui5/webcomponents-react/dist/Assets.js';
    import { ThemeProvider } from '@ui5/webcomponents-react';
    import { StrictMode } from 'react';
    import { createRoot } from 'react-dom/client';
    import App from './App.tsx';
    import './index.css';
    
    createRoot(document.getElementById('root') as HTMLElement).render(
      <StrictMode>
        <ThemeProvider>
          <App />
        </ThemeProvider>
      </StrictMode>
    );
    ```
    
    - `Assets.js'`: This includes assets like translation files (CLDR), theming, etc. of the required packages.
    - `ThemeProvider`: Inter alia, this provider makes your app react to theme and language changes and injects the CSS of __used__ components.
    - `StrictMode`: The React `StrictMode` component enables addition development behaviors and warnings for the component tree inside. It is not required, but using it helps find common pitfalls and bugs in development. You can find out more about it [here](https://react.dev/reference/react/StrictMode).
    - `createRoot`: This function lets you create the React root to display React components inside a browser DOM node ([React docs](https://react.dev/reference/react-dom/client/createRoot)). You will usually add this DOM node inside the `index.html` file.
    - `App`: A React component.
    - `index.css`: Global CSS file.


### Launch the app to start developing

Now you can start the app in development mode. Execute the following command from the root directory of the project.

```Shell
npm run dev
```

Open <http://localhost:5173> to view it in the browser.

![Root Component](01_rootComponent.png)

The page will automatically reload if you make changes to the code. You will see the build errors and lint warnings in the console.

> **TIP:** You can put a browser window next to the editor in your screen to see the changes live in action.

><!-- border -->![split](splitscreen.png)
>
>&nbsp;



---
