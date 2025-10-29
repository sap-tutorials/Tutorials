---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product>sap-fiori]
primary_tag: programming-tool>html5
author_name: Lukas Harbarth
author_profile: https://github.com/Lukas742
---

# Add Custom Styles and Components for UI5 Web Components for React
<!-- description --> Add custom styles and custom components to your application using UI5 Web Components for React.

## You will learn
-  How to change style of existing components
-  How to style own components


## Intro
In this tutorial, you will learn how to apply styling to the UI5 Web Components. You can add custom styles and even overwrite CSS Variables on any level. Also, you'll learn how to style your custom components with the styling approach of SAP Fiori.


---

### Change style for existing components

You can change the appearance of the UI5 Web Components by using [CSS Variables](https://www.w3schools.com/Css/css3_variables.asp).
Per default, the Horizon theme parameters are added to the `adoptedStyleSheets` of the `document`. 
For example, if you want to change the color of all texts that use the `--sapTile_TitleTextColor` variable, you can create an additional `style` rule overriding the value of the CSS variable.

Open the `index.css` file inside your `src` folder and add the following content:

```CSS
* {
  --sapTile_TitleTextColor: limegreen;
}
```

The `sapTile_TitleTextColor` CSS Variable is used in the `CardHeader` component to set the color of the title. With the `*` selector, you can apply styles to all elements in the subtree and therefore change the color of all components using `sapTile_TitleTextColor`.

![Custom Style](02_customStyle.png)

As a consequence, all HTML Elements in the subtree where this style was applied are now displaying their texts in `limegreen` instead of `rgb(29, 45, 62)` which would be the default value for Horizon. You can change CSS Variables on any level - in the head, or on every single element by using either CSS classes or element style.

A full list of all supported CSS Variables can be found in the [`ThemingParameters`](https://ui5.github.io/webcomponents-react/?path=/docs/knowledge-base-public-utils--docs#theming-parameters) object or in the [theming-base-content](https://github.com/SAP/theming-base-content) repository.

> **Warning:** Overriding the value of CSS variables changes it for all themes and therefore can cause inconsistencies! 


### Style your own component


If you want to add a custom component to your app, but still want to use the styling approach of the UI5 Web Components, you can use the global CSS vars ([`ThemingParameters`](https://ui5.github.io/webcomponents-react/?path=/docs/knowledge-base-public-utils--docs#theming-parameters)). If you want to style your components with a CSS-in-JS library like [`react-jss`](https://cssinjs.org/react-jss), or want to use the React inline styling, you can use the `ThemingParameters` directly.

In this step we will use [CSS Modules](https://github.com/css-modules/css-modules) and inline styling to style a custom component.

> **Note:** The Vite template already includes support for CSS Modules, so you can use them out of the box.

1. Create a custom component `MyCustomElement.tsx` under `./src` with following content:

    ```TypeScript / TSX
    import { ThemingParameters } from "@ui5/webcomponents-react-base";

    export const MyCustomElement = () => {
      return (
        <div >
          <span>My custom Text Element</span>
        </div>
      );
    };
    ```
   
2. Explicitly import `@ui5/webcomponents-react-base` package

    `@ui5/webcomponents-react-base` is already installed by `@ui5/webcomponents-react`, but as we want to import the `ThemingParameters` in the following step, it's recommended adding the package to the dependencies as well.

    ```shell
    npm install @ui5/webcomponents-react-base
    ```

3. Add inline-styles to apply `ThemingParameters` to the `<span>`

    ```TypeScript / TSX
    <span style={{ color: ThemingParameters.sapNegativeColor, fontSize: ThemingParameters.sapFontHeader1Size }}>
      My custom Text Element
    </span>
    ```

    The global CSS variables contain all publicly available styling parameters. With this it is possible to style custom components with the standardized styles of the UI5 Web Components.

4. Add styling using CSS Modules

    First create a new file `MyCustomElement.module.css` in the same folder as `MyCustomElement.tsx` with the following content:

    ```CSS
     .container {
       background-color: var(--sapBackgroundColor);
       font-family: var(--sapFontFamily);
       height: 50px;
       display: flex;
       justify-content: center;
       align-items: center;
     }
    ```
    Then import the CSS file in your `MyCustomElement.tsx` file and add the class to the `div` element.

    ```TypeScript / TSX
      import { ThemingParameters } from '@ui5/webcomponents-react-base';
      import classes from './MyCustomElement.module.css';
      
      export const MyCustomElement = () => {
        return (
          <div className={classes.container}>
            <span
              style={{
                color: ThemingParameters.sapNegativeColor,
                fontSize: ThemingParameters.sapFontHeader1Size,
              }}
            >
              My custom Text Element
            </span>
          </div>
        );
      };
    ```

5. Import the custom component and add it to your `Home` component.

    ```TypeScript / TSX
    import { MyCustomElement } from "./MyCustomElement";
    ```

    ```TypeScript / TSX
     return (
        <FlexBox
          justifyContent={FlexBoxJustifyContent.Center}
          wrap={FlexBoxWrap.Wrap}
        >
          <MyCustomElement />
       ...
    ```
    ![Custom Element](01_customElement.png)

   Now you can see, that the element has the same `fontFamily` and uses the same semantic colors as UI5 Web Components for React.

### SAP UI Common CSS

It's often required setting more complex styles for e.g. layouting. For this it's recommended using CSS classes of the [SAP UI Common CSS](https://sap.github.io/fundamental-styles/?path=/docs/common-css-introduction--docs) library which is also following the Fiori design guidelines.

One example is applying a responsive content padding to the content of the dashboard:

1. Install Common CSS

    ```shell
    npm i @sap-ui/common-css
    ```

2. Import the required classes in `Home.tsx`

    ```ts
    import '@sap-ui/common-css/dist/sap-content-paddings.css';
    import '@sap-ui/common-css/dist/sap-container-type.css';
    ```
3. Apply the classes on elements

    Add `div` that is wrapping the outer `FlexBox` and apply the `sap-container-type-inline-size` class:

    ```tsx
    <div className="sap-container-type-inline-size">
       <FlexBox
         justifyContent={FlexBoxJustifyContent.Center}
         wrap={FlexBoxWrap.Wrap}
         ...
    ```
    
    Remove `style` from the outer `FlexBox` and apply the `sap-content-paddings-container` class:

    ```tsx
    <FlexBox
      justifyContent={FlexBoxJustifyContent.Center}
      wrap={FlexBoxWrap.Wrap}
      className="sap-content-paddings-container"
    >
    ```   
    Now, the padding of the `FlexBox` is adjusted automatically according to the viewport size.

### Conclusion

In this tutorial mission, you learned the basics of how to build a single page application using UI5 Web Components for React, with routing, styling, and general component behavior and modern React APIs.
If you have questions about UI5 Web Components for React, or found something that isn't right, then please feel free to join the [OpenUI5 Slack community](https://ui5-slack-invite.cfapps.eu10.hana.ondemand.com/) and visit us in the [`#webcomponents-react`](https://openui5.slack.com/archives/CSQEJ2J04) channel, or come directly to our [GitHub page](https://github.com/UI5/webcomponents-react), where you can file [issues](https://github.com/UI5/webcomponents-react/issues/new/choose) or participate in [discussions](https://github.com/UI5/webcomponents-react/discussions).

If you enjoyed this tutorial, you can help us out a lot by starring our repo and become one of our wonderful [✨stargazers✨](https://github.com/UI5/webcomponents-react/stargazers)!



---