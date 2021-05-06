---
title: Add Custom Styles and Components for UI5 Web Components for React
description: Add custom styles and custom components to your application using UI5 Web Components for React.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-fiori]
primary_tag: topic>html5
author_name: Lukas Harbarth
author_profile: https://github.com/Lukas742
---

## Details
### You will learn
-  How to change style of existing components
-  How to style own components


In this tutorial, you will learn how to apply styling to the UI5 Web Components. You can add custom styles and even overwrite CSS Variables on any level. Also you'll learn how to style your custom components with the styling approach of SAP Fiori


---

[ACCORDION-BEGIN [Step 1:](Change style for existing components)]
You can change the appearance of the Web Components by using [CSS Variables](https://www.w3schools.com/Css/css3_variables.asp). Per default, the Fiori 3 theme parameters are injected into the document head as CSS Variables. If you want to change e.g. the color of all texts, you can do that by creating another style element.

Open the `index.html` file inside of your `public` folder and add the following content inside of the `<head>` element:

```HTML
  <style>
    * {
      --sapTile_TitleTextColor: limegreen;
    }
  </style>
```

The `sapTile_TitleTextColor` CSS Variable changes the style of the `Card` titles and the `*` selector appends the style to all elements.

![Custom Style](02_customStyle.png)

As a consequence, all HTML Elements in the subtree where this style was applied are now displaying their texts in `limegreen` instead of `#6a6d70` which would be the default value for Fiori 3. You can change CSS Variables on any level - in the head, or on every single element by using either CSS classes or element style.

A full list of all supported CSS Variables can be found in the [`ThemingParameters`](https://github.com/SAP/ui5-webcomponents-react/blob/master/packages/base/src/styling/ThemingParameters.ts).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2:](Style your own component)]

If you want to add a custom component to your app, but still want to use the styling approach of the UI5 Web Components, you can import the `ThemingParameters` that contain the various CSS variables used in our theming. If you want to style your components with the [`react-jss`](https://cssinjs.org/react-jss/?v=v10.1.1) syntax, you can use the custom `jss` styling hook `createUseStyles`.

1. Create a custom component `MyCustomElement.jsx` under `./src` with following content:

    ```JavaScript / JSX
    import React from "react";
    import { createUseStyles } from 'react-jss';
    import { ThemingParameters } from "@ui5/webcomponents-react-base/dist/ThemingParameters";

    export const MyCustomElement = () => {
      return (
        <div >
          <span>My custom Text Element</span>
        </div>
      );
    };
    ```
2. Add inline-styles with the `ThemingParameters` to the `<span>`

    ```JavaScript / JSX
    <span style={{ color: ThemingParameters.sapNegativeColor, fontSize: ThemingParameters.sapFontHeader1Size }}>
      My custom Text Element
    </span>
    ```

    The `ThemingParameters` contain all available styling parameters. With this it is possible to style custom components with the standardized styles of the UI5 Web Components.

3. Add styling with `react-jss` and the `createUseStyles` hook

    ```JavaScript / JSX
    import React from "react";
    import { createUseStyles } from 'react-jss';
    import { ThemingParameters } from "@ui5/webcomponents-react-base/dist/ThemingParameters";

    const styles = {
      container: {
        backgroundColor: ThemingParameters.sapBackgroundColor,
        fontFamily: ThemingParameters.sapFontFamily,
        height: "50px",
        display: "flex",
        justifyContent: "center",
        alignItems: "center",
      }
    };

    const useStyles = createUseStyles(styles);

    export const MyCustomElement = () => {
      const classes = useStyles();

      return (
        <div className={classes.container}>
          <span style={{ color: ThemingParameters.sapNegativeColor, fontSize: ThemingParameters.sapFontHeader1Size }}>
            My custom Text Element
          </span>
        </div>
      );
    };
    ```

4. Import the custom component and add it to your `Home` component.

    ```JavaScript / JSX
    import { MyCustomElement } from "./MyCustomElement";
    ```

    ```JavaScript / JSX
     return (
        <FlexBox
          justifyContent={FlexBoxJustifyContent.Center}
          wrap={FlexBoxWrap.Wrap}
        >
          <MyCustomElement />
       ...
    ```
    ![Custom Element](01_customElement.png)

   Now you can see, that the element has the same `fontFamily` and uses the same semantic colors as the UI5 Web Components for React.

[VALIDATE_1]
[ACCORDION-END]

---
