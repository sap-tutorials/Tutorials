---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-fiori]
primary_tag: programming-tool>html5
author_name: Lukas Harbarth
author_profile: https://github.com/Lukas742
---

# Create a Card Component
<!-- description --> Define your first component using UI5 Web Components for React.

## You will learn
-  How to import components into your application
-  Learn about the usage of props
-  How to use event handling


## Intro
Integrate the first component into your App. You can find **all** available components in the [Storybook](https://sap.github.io/ui5-webcomponents-react).
There you can try out the different components and also take a look at the coding and the available props.

---

### Add components to MyApp.jsx

First you need to import the components you want to use.

You can check out all available components in the [Storybook](https://sap.github.io/ui5-webcomponents-react). Take your time to play around a little, change some `props` and take a look at the coding.

1. Start with importing a `Card` component into your `MyApp.jsx` file (right below the existing import statement).

    ```JavaScript / JSX
    import { Card } from "@ui5/webcomponents-react";
    ```

2. So, you imported the `Card` component. Now it's time to use it. Replace the content of your `<div>` with a `<Card>`.

    In the [Storybook](https://sap.github.io/ui5-webcomponents-react/?path=/story/4-ui5-web-components-card--default-story), you can see that Cards can receive different props. For now only add some text as `children`.

    ```JavaScript / JSX
    <div>
      <Card>This is the content area of the Card</Card>
    </div>
    ```

Your webpage should now look like this.

![Card01](01_card.png)

And the file like this:

```JavaScript / JSX
import { Card } from "@ui5/webcomponents-react";

export function MyApp() {
  return (
    <div>
      <Card>This is the content area of the Card</Card>
    </div>
  );
}
```



### Add content to the Card component

The heading area of the `Card` component is empty, this is because it didn't receive the `header` prop. The `header` prop expects another component, the `CardHeader`.

1. Import the `CardHeader`.

    ```JavaScript / JSX
    import { Card, CardHeader } from "@ui5/webcomponents-react";
    ```

2. Add the `CardHeader` component to your `Card` and give it a title by setting the `titleText` prop:

    ```JavaScript / JSX
    <div>
      <Card header={<CardHeader titleText="Card" />}>
        This is the content area of the Card
      </Card>
    </div>
    ```

2. Now the `Card` has a header area, but the `font-family` of the content area differs from the `Card` header. All UI5 Web Components for React components use the same styling, this includes `font-family`, `color`, etc.  

     Add the `Text` import to your `MyApp.jsx` file.

    ```JavaScript / JSX
    import { Card, CardHeader, Text } from "@ui5/webcomponents-react";
    ```

    And wrap the text within the `Text` component.

    ```JavaScript / JSX
    <div>
      <Card header={<CardHeader titleText="Card" />}>
        <Text>This is the content area of the Card</Text>
      </Card>
    </div>
    ```

The `font-family` of the content now corresponds to the `font-family` of the header.



### Style your component

In this step, we will only apply [inline-styling](https://reactjs.org/docs/dom-elements.html#style). You can also style your component using normal CSS or even authoring tools like [JSS](https://cssinjs.org/?v=v10.0.0), but this will be covered in [Tutorial 6](ui5-webcomponents-react-styling) of the tutorial series.

The Card now spreads across the whole screen, this behavior is intended so it takes up the whole space of its container.

1. To restrict the `width` of the `Card`, add the `style` prop.

    ```JavaScript / JSX
    <Card header={<CardHeader titleText="Card" />} style={{ width: "300px" }}>
      <Text>This is the content area of the Card</Text>
    </Card>
    ```

    ![Card02](02_card.png)

2. The content of the card is way too close to the border of the `Card`, so a `padding` is needed. UI5 Web Components comes with a `Util` library, which includes `padding` sizes.

    Execute this in your terminal:

    ```Shell
    npm install @ui5/webcomponents-react-base --save
    ```

    Then import:

    ```JavaScript / JSX
    import { spacing } from "@ui5/webcomponents-react-base";
    ```

    And finally add this to your `Text` component:

    ```JavaScript / JSX
    <Text style={spacing.sapUiContentPadding}>
      This is the content area of the Card
    </Text>
    ```
    Hereby you get a standardized content-padding. `spacing` comes with many more properties, feel free to test them and see what they do.

After this step `MyApp.jsx` should look like this:
```JavaScript / JSX
import React from "react";
import { Card, Text, CardHeader } from "@ui5/webcomponents-react";
import { spacing } from "@ui5/webcomponents-react-base";

export function MyApp() {
  return (
    <div>
      <Card header={<CardHeader titleText="Card" />} style={{ width: "300px" }}>
        <Text style={spacing.sapUiContentPadding}>
          This is the content area of the Card
        </Text>
      </Card>
    </div>
  );
}
```
And your application like this:

![Card03](03_card.png)


### Event handling


1. The Card header can also be clickable. For this you need to set its `interactive` prop to true.

    ```JavaScript / JSX
    <Card
      header={<CardHeader titleText="Card" interactive />}
      ...
    </Card>
    ```

    We didn't pass a value to `interactive`, because it [defaults to true](https://reactjs.org/docs/jsx-in-depth.html#props-default-to-true) if the value is omitted.

2.  To make the header react to a click, add a function as value to the `onClick` prop.

    ```JavaScript / JSX
    <Card
      header={
        <CardHeader
          titleText="Card"
          interactive
          onClick={handleHeaderClick}
        />
      }
      ...
    </Card>
    ```

3. Now, add the callback function right in the beginning of the component (definition function):
    ```JavaScript / JSX
    export function MyApp() {
      const handleHeaderClick = () => {
        alert("Header clicked");
    };
    ...
    ```

The file now looks like this:
```JavaScript / JSX
import React from "react";
import { Card, Text, CardHeader } from "@ui5/webcomponents-react";
import { spacing } from "@ui5/webcomponents-react-base";

export function MyApp() {
  const handleHeaderClick = () => {
    alert("Header clicked");
  };
  return (
    <div>
      <Card
        header={
          <CardHeader
            titleText="Card"
            interactive
            onClick={handleHeaderClick}
          />
        }
        style={{ width: "300px" }}
      >
        <Text style={spacing.sapUiContentPadding}>
          This is the content area of the Card
        </Text>
      </Card>
    </div>
  );
}
```
Now the header opens an alert box on click.



---
