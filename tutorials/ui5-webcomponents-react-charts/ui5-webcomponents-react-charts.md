---
title: Integrate Charts and Conditional Rendering
description: Conditionally rendering of charts to present data.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-fiori]
primary_tag: topic>html5
---

## Details
### You will learn
-  How to install and import charts
-  Learn about charts in UI5 web components
-  How to add dynamic rendering

UI5 Web Components for React also come with a chart library. In this tutorial you will integrate two chart types and add data to them. Also you will learn how to conditionally render components, and how React handles updates to the DOM and single components.

---

[ACCORDION-BEGIN [Step 1: ](Installing the module and importing the charts)]
1. Install the chart library of UI5 Web Components for React.

    ```sh
     npm install @ui5/webcomponents-react-charts --save
    ```


2. Then, import `LineChart` and `BarChart` into `MyApp.jsx`.

    ```JSX
    import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add Charts to Card component)]
1. Start with the `LineChart` and add it underneath the `Text` component.

    ```JSX
    <Text style={spacing.sapUiContentPadding}>
      This is the content area of the Card
    </Text>
    <LineChart />
    ```

    Well, that didn't change much, didn't it? It's because the chart didn't receive any data, and therefore the content is empty.

2. Add data and corresponding labels to your component (right above the `return` statement).

    ```JSX
    const datasets = [{
        label: "Stock Price",
        data: [65, 59, 80, 81, 56, 55, 40]
    }];
    const labels = [
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July"
    ];
    ```

3. Now add `datasets` and `labels` to your `LineChart` components.

    ```JSX
    <LineChart datasets={datasets} labels={labels} />
    ```

    Congratulation, you implemented your first Chart component.

    ![LineChart](01_linechart.png)

4. Add a `BarChart` to the `Card`.

    We want the same data just with a different representation, therefore you can use the same labels and datasets as you did with the `LineChart`.

    ```JSX
    <BarChart datasets={datasets} labels={labels} />
    ```

    Two charts are rendered now with equal datasets but different representation.

Your `MyApp.jsx` component should look like this:

```JSX
import React from "react";
import { Card, Text } from "@ui5/webcomponents-react";
import { spacing } from "@ui5/webcomponents-react-base";
import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";

export function MyApp() {
    const handleHeaderClick = () => {
        alert("Header clicked");
    };
    const datasets = [{
        label: "Stock Price",
        data: [65, 59, 80, 81, 56, 55, 40]
    }];
    const labels = [
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July"
    ];
    return (
        <div>
            <Card
                heading="Card"
                style={{ width: "300px" }}
                headerInteractive
                onHeaderClick={handleHeaderClick} >
                <Text style={spacing.sapUiContentPadding}>
                    This is the content area of the Card
                </Text>
                <LineChart datasets={datasets} labels={labels} />
                <BarChart datasets={datasets} labels={labels} />
            </Card>
        </div>
    );
}
```  

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Conditional rendering)]
Two charts in one `Card` is a bit too much, don't you think? It would be nicer if the charts could be toggled by clicking on the header. Let's implement that!

1. First add a state. It should control which chart is going to be rendered.
Use the [State Hook logic](https://reactjs.org/docs/hooks-state.html) to implement the state and set `"lineChart"` as default value. Don't forget to import `useState` from React, otherwise you will get an error.
    - Import the `useState` function in the header of the `MyApp.jsx` file (replace the current import of React).
    ```JSX
      import React, { useState } from "react";
    ```
    - Use the `useState` function in the right after you start to define the `MyApp` function (before the click handler).
    ```JSX
      const [toggleCharts, setToggleCharts] = useState("lineChart");
    ```

2. By clicking on the `Card` header the state should be set corresponding to the chart which should be displayed.

    Rewrite your `handleHeaderClick` function so it will handle this logic.
    ```JSX
    const handleHeaderClick = () => {
      if (toggleCharts === "lineChart") {
        setToggleCharts("barChart");
      } else {
        setToggleCharts("lineChart");
      }
    };
    ```
3. To only render the current chart, add the following lines to the render of the component:
    ```JSX
    <Card
        heading="Card"
        style={{ width: "300px" }}
        headerInteractive
        onHeaderClick={handleHeaderClick}>
        <Text style={spacing.sapUiContentPadding}>
            This is the content area of the Card
        </Text>
        {toggleCharts === "lineChart" ? (
            <LineChart datasets={datasets} labels={labels} />
        ) : (
             <BarChart datasets={datasets} labels={labels} />
         )}
    </Card>
    ```

Done! Now you can toggle between charts by clicking on the header of the `Card`.

If something went wrong you can compare your component to this code snippet:
```JSX
import React, { useState } from "react";
import { Card, Text } from "@ui5/webcomponents-react";
import { spacing } from "@ui5/webcomponents-react-base";
import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";

export function MyApp() {
    const [toggleCharts, setToggleCharts] = useState("lineChart");
    const handleHeaderClick = () => {
        if (toggleCharts === "lineChart") {
            setToggleCharts("barChart");
        } else {
            setToggleCharts("lineChart");
        }
    };
    const datasets = [{
        label: "Stock Price",
        data: [65, 59, 80, 81, 56, 55, 40]
    }];
    const labels = [
        "January",
        "February",
        "March",
        "April",
        "May",
        "June",
        "July"
    ];
    return (
        <div>
            <Card
                heading="Card"
                style={{ width: "300px" }}
                headerInteractive
                onHeaderClick={handleHeaderClick}>
                <Text style={spacing.sapUiContentPadding}>
                    This is the content area of the Card
                </Text>
                {toggleCharts === "lineChart" ? (
                    <LineChart datasets={datasets} labels={labels} />
                ) : (
                     <BarChart datasets={datasets} labels={labels} />
                 )}
            </Card>
        </div>
    );
}
```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Updating a component in React - Loading Indicator)]
One of the main advantages of React are how UI updates are handled. React will only re-render the component if the state of the component has been changed. So it will not update the whole UI, but only the component that is effected by changes.

1. In order to demonstrate this behavior add a new `state` (right after the definition of the previous state).
    ```JSX
      const [loading, setLoading] = useState(false);
    ```

2. Then edit your `handleHeaderClick` function like this:
    ```JSX
    const handleHeaderClick = () => {
        if (toggleCharts === "lineChart") {
          setLoading(true);
          setTimeout(() => {
            setLoading(false);
            setToggleCharts("barChart");
          }, 2000);
        } else {
          setLoading(true);
          setTimeout(() => {
            setLoading(false);
            setToggleCharts("lineChart");
          }, 2000);
        }
    };
    ```

3. And add `loading` to both of your charts.
    ```JSX
     <LineChart datasets={datasets} labels={labels} loading={loading} />
    ```
    ```JSX
     <BarChart datasets={datasets} labels={labels} loading={loading} />
    ```

This updates the component every time you switch between charts and simulates a data call.

As you can see only the component effected by the `state` is updated, the rest stays the same. If you're working with data you most probably will need a loading indicator. All UI5 Web Components which are able to display data have a `loading` prop and therefore also a loading indicator.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Dynamic Header and Text)]
To make your `Card` look cleaner and to give the user the information that the header is clickable you can add some logic to your component.

1. Add a dynamic content `Text`

    The content text is not really informative. Let's change that and display the type of the chart. Add the following constants to your component (after the event handler):

    ```JSX
    const contentTitle = toggleCharts === 'lineChart' ? 'Line Chart' : 'Bar Chart';
    const switchToChart = toggleCharts === 'lineChart' ? 'Bar Chart' : 'Line Chart';
    ```
2. Change the title and add a subtitle to your `Card`
    First change the value of `heading` to something which explains the content of the `Card`. E.g. `"Stock Price"`
    Then add a `subtitle` prop. Here you can give the users the information, that they can switch between charts by clicking on the header.

    ```JSX
    <Card
        heading="Stock Price"
        style={{ width: "300px" }}
        headerInteractive
        onHeaderClick={handleHeaderClick}
        subtitle={`Click here to switch to ${switchToChart}`} >
        <Text style={spacing.sapUiContentPadding}>{contentTitle}</Text>
        {toggleCharts === "lineChart" ? (
            <LineChart datasets={datasets} labels={labels} loading={loading} />
        ) : (
            <BarChart datasets={datasets} labels={labels} loading={loading} />
        )}
    </Card>
    ```

[DONE]
[ACCORDION-END]
---
