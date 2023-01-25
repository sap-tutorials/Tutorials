---
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-fiori]
primary_tag: programming-tool>html5
author_name: Lukas Harbarth
author_profile: https://github.com/Lukas742
---

# Create an Analytical Dashboard via UI5 Web Components for React
<!-- description --> Create an analytical dashboard with different components using UI5 Web Components for React.

## You will learn
-  How to use the `ShellBar` component
-  How to use the `AnalyticalTable` component
-  How to style components

## Intro
So far, you have built your first `Card` component. Now to take things further, it's time to build something bigger. In this step, you will learn how different components work together by building an analytical dashboard.

---

### Add necessary imports

To make things easier, first import all the components you will need in this step. Just copy the code below and replace the previous imported components in `MyApp.jsx`.

```JavaScript / JSX
import {
  Avatar,
  Card,
  CardHeader,
  Text,
  ShellBar,
  ShellBarItem,
  List,
  StandardListItem,
  CustomListItem,
  ValueState,
  ProgressIndicator,
  FlexBox,
  FlexBoxJustifyContent,
  FlexBoxWrap,
  FlexBoxDirection,
  AnalyticalTable,
  Icon
} from "@ui5/webcomponents-react";
import { spacing, ThemingParameters } from "@ui5/webcomponents-react-base";
```


### Add a `ShellBar`


The `ShellBar` is the central navigation element in your Web Application and should therefore be visible on all pages.

> Again, you can try it out in the [Storybook](https://sap.github.io/ui5-webcomponents-react/?path=/docs/layouts-floorplans-shellbar--default-story).

1. Start with adding the `ShellBar` above your `Card` component and add a `primaryTitle` prop.

    ```JavaScript / JSX
    <ShellBar primaryTitle="My App" />
    ```

2. Add some more properties

    The logo of the application should be displayed and also a profile picture would be nice.

    Use the `logo` and `profile` prop to achieve this. The `logo` prop accepts either an `img` tag or the `Avatar` component, the `profile` prop only accepts the `Avatar` component. First add the `logo` prop like this:

    ```JavaScript / JSX
     <ShellBar logo={<img src="" />} primaryTitle="My App" />
    ```

    Then pass the `profile` prop like this:

    ```JavaScript / JSX
    <ShellBar
      logo={<img src="" />}
      profile={<Avatar><img src="" /></Avatar>}
      primaryTitle="My App"
    />
    ```

    You can use your own image, use an URL to an image or simply download the images below and add them to your `public` folder in your project.

    [`reactLogo.png`](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/ui5-webcomponents-react-dashboard/reactLogo.png)

    [`profilePictureExample.png`](https://github.com/SAPDocuments/Tutorials/raw/master/tutorials/ui5-webcomponents-react-dashboard/profilePictureExample.png)

    ```JavaScript / JSX
    <ShellBar
      logo={<img src="reactLogo.png" />}
      profile={
        <Avatar>
          <img src="profilePictureExample.png" />
        </Avatar>
      }
      primaryTitle="My App"
    />
    ```

3. Add custom elements

    By passing a `ShellBarItem` as `child` you are able to add custom elements to your `ShellBar`. The element is basically a `Button` with responsive behavior and styling adapted to the `ShellBar`.

    ```JavaScript / JSX
    <ShellBar
      logo={<img src="reactLogo.png" />}
      profile={
        <Avatar>
          <img src="profilePictureExample.png" />
        </Avatar>
      }
      primaryTitle="My App"
    >
      <ShellBarItem icon="add" text="Add" />
    </ShellBar>
    ```

    That is strange -- when you render your component, the `ShellBarItem` is not shown.

    Every `Icon` that is used in a component has to be imported manually. All available icons can be found [here](https://sapui5.hana.ondemand.com/test-resources/sap/m/demokit/iconExplorer/webapp/index.html#/overview/SAP-icons).

    Add this line to your imports:

    ```JavaScript / JSX
    import addIcon from "@ui5/webcomponents-icons/dist/add.js";
    ```

    Now your `ShellBarItem` shows up at the right side of the `ShellBar`.

    ![ShellBar](02_shellbar.png)

    For maintainability reasons, replace `icon="add"` with the import name `icon={addIcon}`. Now if you replace the icon but forget to remove the import, modern IDEs or linters like [eslint](https://eslint.org/) will tell you that there is an unused import in your file.

Your component should now look like this:

```JavaScript / JSX
import React, { useState } from "react";
import {
  Avatar,
  Card,
  CardHeader,
  Text,
  ShellBar,
  ShellBarItem,
  List,
  CustomListItem,
  StandardListItem,
  ValueState,
  ProgressIndicator,
  FlexBox,
  FlexBoxJustifyContent,
  FlexBoxWrap,
  FlexBoxDirection,
  AnalyticalTable,
  Icon,
} from "@ui5/webcomponents-react";
import { spacing, ThemingParameters } from "@ui5/webcomponents-react-base";
import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";
import lineChartIcon from "@ui5/webcomponents-icons/dist/line-chart.js";
import barChartIcon from "@ui5/webcomponents-icons/dist/horizontal-bar-chart.js";
import addIcon from "@ui5/webcomponents-icons/dist/add.js";
import listIcon from "@ui5/webcomponents-icons/dist/list.js";

const dataset = [
  {
    month: "January",
    data: 65,
  },
  {
    month: "February",
    data: 59,
  },
  {
    month: "March",
    data: 80,
  },
  {
    month: "April",
    data: 81,
  },
  {
    month: "May",
    data: 56,
  },
  {
    month: "June",
    data: 55,
  },
  {
    month: "July",
    data: 40,
  },
];

export function MyApp() {
  const [toggleCharts, setToggleCharts] = useState("lineChart");
  const [loading, setLoading] = useState(false);
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
  const contentTitle =
    toggleCharts === "lineChart" ? "Line Chart" : "Bar Chart";
  const switchToChart =
    toggleCharts === "lineChart" ? "Bar Chart" : "Line Chart";
  return (
    <div>
      <ShellBar
        logo={<img src="reactLogo.png" />}
        profile={
          <Avatar>
            <img src="profilePictureExample.png" />
          </Avatar>
        }
        primaryTitle="My App"
      >
        <ShellBarItem icon={addIcon} text="Add" />
      </ShellBar>
      <Card
        header={
          <CardHeader
            titleText="Stock Prices"
            subtitleText={`Click here to switch to ${switchToChart}`}
            interactive
            onClick={handleHeaderClick}
            avatar={
              <Icon
                name={
                  toggleCharts === "lineChart" ? lineChartIcon : barChartIcon
                }
              />
            }
          />
        }
        style={{ width: "300px" }}
      >
        <Text style={spacing.sapUiContentPadding}>{contentTitle}</Text>
        {toggleCharts === "lineChart" ? (
          <LineChart
            dimensions={[{ accessor: "month" }]}
            measures={[{ accessor: "data", label: "Stock Price" }]}
            dataset={dataset}
            loading={loading}
          />
        ) : (
          <BarChart
            dimensions={[{ accessor: "month" }]}
            measures={[{ accessor: "data", label: "Stock Price" }]}
            dataset={dataset}
            loading={loading}
          />
        )}
      </Card>
    </div>
  );
}
```

### Add a `List`


1. To wrap the `List` add a `Card` (right after the first one).

    ```JavaScript / JSX
    <Card
      header={
        <CardHeader
          titleText="Progress"
          subtitleText="List"
          avatar={<Icon name={listIcon} />}
        />
      }
      style={{ width: "300px" }}
    ></Card>
    ```

2. Add the list `Icon` to your imports.

    ```JavaScript / JSX
    import listIcon from "@ui5/webcomponents-icons/dist/list.js";
    ```

3. Add the `List` component as child of the `Card`.

    ```JavaScript / JSX
    <List></List>
    ```

4. To render elements of the list, use the `StandardListItem` and pass a `string` as child.

    ```JavaScript / JSX
     <List>
       <StandardListItem>Activity 1</StandardListItem>
     </List>
    ```

5. The user should know the status of the activities. Add the `additionalText` prop to the `StandardListItem`. To visualize if the status is neutral, positive or negative, also add the `additionalTextState` prop.

    ```JavaScript / JSX
     <StandardListItem additionalText="finished" additionalTextState={ValueState.Success}>
       Activity 1
     </StandardListItem>
     <StandardListItem additionalText="failed" additionalTextState={ValueState.Error}>
       Activity 2
     </StandardListItem>
    ```

6. The "Progress" card shows two list items, but both of them are already completed. Let's create two more activities which are still in progress.

    First, create two `CustomListItem`s below the completed items.

    ```JavaScript / JSX
    <CustomListItem></CustomListItem>
    <CustomListItem></CustomListItem>
    ```

    > The `CustomListItem` allows customizing the content of the list item and for this reason doesn't offer props like `additionalText`.

    To show the progress, add the `ProgressIndicator` as child of the items, with the following props:

    - `value`: The value, which indicates the progress
    - `valueState`: The value-state (color) of the indicator

    ```JavaScript / JSX
    <CustomListItem>
      <ProgressIndicator value={89} valueState={ValueState.Success} />
    </CustomListItem>
    <CustomListItem>
      <ProgressIndicator value={5} valueState={ValueState.Error} />
    </CustomListItem>
    ```

7. The indicators are displayed as part of the list item, but the title and status of the activities is still missing.
For this, add two `Text` components above the indicator:

    ```JavaScript / JSX
    <CustomListItem>
      <Text>Activity 3</Text>
      <Text>in progress</Text>
      <ProgressIndicator value={89} valueState={ValueState.Success} />
    </CustomListItem>
    ```

8. All necessary information are now available in each item, but the formatting looks terrible. Let's fix that by using a flex box:

    ```JavaScript / JSX
    <CustomListItem>
      <FlexBox direction={FlexBoxDirection.Column}>
        <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
          <Text>Activity 3</Text>
          <Text>in progress</Text>
        </FlexBox>
        <ProgressIndicator value={89} valueState={ValueState.Success} />
      </FlexBox>
    </CustomListItem>
    ```

    > The `FlexBox` implements most of the [`CSS Flexbox`](https://www.w3schools.com/css/css3_flexbox.asp) behavior without being forced to actually use CSS or other styling methods.

9. The content of the list item is now aligned correctly, but doesn't apply the correct padding, colors and dimensions. To fix this as well, pass the `style` prop, to use the default [React `inlineStyle` syntax](https://reactjs.org/docs/dom-elements.html):

    ```JavaScript / JSX
    <CustomListItem>
      <FlexBox
        direction={FlexBoxDirection.Column}
        style={{ width: "100%", ...spacing.sapUiContentPadding }}
      >
        <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
          <Text style={{ fontSize: ThemingParameters.sapFontLargeSize }}>
            Activity 3
          </Text>
          <Text style={{ color: ThemingParameters.sapCriticalTextColor }}>
            in progress
          </Text>
        </FlexBox>
        <ProgressIndicator
          value={89}
          valueState={ValueState.Success}
          style={{ ...spacing.sapUiTinyMarginTop }}
        />
      </FlexBox>
    </CustomListItem>
    ```

    > The `ThemingParameters` used for the color of the status text, contain all available theme dependent styles of UI5 Web Components for React. You will find out more about this in [Tutorial 6](ui5-webcomponents-react-styling)

10. Finally, apply the same layout and styles to the content of the second `CustomListItem`.

![List](03_list.png)


### Add an `AnalyticalTable`


1. The last tile should contain a `AnalyticalTable` component. Again, create a `Card` to wrap the Table and set the `max-width` to `900px`.

    ```JavaScript / JSX
    <Card
      header={
        <CardHeader
          titleText="AnalyticalTable"
          avatar={<Icon name={tableViewIcon} />}
        />
      }
      style={{ maxWidth: "900px" }}
    >
      <AnalyticalTable />
    </Card>
    ```

    Also import the `table-view` `Icon`.

    ```JavaScript / JSX
    import tableViewIcon from "@ui5/webcomponents-icons/dist/table-view.js";
    ```
2. Add data and columns to the table. The `columns` prop expects an array of objects that include at least the `accessor` to the data or a unique `id` property. The value of `Header` will be shown as column header.

    You can create your own data or just use the code below and paste it right after the definition of the `dataset` of the chart.

    ```JavaScript / JSX
    const tableData = new Array(500).fill(null).map((_, index) => {
      return {
        name: `name${index}`,
        age: Math.floor(Math.random() * 100),
        friend: {
          name: `friend.Name${index}`,
          age: Math.floor(Math.random() * 100)
        }
      };
    });

    const tableColumns = [
      {
        Header: "Name",
        accessor: "name" // String-based value accessors!
      },
      {
        Header: "Age",
        accessor: "age"
      },
      {
        Header: "Friend Name",
        accessor: "friend.name"
      },
      {
        Header: "Friend Age",
        accessor: "friend.age"
      }
    ];
    ```

3. Display the data by replacing the current table with.

    ```JavaScript / JSX
    <AnalyticalTable data={tableData} columns={tableColumns} />
    ```

    ![Table](04_table.png)

3. Add more properties

    You can add many more properties to the `AnalyticalTable` component. For example you can group the columns with `groupable`, search for entries in a column with `filterable`, and change the row height with `rowHeight`.

    The default visible rows count is at 15. This number is a bit to high for a dashboard table. Reduce the `visibleRows` count to 5 by setting the corresponding prop.

    ```JavaScript / JSX
    <AnalyticalTable
      data={tableData}
      columns={tableColumns}
      visibleRows={5}/>
    ```

### Dashboard layout


At the moment, the dashboard doesn't really look like a dashboard. The components are way too close to each other and not aligned correctly. Let's change that.

1. Add padding to each `Card`

    To add a padding to the cards, you can use `spacing` again. Inside of the style property [spread](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax) the `spacing` object and append the style. Do this for each `Card` component.

    ```JavaScript / JSX
    <Card
      header={
        <CardHeader
          titleText="Stock Prices"
          ...
        />
      }
      style={{ width: "300px", ...spacing.sapUiContentPadding }}
    >
    ```

    ```JavaScript / JSX
    <Card
      header={
        <CardHeader
          titleText="Progress"
          ...
        />
      }
      style={{ width: "300px", ...spacing.sapUiContentPadding }}
    >
    ```

    ```JavaScript / JSX
    <Card
      header={
        <CardHeader
          titleText="AnalyticalTable"
          ...
        />
      }
      style={{ maxWidth: "900px", ...spacing.sapUiContentPadding }}
    >
    ```

2. Align the elements

    To properly align the tiles, use a `FlexBox` component and wrap your `Cards` inside of it. Use the `justifyContent` prop to center align all elements and `wrap` to make them move to the next line if not enough space is available, also apply the `style` prop to add a padding to the whole content area.

    ```JavaScript / JSX
    <FlexBox
        justifyContent={FlexBoxJustifyContent.Center}
        wrap={FlexBoxWrap.Wrap}
        style={spacing.sapUiContentPadding}
        ...
    </FlexBox>
    ```

Your component should now look like this:

<!-- border -->![Dashboard](05_dashboard.png)

```JavaScript / JSX
import React, { useState } from "react";
import {
  Avatar,
  Card,
  CardHeader,
  Text,
  ShellBar,
  ShellBarItem,
  List,
  CustomListItem,
  StandardListItem,
  ValueState,
  ProgressIndicator,
  FlexBox,
  FlexBoxJustifyContent,
  FlexBoxWrap,
  FlexBoxDirection,
  AnalyticalTable,
  Icon,
} from "@ui5/webcomponents-react";
import { spacing, ThemingParameters } from "@ui5/webcomponents-react-base";
import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";
import lineChartIcon from "@ui5/webcomponents-icons/dist/line-chart.js";
import barChartIcon from "@ui5/webcomponents-icons/dist/horizontal-bar-chart.js";
import addIcon from "@ui5/webcomponents-icons/dist/add.js";
import listIcon from "@ui5/webcomponents-icons/dist/list.js";
import tableViewIcon from "@ui5/webcomponents-icons/dist/table-view.js";

const dataset = [
  {
    month: "January",
    data: 65,
  },
  {
    month: "February",
    data: 59,
  },
  {
    month: "March",
    data: 80,
  },
  {
    month: "April",
    data: 81,
  },
  {
    month: "May",
    data: 56,
  },
  {
    month: "June",
    data: 55,
  },
  {
    month: "July",
    data: 40,
  },
];

const tableData = new Array(500).fill(null).map((_, index) => {
  return {
    name: `name${index}`,
    age: Math.floor(Math.random() * 100),
    friend: {
      name: `friend.Name${index}`,
      age: Math.floor(Math.random() * 100),
    },
  };
});

const tableColumns = [
  {
    Header: "Name",
    accessor: "name", // String-based value accessors!
  },
  {
    Header: "Age",
    accessor: "age",
  },
  {
    Header: "Friend Name",
    accessor: "friend.name",
  },
  {
    Header: "Friend Age",
    accessor: "friend.age",
  },
];

export function MyApp() {
  const [toggleCharts, setToggleCharts] = useState("lineChart");
  const [loading, setLoading] = useState(false);
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
  const contentTitle =
    toggleCharts === "lineChart" ? "Line Chart" : "Bar Chart";
  const switchToChart =
    toggleCharts === "lineChart" ? "Bar Chart" : "Line Chart";
  return (
    <div>
      <ShellBar
        logo={<img src="reactLogo.png" />}
        profile={
          <Avatar>
            <img src="profilePictureExample.png" />
          </Avatar>
        }
        primaryTitle="My App"
      >
        <ShellBarItem icon={addIcon} text="Add" />
      </ShellBar>
      <FlexBox
        justifyContent={FlexBoxJustifyContent.Center}
        wrap={FlexBoxWrap.Wrap}
        style={spacing.sapUiContentPadding}
      >
        <Card
          header={
            <CardHeader
              titleText="Stock Prices"
              subtitleText={`Click here to switch to ${switchToChart}`}
              interactive
              onClick={handleHeaderClick}
              avatar={
                <Icon
                  name={
                    toggleCharts === "lineChart" ? lineChartIcon : barChartIcon
                  }
                />
              }
            />
          }
          style={{ width: "300px", ...spacing.sapUiContentPadding }}
        >
          <Text style={spacing.sapUiContentPadding}>{contentTitle}</Text>
          {toggleCharts === "lineChart" ? (
            <LineChart
              dimensions={[{ accessor: "month" }]}
              measures={[{ accessor: "data", label: "Stock Price" }]}
              dataset={dataset}
              loading={loading}
            />
          ) : (
            <BarChart
              dimensions={[{ accessor: "month" }]}
              measures={[{ accessor: "data", label: "Stock Price" }]}
              dataset={dataset}
              loading={loading}
            />
          )}
        </Card>
        <Card
          header={
            <CardHeader
              titleText="Progress"
              subtitleText="List"
              avatar={<Icon name={listIcon} />}
            />
          }
          style={{ width: "300px", ...spacing.sapUiContentPadding }}
        >
          <List>
            <StandardListItem
              additionalText="finished"
              additionalTextState={ValueState.Success}
            >
              Activity 1
            </StandardListItem>
            <StandardListItem
              additionalText="failed"
              additionalTextState={ValueState.Error}
            >
              Activity 2
            </StandardListItem>
            <CustomListItem>
              <FlexBox
                direction={FlexBoxDirection.Column}
                style={{ width: "100%", ...spacing.sapUiContentPadding }}
              >
                <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
                  <Text
                    style={{ fontSize: ThemingParameters.sapFontLargeSize }}
                  >
                    Activity 3
                  </Text>
                  <Text
                    style={{ color: ThemingParameters.sapCriticalTextColor }}
                  >
                    in progress
                  </Text>
                </FlexBox>
                <ProgressIndicator
                  value={89}
                  valueState={ValueState.Success}
                  style={{ ...spacing.sapUiTinyMarginTop }}
                />
              </FlexBox>
            </CustomListItem>
            <CustomListItem>
              <FlexBox
                direction={FlexBoxDirection.Column}
                style={{ width: "100%", ...spacing.sapUiContentPadding }}
              >
                <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
                  <Text
                    style={{ fontSize: ThemingParameters.sapFontLargeSize }}
                  >
                    Activity 4
                  </Text>
                  <Text
                    style={{ color: ThemingParameters.sapCriticalTextColor }}
                  >
                    in progress
                  </Text>
                </FlexBox>
                <ProgressIndicator
                  value={5}
                  valueState={ValueState.Error}
                  style={{ ...spacing.sapUiTinyMarginTop }}
                />
              </FlexBox>
            </CustomListItem>
          </List>
        </Card>
        <Card
          header={
            <CardHeader
              titleText="AnalyticalTable"
              avatar={<Icon name={tableViewIcon} />}
            />
          }
          style={{ maxWidth: "900px", ...spacing.sapUiContentPadding }}
        >
          <AnalyticalTable
            data={tableData}
            columns={tableColumns}
            visibleRows={5}
          />
        </Card>
      </FlexBox>
    </div>
  );
}
```




---
