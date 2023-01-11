---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-fiori]
primary_tag: programming-tool>html5
author_name: Lukas Harbarth
author_profile: https://github.com/Lukas742
---

# Add Routing to a UI5 Web Components for React Project
<!-- description --> Use routes to navigate between different pages using UI5 Web Components for React.

## You will learn
-  How to refactor code into different React components
-  How to create multiple pages
-  How to use routing


## Intro
In this tutorial, you will learn how to create routes to different paths of your application. For this you will create a new page and set up routing between the page and the dashboard.

---

### Create a Detail page

In `src` create a `Detail.jsx` file.

Inside of that file, create the `Detail` component that will return a title to your liking.

```JavaScript / JSX
import React from "react";
import { Title } from "@ui5/webcomponents-react";

export function Detail() {
  return <Title>Hello World!</Title>;
}
```

### Add Router


1. Install `react-router-dom`.

    [React Router](https://reactrouter.com) is a collection of navigational components that compose declaratively with your application.

    Execute the following line in the terminal in your root location of the project.

    ```Shell
    npm install react-router-dom
    ```

2. Import `HashRouter` in `src/App.js`.

    ```JavaScript / JSX
    import { HashRouter } from "react-router-dom";
    ```

    And wrap your root component inside of it:

    ```JavaScript / JSX
    <HashRouter>
        <MyApp />
    </HashRouter>
    ```

### Create Home component


1. In `src`, create a new `Home.jsx` file.

    For now only create a `Home` component that returns `null`.

    ```JavaScript / JSX
    import React from "react";

    export function Home() {
      return null;
    }
    ```

2. Refactor `MyApp.jsx`.

    Some refactoring is necessary now. Extract every component and the corresponding logic except for the `ShellBar` from `MyApp.jsx` and move it to `Home.jsx`.

    `MyApp.jsx` should now look like this:

    ```JavaScript / JSX
    import React from "react";
    import { Avatar, ShellBar, ShellBarItem } from "@ui5/webcomponents-react";
    import addIcon from "@ui5/webcomponents-icons/dist/add.js";

    export function MyApp() {
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
        </div>
      );
    }

    ```

    And `Home.jsx` like this:

    ```JavaScript / JSX
    import React, { useState } from "react";
    import {
      AnalyticalTable,
      Card,
      CardHeader,
      CustomListItem,
      FlexBox,
      FlexBoxDirection,
      FlexBoxJustifyContent,
      FlexBoxWrap,
      Icon,
      List,
      ProgressIndicator,
      StandardListItem,
      Text,
      ValueState,
    } from "@ui5/webcomponents-react";
    import { spacing, ThemingParameters } from "@ui5/webcomponents-react-base";
    import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";
    import lineChartIcon from "@ui5/webcomponents-icons/dist/line-chart.js";
    import barChartIcon from "@ui5/webcomponents-icons/dist/horizontal-bar-chart.js";
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

    export function Home() {
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
        <>
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
        </>
      );
    }
    ```

### Import Router components


1. In `MyApp.jsx`, import `Routes`, `Route` and `Navigate` from `react-router-dom` and the `Home` and `Detail` components.

    ```JavaScript / JSX
    import { Routes, Route, Navigate } from "react-router-dom";
    import { Home } from "./Home";
    import { Detail } from "./Detail";
    ```

2. Wrap your `ShellBar` component inside of [Fragments](https://reactjs.org/docs/fragments.html) and set up paths with `Routes` and the component (`element`) displayed respectively.

    ```JavaScript / JSX
    return (
      <>
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
        <Routes>
          <Route path="/home" element={<Home />} />
          <Route path="/detail" element={<Detail />} />
          <Route path="/" element={<Navigate replace to="/home" />} />
        </Routes>
      </>
    );
    ```

The `Navigate` component will redirect the user if the URL doesn't match any given paths.

Your current URL now displays the `#/home` path. If you replace `home` with `detail`, you will be routed to the detail page.

![Home](01_home.png)

![Detail](02_detail.png)

### Handle navigation


Except for changing the URL of the App the user doesn't have options to navigate to the `Detail` page. The page could contain some more information about activities and should therefore be connected to the `Progress Card`.

1. Go into your `Home` component and add `interactive` and `onClick={handleProgressHeaderClick}` to your second `CardHeader` component.

    ```JavaScript / JSX
    <Card
      header={
        <CardHeader
          titleText="Progress"
          subtitleText="List"
          avatar={<Icon name={listIcon} />}
          interactive
          onClick={handleProgressHeaderClick}
        />
      }
      style={{ width: "300px", ...spacing.sapUiContentPadding }}
    >
    ```

2. Define the `handleProgressHeaderClick` function as follows. And import the `useNavigate` hook from `react-router-dom`.

    ```JavaScript / JSX
    import { useNavigate } from "react-router-dom";
    ```
    ```JavaScript / JSX
    const navigate = useNavigate();
    const handleProgressHeaderClick = () => {
      navigate("/detail");
    };
    ```

    Now it is possible to navigate to the `Details` component, but right now, there is no way to go back except for the back button of the browser and typing in the URL manually.

3. Navigate back to the Home Screen

    Normally when you click on the Logo of the app, it should send you back to your `Home` screen. Let's implement that.

    Go into your `MyApp.jsx` file where your `ShellBar` is located and add an `onLogoClick` prop. The function is almost the same as for the `onHeaderClick`, but this time the destination should be the default page.

    ```JavaScript / JSX
    const navigate = useNavigate();
    const handleLogoClick = () => {
      navigate("./");
    };
    ```

    ```JavaScript / JSX
    <ShellBar
      logo={<img src="reactLogo.png" />}
      profile={<Avatar image="profilePictureExample.png" />}
      primaryTitle="My App"
      onLogoClick={handleLogoClick}
    >
    ```


### Code overview


If needed, you can compare your files with the following code references:

`MyApp.jsx`:

```JavaScript / JSX
import React from "react";
import { Avatar, ShellBar, ShellBarItem } from "@ui5/webcomponents-react";
import addIcon from "@ui5/webcomponents-icons/dist/add.js";
import { Navigate, Route, Routes, useNavigate } from "react-router-dom";
import { Home } from "./Home";
import { Detail } from "./Detail";

export function MyApp() {
  const navigate = useNavigate();
  const handleLogoClick = () => {
    navigate("./");
  };
  return (
    <>
      <ShellBar
        logo={<img src="reactLogo.png" />}
        profile={
          <Avatar>
            <img src="profilePictureExample.png" />
          </Avatar>
        }
        primaryTitle="My App"
        onLogoClick={handleLogoClick}
      >
        <ShellBarItem icon={addIcon} text="Add" />
      </ShellBar>
      <Routes>
        <Route path="/home" element={<Home />} />
        <Route path="/detail" element={<Detail />} />
        <Route path="/" element={<Navigate replace to="/home" />} />
      </Routes>
    </>
  );
}
```

`Home.jsx`:

```JavaScript / JSX
import React, { useState } from "react";
import {
  AnalyticalTable,
  Card,
  CardHeader,
  CustomListItem,
  FlexBox,
  FlexBoxDirection,
  FlexBoxJustifyContent,
  FlexBoxWrap,
  Icon,
  List,
  ProgressIndicator,
  StandardListItem,
  Text,
  ValueState,
} from "@ui5/webcomponents-react";
import { spacing, ThemingParameters } from "@ui5/webcomponents-react-base";
import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";
import lineChartIcon from "@ui5/webcomponents-icons/dist/line-chart.js";
import barChartIcon from "@ui5/webcomponents-icons/dist/horizontal-bar-chart.js";
import listIcon from "@ui5/webcomponents-icons/dist/list.js";
import tableViewIcon from "@ui5/webcomponents-icons/dist/table-view.js";
import { useNavigate } from "react-router-dom";

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

export function Home() {
  const [toggleCharts, setToggleCharts] = useState("lineChart");
  const [loading, setLoading] = useState(false);
  const navigate = useNavigate();
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
  const handleProgressHeaderClick = () => {
    navigate("/detail");
  };
  return (
    <>
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
              interactive
              onClick={handleProgressHeaderClick}
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
    </>
  );
}
```


---
