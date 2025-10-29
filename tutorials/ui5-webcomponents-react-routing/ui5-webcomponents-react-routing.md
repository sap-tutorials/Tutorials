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
In this tutorial, you will learn how to create routes to different paths of your application using [React Router](https://reactrouter.com/). For this you will create a new page and set up routing between the page and the dashboard.

---

### Create a Detail page

In `src` create a `Detail.tsx` file.

Inside of that file, create the `Detail` component that will return a title to your liking.

```TypeScript / TSX
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

2. Import `HashRouter` in `src/App.tsx`.

    ```TypeScript / TSX
    import { HashRouter } from "react-router-dom";
    ```

    And wrap your root component inside of it:

    ```TypeScript / TSX
    <HashRouter>
        <MyApp />
    </HashRouter>
    ```

### Create Home component


1. In `src`, create a new `Home.tsx` file.

    For now only create a `Home` component that returns `null`.

    ```TypeScript / TSX
    export function Home() {
      return null;
    }
    ```

2. Refactor `MyApp.tsx`.

    Some refactoring is necessary now. Extract every component and the corresponding logic except for the `ShellBar` from `MyApp.tsx` and move it to `Home.tsx`.

    `MyApp.tsx` should now look like this:

    ```TypeScript / TSX
    import activateIcon from "@ui5/webcomponents-icons/dist/activate.js";
    import { Avatar, ShellBar, ShellBarItem } from "@ui5/webcomponents-react";
    import profilePictureExample from "./assets/profilePictureExample.png";
    import reactLogo from "./assets/reactLogo.png";
    
    export function MyApp() {
      return (
        <div>
          <ShellBar
            logo={<img src={reactLogo} alt="Company Logo" />}
            profile={
              <Avatar>
                <img src={profilePictureExample} alt="User Avatar" />
              </Avatar>
            }
            primaryTitle="My App"
          >
            <ShellBarItem icon={activateIcon} text="Activate" />
          </ShellBar>
        </div>
      );
    }
    ```

    And `Home.tsx` like this:

    ```TypeScript / TSX
    import tableViewIcon from "@ui5/webcomponents-icons/dist/table-view.js";
    import listIcon from "@ui5/webcomponents-icons/dist/list.js";
    import lineChartIcon from "@ui5/webcomponents-icons/dist/line-chart.js";
    import barChartIcon from "@ui5/webcomponents-icons/dist/horizontal-bar-chart.js";
    import { useState } from "react";
    import {
      Card,
      CardHeader,
      Text,
      List,
      ListItemStandard,
      ListItemCustom,
      ProgressIndicator,
      FlexBox,
      FlexBoxJustifyContent,
      FlexBoxWrap,
      FlexBoxDirection,
      AnalyticalTable,
      Icon,
    } from "@ui5/webcomponents-react";
    import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";
    import ValueState from "@ui5/webcomponents-base/dist/types/ValueState.js";
    
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
    
    export function Home() {
      const [toggleCharts, setToggleCharts] = useState("lineChart");
      const [loading, setLoading] = useState(false);
    
      const contentTitle =
        toggleCharts === "lineChart" ? "Line Chart" : "Bar Chart";
      const switchToChart =
        toggleCharts === "lineChart" ? "Bar Chart" : "Line Chart";
    
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
      return (
        <FlexBox
          justifyContent={FlexBoxJustifyContent.Center}
          wrap={FlexBoxWrap.Wrap}
          style={{ padding: "var(--sapContent_Space_M)" }}
        >
          <Card
            header={
              <CardHeader
                titleText="Prices"
                subtitleText={`Click here to switch to ${switchToChart}`}
                interactive
                avatar={
                  <Icon
                    name={
                      toggleCharts === "lineChart" ? lineChartIcon : barChartIcon
                    }
                    accessibleName={contentTitle}
                  />
                }
                onClick={handleHeaderClick}
              />
            }
            style={{ width: "300px", margin: "var(--sapContent_Margin_Small)" }}
          >
            <Text style={{ padding: "var(--sapContent_Space_S)" }}>
              {contentTitle}
            </Text>
            {toggleCharts === "lineChart" ? (
              <LineChart
                dimensions={[{ accessor: "month" }]}
                measures={[{ accessor: "data", label: "Price" }]}
                dataset={dataset}
                loading={loading}
              />
            ) : (
              <BarChart
                dimensions={[{ accessor: "month" }]}
                measures={[{ accessor: "data", label: "Price" }]}
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
            style={{ width: "300px", margin: "var(--sapContent_Margin_Small)" }}
          >
            <List>
              <ListItemStandard
                additionalText="finished"
                additionalTextState={ValueState.Positive}
              >
                Activity 1
              </ListItemStandard>
              <ListItemStandard
                additionalText="failed"
                additionalTextState={ValueState.Negative}
              >
                Activity 2
              </ListItemStandard>
              <ListItemCustom>
                <FlexBox
                  direction={FlexBoxDirection.Column}
                  fitContainer
                  style={{ paddingBlock: "var(--sapContent_Space_S)" }}
                >
                  <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
                    <Text style={{ fontSize: "var(--sapFontLargeSize)" }}>
                      Activity 3
                    </Text>
                    <Text style={{ color: "var(--sapCriticalTextColor)" }}>
                      in progress
                    </Text>
                  </FlexBox>
                  <ProgressIndicator
                    value={89}
                    valueState={ValueState.Positive}
                    style={{ marginBlockStart: "0.5rem" }}
                  />
                </FlexBox>
              </ListItemCustom>
              <ListItemCustom>
                <FlexBox
                  direction={FlexBoxDirection.Column}
                  fitContainer
                  style={{ paddingBlock: "var(--sapContent_Space_S)" }}
                >
                  <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
                    <Text style={{ fontSize: "var(--sapFontLargeSize)" }}>
                      Activity 3
                    </Text>
                    <Text style={{ color: "var(--sapCriticalTextColor)" }}>
                      in progress
                    </Text>
                  </FlexBox>
                  <ProgressIndicator
                    value={5}
                    valueState={ValueState.Negative}
                    style={{ marginBlockStart: "0.5rem" }}
                  />
                </FlexBox>
              </ListItemCustom>
            </List>
          </Card>
          <Card
            header={
              <CardHeader
                titleText="AnalyticalTable"
                avatar={<Icon name={tableViewIcon} />}
              />
            }
            style={{
              maxWidth: "900px",
              margin: "var(--sapContent_Margin_Small)",
            }}
          >
            <AnalyticalTable
              data={tableData}
              columns={tableColumns}
              visibleRows={5}
            />
          </Card>
        </FlexBox>
      );
    }
    ```

### Import Router components


1. In `MyApp.tsx`, import `Routes`, `Route` and `Navigate` from `react-router-dom` and the `Home` and `Detail` components.

    ```TypeScript / TSX
    import { Routes, Route, Navigate } from "react-router-dom";
    import { Home } from "./Home";
    import { Detail } from "./Detail";
    ```

2. Wrap your `ShellBar` component inside of [Fragments](https://reactjs.org/docs/fragments.html) and set up paths with `Routes` and the component (`element`) displayed respectively.

    ```TypeScript / TSX
    return (
      <>
        <ShellBar
          logo={<img src={reactLogo} alt="Company Logo" />}
          profile={
            <Avatar>
              <img src={profilePictureExample} alt="User Avatar" />
            </Avatar>
          }
          primaryTitle="My App"
        >
          <ShellBarItem icon={activateIcon} text="Activate" />
        </ShellBar>
        <Routes>
          <Route path="/home" element={<Home />} />
          <Route path="/detail" element={<Detail />} />
          <Route path="*" element={<Navigate replace to="/home" />} />
        </Routes>
      </>
    );
    ```

The `Navigate` component will redirect the user if the URL doesn't match any given paths.

Your current URL now displays the `#/home` path. If you replace `home` with `detail`, you will be routed to the detail page.

![Home](01_home.png)

![Detail](02_detail.png)

### Handle navigation


Except for changing the URL of the App users don't have options to navigate to the `Detail` page. The page could contain some more information about activities and should therefore be connected to the `Progress Card`.

1. Go into your `Home` component and add `interactive` and `onClick={handleProgressHeaderClick}` to your second `CardHeader` component.

    ```TypeScript / TSX
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
      style={{ width: "300px", margin: "var(--sapContent_Margin_Small)" }}
    >
    ```

2. Define the `handleProgressHeaderClick` function as follows. And import the `useNavigate` hook from `react-router-dom`.

    ```TypeScript / TSX
    import { useNavigate } from "react-router-dom";
    ```
    ```TypeScript / TSX
    const navigate = useNavigate();
    const handleProgressHeaderClick = () => {
      navigate("/detail");
    };
    ```

    Now it is possible to navigate to the `Details` component, but right now, there is no way to go back except for the back button of the browser and typing in the URL manually.

3. Navigate back to the Home Screen

    Normally when you click on the Logo of the app, it should send you back to your `Home` screen. Let's implement that.

    Go into your `MyApp.tsx` file where your `ShellBar` is located and add an `onLogoClick` prop. The function is almost the same as for the `onHeaderClick`, but this time the destination should be the default page.

    ```TypeScript / TSX
    const navigate = useNavigate();
    const handleLogoClick = () => {
      navigate("./");
    };
    ```

    ```TypeScript / TSX
    <ShellBar
      logo={<img src={reactLogo} alt="Company Logo" />}
      profile={
        <Avatar>
          <img src={profilePictureExample} alt="User Avatar" />
        </Avatar>
      }
      primaryTitle="My App"
      onLogoClick={handleLogoClick}
    >
    ```


### Code overview


If needed, you can compare your files with the following code references:

`MyApp.tsx`:

```TypeScript / TSX
import activateIcon from "@ui5/webcomponents-icons/dist/activate.js";
import { Avatar, ShellBar, ShellBarItem } from "@ui5/webcomponents-react";
import profilePictureExample from "./assets/profilePictureExample.png";
import reactLogo from "./assets/reactLogo.png";
import { Routes, Route, Navigate } from "react-router-dom";
import { Home } from "./Home";
import { Detail } from "./Detail";
import { useNavigate } from "react-router-dom";

export function MyApp() {
  const navigate = useNavigate();
  const handleLogoClick = () => {
    navigate("./");
  };
  return (
    <>
      <ShellBar
        logo={<img src={reactLogo} alt="Company Logo" />}
        profile={
          <Avatar>
            <img src={profilePictureExample} alt="User Avatar" />
          </Avatar>
        }
        primaryTitle="My App"
        onLogoClick={handleLogoClick}
      >
        <ShellBarItem icon={activateIcon} text="Activate" />
      </ShellBar>
      <Routes>
        <Route path="/home" element={<Home />} />
        <Route path="/detail" element={<Detail />} />
        <Route path="*" element={<Navigate replace to="/home" />} />
      </Routes>
    </>
  );
}
```

`Home.tsx`:

```TypeScript / TSX
import tableViewIcon from "@ui5/webcomponents-icons/dist/table-view.js";
import listIcon from "@ui5/webcomponents-icons/dist/list.js";
import lineChartIcon from "@ui5/webcomponents-icons/dist/line-chart.js";
import barChartIcon from "@ui5/webcomponents-icons/dist/horizontal-bar-chart.js";
import { useState } from "react";
import {
  Card,
  CardHeader,
  Text,
  List,
  ListItemStandard,
  ListItemCustom,
  ProgressIndicator,
  FlexBox,
  FlexBoxJustifyContent,
  FlexBoxWrap,
  FlexBoxDirection,
  AnalyticalTable,
  Icon,
} from "@ui5/webcomponents-react";
import { BarChart, LineChart } from "@ui5/webcomponents-react-charts";
import ValueState from "@ui5/webcomponents-base/dist/types/ValueState.js";
import { useNavigate } from "react-router-dom";

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

export function Home() {
  const [toggleCharts, setToggleCharts] = useState("lineChart");
  const [loading, setLoading] = useState(false);
  const navigate = useNavigate();

  const contentTitle =
    toggleCharts === "lineChart" ? "Line Chart" : "Bar Chart";
  const switchToChart =
    toggleCharts === "lineChart" ? "Bar Chart" : "Line Chart";

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

  const handleProgressHeaderClick = () => {
    navigate("/detail");
  };

  return (
    <FlexBox
      justifyContent={FlexBoxJustifyContent.Center}
      wrap={FlexBoxWrap.Wrap}
      style={{ padding: "var(--sapContent_Space_M)" }}
    >
      <Card
        header={
          <CardHeader
            titleText="Prices"
            subtitleText={`Click here to switch to ${switchToChart}`}
            interactive
            avatar={
              <Icon
                name={
                  toggleCharts === "lineChart" ? lineChartIcon : barChartIcon
                }
                accessibleName={contentTitle}
              />
            }
            onClick={handleHeaderClick}
          />
        }
        style={{ width: "300px", margin: "var(--sapContent_Margin_Small)" }}
      >
        <Text style={{ padding: "var(--sapContent_Space_S)" }}>
          {contentTitle}
        </Text>
        {toggleCharts === "lineChart" ? (
          <LineChart
            dimensions={[{ accessor: "month" }]}
            measures={[{ accessor: "data", label: "Price" }]}
            dataset={dataset}
            loading={loading}
          />
        ) : (
          <BarChart
            dimensions={[{ accessor: "month" }]}
            measures={[{ accessor: "data", label: "Price" }]}
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
        style={{ width: "300px", margin: "var(--sapContent_Margin_Small)" }}
      >
        <List>
          <ListItemStandard
            additionalText="finished"
            additionalTextState={ValueState.Positive}
          >
            Activity 1
          </ListItemStandard>
          <ListItemStandard
            additionalText="failed"
            additionalTextState={ValueState.Negative}
          >
            Activity 2
          </ListItemStandard>
          <ListItemCustom>
            <FlexBox
              direction={FlexBoxDirection.Column}
              fitContainer
              style={{ paddingBlock: "var(--sapContent_Space_S)" }}
            >
              <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
                <Text style={{ fontSize: "var(--sapFontLargeSize)" }}>
                  Activity 3
                </Text>
                <Text style={{ color: "var(--sapCriticalTextColor)" }}>
                  in progress
                </Text>
              </FlexBox>
              <ProgressIndicator
                value={89}
                valueState={ValueState.Positive}
                style={{ marginBlockStart: "0.5rem" }}
              />
            </FlexBox>
          </ListItemCustom>
          <ListItemCustom>
            <FlexBox
              direction={FlexBoxDirection.Column}
              fitContainer
              style={{ paddingBlock: "var(--sapContent_Space_S)" }}
            >
              <FlexBox justifyContent={FlexBoxJustifyContent.SpaceBetween}>
                <Text style={{ fontSize: "var(--sapFontLargeSize)" }}>
                  Activity 3
                </Text>
                <Text style={{ color: "var(--sapCriticalTextColor)" }}>
                  in progress
                </Text>
              </FlexBox>
              <ProgressIndicator
                value={5}
                valueState={ValueState.Negative}
                style={{ marginBlockStart: "0.5rem" }}
              />
            </FlexBox>
          </ListItemCustom>
        </List>
      </Card>
      <Card
        header={
          <CardHeader
            titleText="AnalyticalTable"
            avatar={<Icon name={tableViewIcon} />}
          />
        }
        style={{
          maxWidth: "900px",
          margin: "var(--sapContent_Margin_Small)",
        }}
      >
        <AnalyticalTable
          data={tableData}
          columns={tableColumns}
          visibleRows={5}
        />
      </Card>
    </FlexBox>
  );
}
```


---
