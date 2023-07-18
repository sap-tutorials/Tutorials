---
parser: v2
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>javascript]
primary_tag: topic>user-interface
---

# Build Luigi App with React
<!-- description --> Create a mock shopping application with React and configure it using Luigi.

## You will learn
  - How to add content to your React web shopping app
  - How to use Luigi to configure navigation
  - How to create simple micro-frontends using React
---

### Add a file with product data


In this step, you will create a file with information about the products on sale in your shopping app.

In a real life implementation, this data would be provided by an external service/API. But for simplicity, you will create a `.js` file containing dummy data. This file will provide the displayed data in the micro-frontend. You will create a similar file in the UI5 micro-frontend to avoid bundling issues.

1. Navigate to `react-core-mf/src/assets` and create a `products.js` file with the following content:

    ```Javascript
    export const ProductCollection = [
    {
        "id": 101,
        "name": "Mouse",
        "price": 45.0,
        "stock": 80,
        "icon": "product",
        "currencyCode": "EUR",
        "orderQuantity": 2,
        "description": "Wireless Gaming Mouse with Sensor"
    },
    {
        "id": 102,
        "name": "Keyboard",
        "price": 50.0,
        "stock": 22,
        "icon": "product",
        "currencyCode": "EUR",
        "orderQuantity": 1,
        "description": "A physical keyboard that uses an individual spring and switch for each key. Today, only premium keyboards are built with key switches."
    },
    {
        "id": 103,
        "name": "Optical Mouse",
        "price": 35.0,
        "stock": 4,
        "icon": "product",
        "currencyCode": "EUR",
        "orderQuantity": 2,
        "description": "Utilizing the latest optical sensing technology, the USB Optical Scroll Mouse records precise motion."
    },
    {
        "id": 104,
        "name": "Laptop Pro",
        "price": 1299.0,
        "stock": 11,
        "icon": "laptop",
        "currencyCode": "EUR",
        "orderQuantity": 3,
        "description": "Newest laptop featuring a touch-sensitive OLED display."
    },
    {
        "id": 105,
        "name": "Mouse 2",
        "price": 40.0,
        "stock": 20,
        "icon": "product",
        "currencyCode": "EUR",
        "orderQuantity": 6,
        "description": "The Mouse 2 is a computer mouse featuring a multi-touch acrylic surface for scrolling. The mouse features a lithium-ion rechargeable battery and Lightning connector for charging and pairing."
    },
    {
        "id": 106,
        "name": "Printer",
        "price": 235.0,
        "stock": 24,
        "icon": "fx",
        "currencyCode": "EUR",
        "orderQuantity": 1,
        "description": "Affordable printer providing you with the optimal way to take care of all your printing needs."
    },
    {
        "id": 107,
        "name": "Phone 11",
        "price": 835.0,
        "stock": 45,
        "icon": "iphone",
        "currencyCode": "EUR",
        "orderQuantity": 8,
        "description": "The Phone 11 dimensions are 150.9mm x 75.7mm x 8.3mm (H x W x D). It weighs about 194 grams (6.84 ounces)."
    },
    {
        "id": 108,
        "name": "Phone 3a",
        "price": 299.0,
        "stock": 54,
        "icon": "desktop-mobile",
        "currencyCode": "EUR",
        "orderQuantity": 7,
        "description": "At 5.6 inches, the display is proportionate to the relatively small body of the phone."
    },
    {
        "id": 109,
        "name": "Game Console 4",
        "price": 330.0,
        "stock": 94,
        "icon": "video",
        "currencyCode": "EUR",
        "orderQuantity": 1,
        "description": "This is the fourth home video game console compatible with all gaming systems."
    },
    {
        "id": 110,
        "name": "Monitor",
        "price": 630.0,
        "stock": 20,
        "icon": "sys-monitor",
        "currencyCode": "EUR",
        "orderQuantity": 3,
        "description": "34'' Monitor, Display with stand Height adjustable (115 mm), tiltable (-5° to 21°), rotatable (-30° to 30°) Security slot (cable lock sold separately), anti-theft slot for locking to stand (for display). Includes: DisplayPort cable, HDMI cable, Power cable, Stand, USB 3.0 Type-A to Type-B cable, USB-C cable."
    }
];
    ```



### Add Luigi to index.html


In this step, you will let Luigi take control of the `index.html` file - the entry point for your app.

1. Go to `react-core-mf/public/index.html` and change its content to:

    ```HTML
    <!DOCTYPE html>
    <html>
      <head>
        <title>Luigi</title>
        <meta
          name="viewport"
          content="width=device-width, user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1"
        />
        <link rel="stylesheet" href="/luigi-core/luigi.css" />
      </head>

      <body>
        <noscript>You need to enable JavaScript to run this app.</noscript>
        <script src="/luigi-core/luigi.js"></script>
        <script src="/luigi-config.js"></script>
      </body>
    </html>
    ```


### Create micro-frontends template


In this step, you will create another HTML file which will serve as a template for React to create the React micro-frontends.

1. Go to `react-core-mf/public` and create a new file called `sampleapp.html`. Paste this code into the file:

    ```HTML
      <!DOCTYPE html>
      <html lang="en">
        <head>
          <meta charset="utf-8" />
          <link rel="icon" href="%PUBLIC_URL%/favicon.ico" />
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <title>React App</title>
        </head>
        <body>
          <noscript>You need to enable JavaScript to run this app.</noscript>
          <div id="root"></div>
        </body>
      </html>
    ```



### Configure webpack


In this step, you will configure `webpack` and adjust dependencies in order to make it easier to develop and build the app.

1. Go to `react-core-mf/webpack.config.js`. Note that by default the `webpack-dev-server` will redirect all requests to `index.html` which is the app's main entry point where the Luigi Core library is injected. However, since the micro-frontends are also built in the same environment, it's necessary to host the root React micro-frontend app inside another file, in this case named `sampleapp.html`. This has been done in lines 40-16 and a similar approach can be chosen with any other frontend framework:

2. In `webpack.config.js` around line 80, add a Webpack rule to allow importing CSS in the React project:

    ```Javascript
      module: {
        rules: [
          {
            test: /\.js$/,
            exclude: /node_modules/,
            use: {
              loader: 'babel-loader',
              options: {
                presets: [
                  ['@babel/preset-env', { targets: 'defaults' }],
                  ['@babel/preset-react', { runtime: 'automatic' }]
                ]
              }
            }
          },
          /// <---Add this rule -->
          {
            test: /\.(css)$/,
            use: ['style-loader', 'css-loader'],
          },
          /// <----->
        ]
      },
    ```


### Configure Luigi for "Home" node


With the help of simple parameters pertaining to [navigation](https://docs.luigi-project.io/docs/navigation-parameters-reference) and [general settings](https://docs.luigi-project.io/docs/general-settings), you will create your first "Home" navigation node and make your application responsive.

These are the Luigi navigation parameters you will use:

  - `pathSegment` - text segment added to the URL
  - `label` - the name of the node displayed in the navigation
  - `icon` - a SAP icon shown next to the label
  - `viewUrl`- the URL of your micro-frontend


1. Go to the file `react-core-mf/public/luigi-config.js`. This is where you can find the Luigi configuration. Copy and paste this code inside it:

    ```JavaScript
   
        Luigi.setConfig({
          navigation: {
            nodes: () => [
              {
                pathSegment: "home",
                label: "Home",
                icon: "home",
                viewUrl: "/sampleapp.html#/microfrontend/home",
              },
            ],
          },
          settings: {
            header: { title: "Luigi React App"},
            responsiveNavigation: "simpleMobileOnly",
            customTranslationImplementation: myTranslationProvider,
          },
          lifecycleHooks: {
            luigiAfterInit: () => {
              Luigi.i18n().setCurrentLocale(defaultLocale);
            },
          },
          communication: {
            customMessagesListeners: {
              "set-language": (msg) => {
                Luigi.i18n().setCurrentLocale(msg.locale);
              },
            },
          },
        });
        

    var defaultLocale = "en-US";
    function myTranslationProvider() {
      var dict = {
        "en-US": { PRODUCTS: "Products", ORDERHISTORY: "Order History" },
      };
      return {
        getTranslation: function (label, interpolation, locale) {
          const local = locale || Luigi.i18n().getCurrentLocale() || defaultLocale
          return (
            dict[local][label] || label
          );
        },
      };
    }
    ```


### Configure router for "Home" view


In this step, you will make changes to the entry point `index.js` for the React app. You will create a new view called `home.js` in `react-core-mf/src/views`, configure the router for this view, and import the Luigi Client. You will also create a file called `language.js` which will be useful for the next tutorials dealing with localization.

1. Open `react-core-mf/src/index.js` and change its content to:

    ```JavaScript
    import React, { useState, useEffect } from 'react';
    import { createRoot } from 'react-dom/client';
    import { HashRouter as Router, Routes, Route } from 'react-router-dom';
    import Home from './views/home.js';
    import { dict } from './language.js';
    import { addInitListener, addContextUpdateListener, removeContextUpdateListener, removeInitListener, uxManager} from '@luigi-project/client';
    import { ThemeProvider } from "@ui5/webcomponents-react";

    const container = document.getElementById('root');
    const root = createRoot(container);

    const App = () => {
      const [currentLocale, setCurrentLocale] = useState('en-US');
      const [initListener, setInitListener] = useState(null);
      const [contextUpdateListener, setContextUpdateListener] = useState(null);

      useEffect(() => {
        const updateCurrentLanguage = () => {
          setCurrentLocale(uxManager().getCurrentLocale())
        }

        setInitListener(
          addInitListener(() => {
            console.log("Luigi Client initialized.");
            // update current language upon Luigi Client initialization
            updateCurrentLanguage();
          })
        );

        setContextUpdateListener(
          addContextUpdateListener(() => {
            // update current language upon Luigi Client context update event
            updateCurrentLanguage();
          })
        );

        return function cleanup() {
          removeContextUpdateListener(contextUpdateListener);
          removeInitListener(initListener);
        };
      }, []);

      return (
        <ThemeProvider>
          <React.StrictMode>
            <Router basename="microfrontend">
              <Routes>
                <Route path="/home" element={<Home localeDict={dict[currentLocale]} currentLocale={currentLocale} />} />
              </Routes>
            </Router>
          </React.StrictMode>
        </ThemeProvider>
      );
    };

    root.render(<App />);
    ```

  2. Next, go to the `react-core-mf/src/views` directory created in step 6 of the [previous tutorial](luigi-app-basic-setup). Find the file called `home.js` and paste the following code into it:

    ```JavaScript
    import React, { useState, useEffect } from 'react';
    import "fundamental-styles/dist/fundamental-styles.css";
    import { addInitListener, addContextUpdateListener, removeContextUpdateListener, removeInitListener, sendCustomMessage } from "@luigi-project/client";
    import { Grid, Panel, Select, Option } from "@ui5/webcomponents-react";

    const Home = (props) => {
      const [options] = useState([{ key: 'en-US', text: 'en-US' }]);

      function onChangeValue(event) {
        sendCustomMessage({
          id: "set-language",
          locale: event.detail.selectedOption.innerText,
        });
      }

      return (
        <Grid position="Center" defaultIndent="XL1 L1 M1 S1" defaultSpan="XL10 L10 M10 S10">
          <Panel headerText={props.localeDict.WELCOME_LUIGI} headerLevel="H3">
            <Select onChange={onChangeValue}>
              {options.map((language) => (
                <Option key={language.key}>{language.text}</Option>
              ))}
            </Select>
          </Panel>
        </Grid>
      );
    };

    export default Home;
    ```

3. Create a new file in `react-core-mf/src` called `language.js` with the following content:

    ```JavaScript
    export const dict = {
      "en-US": {
        ITEMS: "Products",
        STOCKS: "Stocks",
        SELECTLANGUAGE: "Please select a language",
        PRICE: "Price",
        WELCOME_LUIGI: "Welcome to Luigi - a micro-frontend framework",
        DESCRIPTION: "Description",
        PRODUCTADDED: "Product has been added to cart",
        AVAILABLE: "Available",
        AVAILABLEQUANT: "Available quantity: ",
        ADDTOCART: "Add to cart",
        BACK: "Back",
        OUTOFSTOCK: "Out of stock",
      },
    };
    ```


### Add "Products" view to Luigi app


In this step, you will add a navigation node in Luigi for the "Products" micro-frontend.

1. Open `react-core-mf/public/luigi-config.js`

2. Add a new "Products" node to your navigation:

    ```JavaScript
    navigation: {
        nodes: () => [
          {
            pathSegment: "home",
            label: "Home",
            icon: "home",
            viewUrl: "/sampleapp.html#/microfrontend/home",
            //<---Add the section below to the config file--->
            children: [
              {
                pathSegment: "products",
                label: "Products",
                icon: "product",
                viewUrl: "/sampleapp.html#/microfrontend/products",
              }
            ],
            // <------>
          },
        ],
      },
    ```

3. Next, rename the file `sample1.js` in `react-core-mf/src/views` to `products.js` and paste following code into it:

    ```JavaScript
    import React, { useEffect, useState } from 'react';
    import "../../node_modules/fundamental-styles/dist/fundamental-styles.css";
    import "@ui5/webcomponents-icons/dist/AllIcons.js";
    import { Grid, List, StandardListItem } from "@ui5/webcomponents-react";
    import { linkManager } from "@luigi-project/client";
    import { ProductCollection } from "../assets/products.js";

    const Products = (props) => {
      const [listItems, setListItems] = useState([]);

      useEffect(() => {
        const tempList = [];
        ProductCollection.forEach((product) => {
          tempList.push(
            <StandardListItem id={product.id} key={product.id} additionalText={product.price + " " + product.currencyCode} additionalTextState="Information" description={product.description} growing="None" headerText={product.orderQuantity} icon={product.icon} type="Active" mode="None" onItemClick={() => handleItemClick(product.id)}>
              <p>{product.name}</p>
            </StandardListItem>
          );
          setListItems(tempList);
        });
      }, [])

      // navigates to productDetail microfrontend through Luigi Client linkManager API
      function handleItemClick(event) {
        linkManager().withParams({ root: "products" });
        linkManager().navigate(
          "/home/products/" + event.detail.item.id.toString()
        );
      };

      return (
        <Grid position="Center" defaultIndent="XL1 L1 M1 S1" defaultSpan="XL10 L10 M10 S10">
          <List headerText={props.localeDict.ITEMS + ": " + ProductCollection.length} onItemClick={handleItemClick}>
            {listItems}
          </List>
        </Grid>
      );
    };
    export default Products;
    ```

4. Add the routing module to the `index.js` by adding the following lines to the file:

    ```JavaScript
    //<---Around line 8, paste this line: --->:
    import Products from "./views/products.js";
    //<------>
   ...
    <Router basename="microfrontend">
      <Routes>
        <Route path="/home" render={(props) => <Home {...props} localeDict={dict[this.state.currentLocale]} currentLocale={this.state.currentLocale} />} />
        //<---Around line 45, paste this line: --->
        <Route path="/products" element={<Products localeDict={dict[currentLocale]} />} />
        //<------>
      </Routes>
    </Router>
    ```


### Add "Product Detail" view to Luigi app


In this step, you will add the `ProductDetail` view to the app. You will be able to show details for each product via a Luigi [dynamic parameter](https://docs.luigi-project.io/docs/navigation-advanced?section=dynamically-changeable-paths), in this case named `:id`.

  1. In `react-core-mf/public/luigi-config.js` add a child node `:id` to the `products` node:

    ```js
          children: [
                      {
                          pathSegment: "products",
                          label: "Products",
                          icon: "product",
                          viewUrl: "/sampleapp.html#/microfrontend/products",
                          //<---Paste the section below to your config:--->
                          keepSelectedForChildren: true,
                          children: [{
                              pathSegment: ':id',
                              viewUrl: '/sampleapp.html#/microfrontend/productDetail/:id',
                              context: { id: ':id' }
                          }]
                          //<------>
                      },
    ```

2. Next, create a new file in `react-core-mf/src/views` named `productDetail.js` and paste following content into it:

    ```js
      import React, { useEffect, useState, useRef } from 'react';
      import "../../node_modules/fundamental-styles/dist/fundamental-styles.css";
      import "@ui5/webcomponents-icons/dist/AllIcons.js";
      import {
          Grid, ObjectPage, Label, DynamicPageHeader, DynamicPageTitle, ObjectStatus, FlexBox, Button, Toast, ObjectPageSection, FormItem, Form, Text, Bar
      } from "@ui5/webcomponents-react";
      import { linkManager, getContext } from "@luigi-project/client";
      import { ProductCollection } from "../assets/products.js";

      const ProductDetail = (props) => {
          const [currentProduct, setCurrentProduct] = useState({});
          const [availability, setAvailability] = useState({
              state: "Warning",
              text: props.localeDict.OUTOFSTOCK
          });
          const toast = useRef(null);

          useEffect(() => {
              setProductAndAvailability();
          }, []);

          function setProductAndAvailability() {
              // get id from Luigi Client getContext API
              const id = getContext().id;

              let product = ProductCollection.find(
                  (product) => product.id.toString() === id
              ) || ProductCollection[0]

              setCurrentProduct(product);
              currentProduct.stock ? setAvailability({ state: "Success", text: props.localeDict.AVAILABLE }) : ""
          }

          // Use Luigi Client linkManager API to navigate to the previous microfrontend
          function navBack() {
              // checks if there is a previous view in history
              if (linkManager().hasBack()) {
                  // navigates to the previously opened microfrontend
                  linkManager().goBack();
              } else {
                  // navigates to the products page directly
                  linkManager().navigate("/home/products");
              }
          };

          return (
              <Grid position="Center" defaultIndent="XL1 L1 M1 S1" defaultSpan="XL10 L10 M10 S10">
                  <ObjectPage
                      headerContent={
                          <DynamicPageHeader>
                              <FlexBox alignItems="Center" wrap="Wrap">
                                  <FlexBox direction="Column" style={{ padding: "10px" }}>
                                      <Label>
                                          {props.localeDict.AVAILABLEQUANT + currentProduct.stock}
                                      </Label>
                                  </FlexBox>
                              </FlexBox>
                          </DynamicPageHeader>
                      }
                      footer={
                          <Bar design="FloatingFooter"
                              endContent={
                                  <>
                                      <Button design="Emphasized" onClick={() => { toast.current.show() }}>
                                          {props.localeDict.ADDTOCART}
                                      </Button>
                                      <Button design="Default" onClick={navBack}>
                                          {props.localeDict.BACK}
                                      </Button>
                                  </>
                              }
                          />
                      }
                      headerContentPinnable
                      headerTitle={
                          <DynamicPageTitle
                              actions={
                                  <>
                                      <Toast ref={toast}>
                                          {props.localeDict.PRODUCTADDED}
                                      </Toast>
                                      <Button design="Emphasized" onClick={() => { toast.current.show() }}>
                                          {props.localeDict.ADDTOCART}
                                      </Button>
                                      <Button onClick={navBack}>
                                          {props.localeDict.BACK}
                                      </Button>
                                  </>
                              }
                              header={currentProduct.name}
                          >
                              <ObjectStatus state={availability.state}>
                                  {availability.text}
                              </ObjectStatus>
                          </DynamicPageTitle>
                      }
                      onSelectedSectionChange={function noRefCheck() { }}
                      selectedSectionId="details"
                      showHideHeaderButton
                      style={{
                          height: "700px",
                      }}
                  >
                      <ObjectPageSection aria-label="Details" id="details" titleText="Details">
                          <Form columnsL={2} columnsM={2} columnsXL={3} labelSpanL={1} labelSpanM={1} labelSpanXL={1}>
                              <FormItem label="Name">
                                  <Text>{currentProduct.name}</Text>
                              </FormItem>
                              <FormItem label={props.localeDict.DESCRIPTION}>
                                  <Text>{currentProduct.description}</Text>
                              </FormItem>
                              <FormItem label={props.localeDict.PRICE}>
                                  <Text>
                                      {currentProduct.price + " " + currentProduct.currencyCode}
                                  </Text>
                              </FormItem>
                          </Form>
                      </ObjectPageSection>
                  </ObjectPage>
              </Grid>
          );
      };

      export default ProductDetail;

    ```

2. Add the route configuration to the `index.js` by pasting below code into it:

    ```JavaScript
        //<---Around line 9, paste this line:--->
        import ProductDetail from './views/productDetail';
        //<------>
       ...
        <Router basename="microfrontend">
          <Routes>
            <Route path="/home" render={(props) => <Home {...props} localeDict={dict[this.state.currentLocale]} currentLocale={this.state.currentLocale} />} />
            <Route path="/products" render={(props) => <Products {...props} localeDict={dict[this.state.currentLocale]} />} />
            //<---Around line 50, paste this line:--->
            <Route path="/productDetail/:id" element={<ProductDetail localeDict={dict[currentLocale]} />} />  
            //<------>    
          </Routes>
        </Router>
    ```


### Run your core app


In this step, you can check if your core app is configured correctly so far by running it locally.

1. Open a terminal/command prompt window. Navigate to the `react-core-mf` folder.

2. Input `npm start`. Your application should be up and running at `http://localhost:3000/`. You should be able to see the homepage and "Products" view. You can also navigate toward the `ProductDetail` view via "Products".




---
