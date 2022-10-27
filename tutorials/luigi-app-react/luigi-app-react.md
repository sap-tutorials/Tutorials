---
title: Build Luigi App with React
description: Create a mock shopping application with React and configure it using Luigi.
auto_validation: true
time: 25
tags: [ tutorial>beginner, programming-tool>javascript]
primary_tag: topic>user-interface
---

## Details
### You will learn
  - How to add content to your React web shopping app
  - How to use Luigi to configure navigation
  - How to create simple micro-frontends using React
---

[ACCORDION-BEGIN [Step 1:](Add a file with product data)]

In this step, you will create a file with information about the products on sale in your shopping app.

In a real life implementation, this data would be provided by an external service/API. But for simplicity, you will create a `.json` file containing dummy data. This file will provide the displayed data in the micro-frontend.

1. Navigate to `ui5-mf/uimodule/webapp/model` and create a `products.json` file with the following content:

    ```JSON
    {
      "ProductCollection": [{
          "id": 101,
          "name": "Logitech Mouse",
          "price": 45.0,
          "stock": 80,
          "icon": "product",
          "currencyCode": "EUR",
          "orderQuantity": 2,
          "description": "LIGHTSPEED Wireless Gaming Mouse with HERO Sensor"
        },
        {
          "id": 102,
          "name": "Logitech Keyboard",
          "price": 50.0,
          "stock": 22,
          "icon": "product",
          "currencyCode": "EUR",
          "orderQuantity": 1,
          "description": "A physical keyboard that uses an individual spring and switch for each key. Today, only premium keyboards are built with key switches; however, they were also used in the past, such as in the Model M keyboard from IBM, which used buckling spring switches"
        },
        {
          "id": 103,
          "name": "HP Optical Mouse",
          "price": 35.0,
          "stock": 4,
          "icon": "product",
          "currencyCode": "EUR",
          "orderQuantity": 2,
          "description": "Utilizing the latest optical sensing technology, the HP USB Optical Scroll Mouse records precise motion."
        },
        {
          "id": 104,
          "name": "MacBook Pro",
          "price": 1299.0,
          "stock": 11,
          "icon": "laptop",
          "currencyCode": "EUR",
          "orderQuantity": 3,
          "description": "It features a touch-sensitive OLED display strip located in place of the function keys, a Touch ID sensor integrated with the power button, a butterfly mechanism keyboard similar to the MacBook, and four USB-C ports that also serve as Thunderbolt 3 ports."
        },
        {
          "id": 105,
          "name": "Magic Mouse",
          "price": 40.0,
          "stock": 20,
          "icon": "product",
          "currencyCode": "EUR",
          "orderQuantity": 6,
          "description": "The Magic Mouse 2 (Apple Magic Mouse 2), is a computer mouse developed and released by Apple Inc. It features a multi-touch acrylic surface for scrolling. ... The mouse features a lithium-ion rechargeable battery and Lightning connector for charging and pairing."
        },
        {
          "id": 106,
          "name": "Brother Printer",
          "price": 235.0,
          "stock": 24,
          "icon": "fx",
          "currencyCode": "EUR",
          "orderQuantity": 1,
          "description": "Our affordable, quality machines provide you with the optimal way to take care of all your printing needs. Shop for the right printer, all-in-one, or fax machine for your home or home office today."
        },
        {
          "id": 107,
          "name": "iPhone 11",
          "price": 835.0,
          "stock": 45,
          "icon": "iphone",
          "currencyCode": "EUR",
          "orderQuantity": 8,
          "description": "The iPhone 11 dimensions are 150.9mm x 75.7mm x 8.3mm (H x W x D). It weighs about 194 grams (6.84 ounces).It features a 6.1-inch all-screen LCD display and is powered by Apple new A13 bionic chip with Third-Generation Neural Engine."
        },
        {
          "id": 108,
          "name": "Google Pixel 3a",
          "price": 299.0,
          "stock": 54,
          "icon": "desktop-mobile",
          "currencyCode": "EUR",
          "orderQuantity": 7,
          "description": "At 5.6 inches, the Google Pixel 3a display is proportionate to the relatively small body of the phone – that is to say, it is rather small. The display is Full HD+ and OLED, with a resolution of 2220 x 1080, and because of the relatively small screen size the pixels per inch count is rather high at 441."
        },
        {
          "id": 109,
          "name": "PlayStation 4",
          "price": 330.0,
          "stock": 94,
          "icon": "video",
          "currencyCode": "EUR",
          "orderQuantity": 1,
          "description": "PS4 is the fourth home video game console produced by Sony Computer Entertainment and is compatible with the PlayStation 3. It was officially announced at a press conference on February 20, 2013 and launched on November 15, 2013."
        },
        {
          "id": 110,
          "name": "Dell Monitor",
          "price": 630.0,
          "stock": 20,
          "icon": "sys-monitor",
          "currencyCode": "EUR",
          "orderQuantity": 3,
          "description": "34'' U3419W Monitor, Display with stand Height adjustable (115 mm), tiltable (-5° to 21°), rotatable (-30° to 30°) Security slot (cable lock sold separately), anti-theft slot for locking to stand (for display). Includes: DisplayPort cable, HDMI cable, Power cable, Stand, USB 3.0 Type-A to Type-B cable, USB-C cable"
        }
      ]
    }
    ```


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Add Luigi to index.html)]

In this step, you will let Luigi take control of the `index.hmtl` file - the entry point for your app.

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

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create micro-frontends template)]

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

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Configure webpack)]

In this step, we configure `webpack` and adjust dependencies in order to make it easier to develop and build the app.

1. Go to `react-core-mf/config/webpack.config.js`

2. Find the following line from the file and [comment it out](https://www.w3schools.com/js/js_comments.asp) by surrounding it with `/*` or `//` tags:

    ```JavaScript
      // Around line 19
      // const ModuleScopePlugin = require('react-dev-utils/ModuleScopePlugin');
    ```

3. In the second occurrence, comment out this whole entry as well:

    ```JavaScript
      //Around line 348
      /*
          new ModuleScopePlugin(paths.appSrc, [
            paths.appPackageJson,
            reactRefreshOverlayEntry,
          ]),*/
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure Luigi for "Home" node)]

With the help of simple parameters pertaining to [navigation](https://docs.luigi-project.io/docs/navigation-parameters-reference) and [general settings](https://docs.luigi-project.io/docs/general-settings), you will create your first "Home" navigation node and make your application responsive.

These are the Luigi navigation parameters you will use:

  - `pathSegment` - text segment added to the URL
  - `label` - the name of the node displayed in the navigation
  - `icon` - a SAP icon shown next to the label
  - `viewUrl`- the URL of your micro-frontend


1. Delete the folder `react-core-mf/src/luigi-config`. (This step is not required.)

2. Go to the file `react-core-mf/public/luigi-config.js`. This is where you can find the Luigi configuration. Copy and paste this code:

    ```JavaScript
    !(function (e) {
      var t = {};
      function n(r) {
        if (t[r]) return t[r].exports;
        var o = (t[r] = { i: r, l: !1, exports: {} });
        return e[r].call(o.exports, o, o.exports, n), (o.l = !0), o.exports;
      }
      (n.m = e),
        (n.c = t),
        (n.d = function (e, t, r) {
          n.o(e, t) || Object.defineProperty(e, t, { enumerable: !0, get: r });
        }),
        (n.r = function (e) {
          "undefined" != typeof Symbol &&
            Symbol.toStringTag &&
            Object.defineProperty(e, Symbol.toStringTag, { value: "Module" }),
            Object.defineProperty(e, "__esModule", { value: !0 });
        }),
        (n.t = function (e, t) {
          if ((1 & t && (e = n(e)), 8 & t)) return e;
          if (4 & t && "object" == typeof e && e && e.__esModule) return e;
          var r = Object.create(null);
          if (
            (n.r(r),
            Object.defineProperty(r, "default", { enumerable: !0, value: e }),
            2 & t && "string" != typeof e)
          )
            for (var o in e)
              n.d(
                r,
                o,
                function (t) {
                  return e[t];
                }.bind(null, o)
              );
          return r;
        }),
        (n.n = function (e) {
          var t =
            e && e.__esModule
              ? function () {
                  return e.default;
                }
              : function () {
                  return e;
                };
          return n.d(t, "a", t), t;
        }),
        (n.o = function (e, t) {
          return Object.prototype.hasOwnProperty.call(e, t);
        }),
        (n.p = ""),
        n((n.s = 0));
    })([
      function (e, t) {
        Luigi.setConfig({
          navigation: {
            nodes: () => [
              {
                pathSegment: "home",
                label: "Home",
                icon: "home",
                viewUrl: "/sampleapp.html#/home",
              },
            ],
          },
          settings: {
            header: { title: "Luigi React App", logo: "/logo192.png" },
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
      },
    ]);

    var defaultLocale = "en-US";
    function myTranslationProvider() {
      var dict = {
        "en-US": { PRODUCTS: "Products", ORDERHISTORY: "Order History" },
      };
      return {
        getTranslation: function (label, interpolation, locale) {
          return (
            dict[locale || Luigi.i18n().getCurrentLocale() || defaultLocale][
              label
            ] || label
          );
        },
      };
    }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure router for "Home" view)]

In this step, you will make changes to the entry point `index.js` for the React app. You will create a new view called `home.js` in `react-core-mf/src/views`, configure the router for this view, and import the Luigi Client. You will also create a file called `language.js` which will be useful for the next tutorials dealing with localization.

1. Open `react-core-mf/src/index.js` and change its content to:

    ```JavaScript
    import React, { Component } from "react";
    import { render } from "react-dom";
    import { BrowserRouter, Route } from "react-router-dom";
    import Home from "./views/home.js";
    import { addInitListener, addContextUpdateListener, uxManager} from "@luigi-project/client";
    import { dict } from "./language.js";
    import "./index.css";

    class App extends Component {
      constructor(props) {
        super(props);
        this.state = { currentLocale: "en-US" };
        const updateCurrentLanguage = () => {
          this.setState({
            currentLocale: uxManager().getCurrentLocale(),
          });
        };

        addInitListener(() => {
          console.log("Luigi Client initialized.");
          updateCurrentLanguage();
        });

        addContextUpdateListener(() => {
          updateCurrentLanguage();
        });
      }

      render() {
        return (
          <BrowserRouter basename={`sampleapp.html#`}>
            <Route path="/home" render={(props) => (<Home {...props} localeDict={dict[this.state.currentLocale]} currentLocale={this.state.currentLocale}/>)}/>
          </BrowserRouter>
        );
      }
    }

    render(<App />, document.getElementById("root"));
    ```

  2. Next, go to the `react-core-mf/src/views` directory created in step 6 of the [previous tutorial](luigi-app-basic-setup). Create a file called `home.js` and paste the following code into it:

    ```JavaScript
    import React, { Component, useState } from "react";
    import "../../node_modules/fundamental-styles/dist/fundamental-styles.css";
    import {addInitListener, addContextUpdateListener,removeContextUpdateListener,removeInitListener,sendCustomMessage} from "@luigi-project/client";
    import {Grid, Panel,Select,Option} from "@ui5/webcomponents-react";

    export default class Home extends Component {
      constructor(props) {
        super(props);
        this.locale = null;
        this.initListener = null;
        this.contextUpdateListener = null;
        this.locale = props.currentLocale;
        this.setLocale = null;
        this.options = [{ key: "en-US", text: "en-US" }];
      }

      componentDidMount() {
        this.initListener = addInitListener((initialContext) => {
          this.setState({
            message: "Luigi Client initialized.",
          });
        });

        this.contextUpdateListener = addContextUpdateListener(
          (updatedContext) => {
            this.setState({
              message: "Luigi Client updated.",
            });
          }
        );
      }

      componentWillUnmount() {
        removeContextUpdateListener(this.contextUpdateListener);
        removeInitListener(this.initListener);
      }

      onChangeValue(event) {
        this.locale = event.detail.selectedOption.innerText;
        sendCustomMessage({
          id: "set-language",
          locale: event.detail.selectedOption.innerText,
        });
      }

      render() {
        return (
          <Grid position="Center" defaultIndent="XL1 L1 M1 S1" defaultSpan="XL10 L10 M10 S10">
            <Panel headerText={this.props.localeDict.WELCOME_LUIGI} headerLevel="H3">
              <Select onChange={this.onChangeValue}>
                {this.options.map((language) => (
                  <Option key={language.key}>{language.text}</Option>
                ))}
              </Select>
            </Panel>
          </Grid>
        );
      }
    }
    ```

3. Create a new file in `react-core-mf/src/` called `language.js` with following content:

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

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add "Products" view to Luigi app)]

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
            viewUrl: "/sampleapp.html#/home",
            //ADD THIS SECTION TO THE CONFIG FILE
            children: [
              {
                pathSegment: "products",
                label: "Products",
                icon: "product",
                viewUrl: "/sampleapp.html#/products",
              }
            ],
            //UNTIL HERE
          },
        ],
      },
    ```

3. Next, create the `products.js` file in `react-core-mf/src/views` and past following code into it:

    ```JavaScript
    import React, { Component } from "react";
    import "../../node_modules/fundamental-styles/dist/fundamental-styles.css";
    import { ProductCollection } from "../../../ui5-mf/uimodule/webapp/model/products.json";
    import { Grid, List, StandardListItem } from "@ui5/webcomponents-react";
    import { linkManager } from "@luigi-project/client";
    import "@ui5/webcomponents-icons/dist/AllIcons.js";

    export default class Product extends Component {
      constructor(props) {
        super(props);
        this.toast = React.createRef();
      }

      render() {
        const handleItemClick = (event) => {
          linkManager().withParams({ root: "products" });
          linkManager().navigate(
            "/home/products/" + event.detail.item.id.toString()
          );
        };

        const listItems = [];
        ProductCollection.forEach((product) => {
          listItems.push(
            <StandardListItem id={product.id} key={product.id} additionalText={product.price + " " + product.currencyCode} additionalTextState="Information" description={product.description} growing="None" headerText={product.orderQuantity} icon={product.icon} type="Active" mode="None" onItemClick={() => handleItemClick(product.id)}>
              <p onClick={() => handleChildClick(product.id)}>{product.name}</p>
            </StandardListItem>
          );
        });

        return (
          <Grid position="Center"  defaultIndent="XL1 L1 M1 S1" defaultSpan="XL10 L10 M10 S10">
            <List headerText={this.props.localeDict.ITEMS + ": " + ProductCollection.length} onItemClick={handleItemClick}>
              {listItems}
            </List>
          </Grid>
        );
      }
    }
    ```

4. Add the routing module to the `index.js` by adding the following lines to the file:

    ```JavaScript
    //AROUND LINE 4 PASTE THIS LINE:
    import Products from "./views/products.js";

    //AROUND LINE 35 PASTE THIS LINE:
    <BrowserRouter basename={`sampleapp.html#`}>
        <Route path="/home" render={(props) => <Home {...props} localeDict={dict[this.state.currentLocale]} currentLocale={this.state.currentLocale} />} />
      //THIS LINE HAS TO BE ADDED:
        <Route path="/products" render={(props) => <Products {...props} localeDict={dict[this.state.currentLocale]} />} />
    </BrowserRouter>
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Add "Product Detail" view to Luigi app)]

In this step, you will add the `ProductDetail.js` view to the app. You will be able to show details for each product via a Luigi [dynamic parameter](https://docs.luigi-project.io/docs/navigation-advanced?section=dynamically-changeable-paths), in this case named `:id`.

  1. In `react-core-mf/public/luigi-config.js` add a child node `:id` to the `products` node:

    ```js
          children: [
                      {
                          pathSegment: "products",
                          label: "Products",
                          icon: "product",
                          viewUrl: "/sampleapp.html#/products",
                          // PASTE THIS SECTION
                          keepSelectedForChildren: true,
                          children: [{
                              pathSegment: ':id',
                              viewUrl: '/sampleapp.html#/productDetail/:id',
                              context: { id: ':id' }
                          }]
                          //UNTIL HERE
                      },
    ```

2. Next create a new file in `react-core-mf/src/views` named `productDetail.js` and paste following content into it:

    ```js
    import React, { Component, useRef, useState } from "react";
    import "../../node_modules/fundamental-styles/dist/fundamental-styles.css";
    import { ProductCollection } from "../../../ui5-mf/uimodule/webapp/model/products.json";
    import { Grid,ObjectPage,Label,DynamicPageHeader,DynamicPageTitle,ObjectStatus,FlexBox,Button,Toast,ObjectPageSection,FormItem,Form,Text,ObjectPageSubSection,Bar } from "@ui5/webcomponents-react";
    import { linkManager } from "@luigi-project/client";
    import "@ui5/webcomponents-icons/dist/AllIcons.js";
    import { render } from "react-dom";

    export default class ProductDetail extends Component {
      constructor(props) {
        super(props);
        this.toast = React.createRef();
      }

      render() {
        const id = linkManager().currentContext.context.id;
        let product = ProductCollection.find(
          (product) => product.id.toString() === id
        );

        if (!product) {
          product = ProductCollection[0];
        }

        const showToast = () => {
          this.toast.current.show();
        };

        const navBack = () => {
          if (linkManager().hasBack()) {
            linkManager().goBack();
          } else {
            linkManager().navigate("/home/products");
          }
        };

        const icon = () => {
          return (
            <ui5-icon
              class="samples-margin"
              name={product.icon}
              style={{ width: "3rem", height: "3rem" }}
            ></ui5-icon>
          );
        };

        let availability = {
          state: "Warning",
          text: this.props.localeDict.OUTOFSTOCK,
        };

        if (product.stock) {
          availability.state = "Success";
          availability.text = this.props.localeDict.AVAILABLE;
        }

        return (
          <Grid position="Center" defaultIndent="XL1 L1 M1 S1" defaultSpan="XL10 L10 M10 S10">
            <ObjectPage
              headerContent={
                <DynamicPageHeader>
                  <FlexBox alignItems="Center" wrap="Wrap">
                    <FlexBox direction="Column" style={{ padding: "10px" }}>
                      <Label>
                        {this.props.localeDict.AVAILABLEQUANT +
                          product.stock}
                      </Label>
                    </FlexBox>
                  </FlexBox>
                </DynamicPageHeader>
              }
              footer={
                <Bar design="FloatingFooter"
                  endContent={
                    <>
                      <Button design="Emphasized" onClick={showToast}>
                        {this.props.localeDict.ADDTOCART}
                      </Button>
                      <Button design="Default" onClick={navBack}>
                        {this.props.localeDict.BACK}
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
                      <Toast ref={this.toast}>
                        {this.props.localeDict.PRODUCTADDED}
                      </Toast>
                      <Button design="Emphasized" onClick={showToast}>
                        {this.props.localeDict.ADDTOCART}
                      </Button>
                      <Button onClick={navBack}>
                        {this.props.localeDict.BACK}
                      </Button>
                    </>
                  }
                  header={product.name}
                >
                  <ObjectStatus state={availability.state}>
                    {availability.text}
                  </ObjectStatus>
                </DynamicPageTitle>
              }
              image={icon()}
              imageShapeCircle
              onSelectedSectionChange={function noRefCheck() {}}
              onSelectedSectionChanged={null}
              selectedSectionId="details"
              showHideHeaderButton
              style={{
                height: "700px",
              }}
            >
              <ObjectPageSection aria-label="Details" id="details" titleText="Details">
                <ObjectPageSubSection aria-label="Details" id="details-subsection" titleText="Details">
                  <Form columnsL={2} columnsM={2} columnsXL={3} labelSpanL={1} labelSpanM={1} labelSpanXL={1}>
                    <FormItem label="Name">
                      <Text>{product.name}</Text>
                    </FormItem>
                    <FormItem label={this.props.localeDict.DESCRIPTION}>
                      <Text>{product.description}</Text>
                    </FormItem>
                    <FormItem label={this.props.localeDict.PRICE}>
                      <Text>
                        {product.price + " " + product.currencyCode}
                      </Text>
                    </FormItem>
                  </Form>
                </ObjectPageSubSection>
              </ObjectPageSection>
            </ObjectPage>
          </Grid>
        );
      }
    }
    ```

2. Add the route configuration to the `index.js` by pasting below code into it:

    ```JavaScript
      //PASTE THIS AROUND LINE 5
        import ProductDetail from './views/productDetail';
      //PASTE THE MENTIONED LINE AROUND LINE 39
        <BrowserRouter basename={`sampleapp.html#`}>
          <Route path="/home" render={(props) => <Home {...props} localeDict={dict[this.state.currentLocale]} currentLocale={this.state.currentLocale} />} />
          <Route path="/products" render={(props) => <Products {...props} localeDict={dict[this.state.currentLocale]} />} />
          //THIS LINE HAS TO BE ADDED:
          <Route path="/productDetail/:id" render={(props) => <ProductDetail {...props} localeDict={dict[this.state.currentLocale]} />} />
      </BrowserRouter>
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Run your core app)]

In this step, you can check if your core app is configured correctly so far by running it locally.

1. Open a terminal/command prompt window. Navigate to the `react-core-mf` folder.

2. Input `npm start`. Your application should be up and running at `http://localhost:3000/`. You should be able to see the homepage and "Products" view. You can also navigate toward the `ProductDetail` view via "Products".

[VALIDATE_1]
[ACCORDION-END]


---
