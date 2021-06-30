---
title: Build Luigi App with React
description: Create a mock shopping application with React and configure it using Luigi.
auto_validation: true
time: 25
tags: [ tutorial>beginner, topic>javascript]
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

In a real life implementation, this data would be provided by a database. But for simplicity, you will create dummy data in a `.json` file. This file will be used both by the main app and the micro-frontends.

Navigate to `ui5-mf/uimodule/webapp/model` and create a `products.json` file with the following content:

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

[ACCORDION-BEGIN [Step 2: ](Prepare React app)]

This step prepares you for development. In order to be able to use `webpack` and gain full control over your React app, you need to trigger the `npm run eject` command.

1. Navigate to the core app:

    ```Shell
    cd react-core-mf
    ```

2. Trigger eject command. Note that due to the way this command works, it is possible the `npm run eject` fails. If you get an error, you need to commit any changes before running the command. More information can be found [here](https://stackoverflow.com/questions/45671057/how-to-run-eject-in-my-react-app).

    ```Shell
    npm run eject
    ```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add Luigi to index.html)]

In this step, you will let Luigi take control of the `index.hmtl` file - the entry point for your app.

Go to `react-core-mf/public/index.html` and change its content to:

```HTML
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1" />
  <title>Luigi</title>
  <link rel="stylesheet" href="/luigi-core/luigi.css" />
</head>
<body>
<script src="/luigi-core/luigi.js"></script>
<script src="/luigi-config.js"></script>
</body>
</html>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create micro-frontends template)]

In this step, you will create another HTML file which will serve as a template for React to create the React micro-frontends.

Go to `react-core-mf/public` and create a new file called `app.html`. Paste this code into the file:

```HTML
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <title>React App</title>
  </head>
  <body>
    <div id="root"></div>
  </body>
</html>
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Configure webpack)]

In this step, we configure `webpack` and adjust dependencies in order to make it easier to develop and build the app.

1. Go to `react-core-mf/config/webpack-config.js`

2. Around line 27, next to the other similar entries, add:

    ```JavaScript
    const CopyWebpackPlugin = require('copy-webpack-plugin');
    ```

3. Remove the following line from the file:

    ```JavaScript
    const ModuleScopePlugin = require('react-dev-utils/ModuleScopePlugin');
    ```

    In the second occurrence, remove the whole entry as well:

    ```JavaScript
    new ModuleScopePlugin(paths.appSrc, [
      paths.appPackageJson,
      reactRefreshOverlayEntry,
    ]),
    ```

4. Find the `plugins` section around line 510. Replace its content with:

    ```JavaScript
    new CopyWebpackPlugin(
       {
        patterns: [
           {
             context: 'public',
             from: 'index.html',
             to: 'index.html'
           },
           {
             from: 'node_modules/@luigi-project/core',
             to: './luigi-core'
           }
         ]
       },
       {
         ignore: ['.gitkeep', '**/.DS_Store', '**/Thumbs.db'],
         debug: 'warning'
       }
     ),
     new HtmlWebpackPlugin(
         {
           inject: true,
           template: __dirname + '/../public/app.html',
           filename: 'app.html'
         }
     ),
    ```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Create Luigi configuration file)]

In this step, you will create a Luigi [configuration](https://www.youtube.com/watch?v=9hczgxJV1eU&t=3s) file. This is the central point of any Luigi app. It allows you to configure the consistent navigation and many other Luigi features.

1. Go to `react-core-mf/public` and create a file called `luigi-config.js`.

> In a real life implementation, you can give the configuration file any name you want or create more than one file. The only important thing is to use the correct Luigi parameters and syntax, which will be introduced in the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Configure Luigi for "Home" node)]

With the help of simple parameters pertaining to [navigation](https://docs.luigi-project.io/docs/navigation-parameters-reference) and [general settings](https://docs.luigi-project.io/docs/general-settings), you will create your first "Home" navigation node and make your application responsive.

These are the Luigi navigation parameters you will use:

  - `pathSegment` - text segment added to the URL
  - `label` - the name of the node displayed in the navigation
  - `icon` - a SAP icon shown next to the label
  - `viewUrl`- the URL of your micro-frontend

1. Copy and paste this to `luigi-config.js`:

    ```JavaScript
    Luigi.setConfig({
      navigation: {
        nodes: () => [
          {
            pathSegment: 'home',
            label: 'Home',
            icon: 'home',
            viewUrl: '/app.html#/home'
          }
        ]
      },
    ```

2. Add a Luigi `settings:` section directly below. Using the code below, you will configure a header for your page and make your app look better on mobile devices using the `responsiveNavigation` parameter:

    ```JavaScript
    settings: {
      header: {
        title: 'Luigi Application',
        logo: '/logo.png'
      },
      responsiveNavigation: 'simpleMobileOnly'
    }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create "Home" view)]

In this step, you will create your first micro-frontend (a.k.a. view) with React. It is a simple "Home" view with a welcome message.

1. Navigate to `react-core-mf/src` and create a folder called `views`.

2. Create a `Home.jsx` file in `views` with the following content:

    ```XML
    import React from 'react';
    import { LayoutPanel } from 'fundamental-react';

    export const Home = () => {
       return (
           <LayoutPanel>
               <LayoutPanel.Body>
                   <h2>Welcome to Luigi - a micro-frontend framework</h2>
               </LayoutPanel.Body>
           </LayoutPanel>
       );
    }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Configure router for "Home" view)]

In this step, you will make changes to the entry point `index.js` for the React app. You will configure the router for the "Home" view created in the previous step, and import Luigi Client.

Open `react-core-mf/src/index.js` and change its content to:

```XML
import React, { Component } from 'react';
import { render } from 'react-dom';
import { addInitListener } from '@luigi-project/client';
import { BrowserRouter as Router, Route } from 'react-router-dom';
import { Home } from './views/Home.jsx';
import './index.css';

class App extends Component {
 constructor(props) {
   super(props);
   addInitListener(() => {
     console.log('Luigi Client initialized.');
   });
 }
 render() {
   return (
     <Router basename={`/app.html#`}>
       <Route path="/home" component={Home} />
     </Router>
   );
 }
}

render(<App />, document.getElementById('root'));

```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9:](Create more views with React)]

In this step, you will create more React micro-frontends including a product list and product details.

1. Navigate to `react-core-mf/src/views`and create a file `List.jsx` with the following content:

    ```XML
    import React from 'react';
    import { MessageStrip, Avatar, LayoutPanel, LayoutGrid } from 'fundamental-react';

    const NO_AVAILABLE_PRODUCT_MSG = 'Unfortunately, there is no available product at this moment.';
    const panelStyle = { cursor: 'pointer' };

    export const List = ({ items }) => (
       (items.length === 0) ? <MessageStrip type='error'>{NO_AVAILABLE_PRODUCT_MSG}</MessageStrip>
       : items.map(({id, name, price, icon, stock}) => {
           return (
                <LayoutPanel key={id} style={panelStyle}>
                    <LayoutPanel.Header>
                        <LayoutPanel.Head  title={name} />
                    </LayoutPanel.Header>
                    <LayoutPanel.Body>
                        <LayoutGrid cols={2}>
                            <div>
                                <div>Price: &euro;{price}</div>
                                <div>Stocks: {stock}</div>
                            </div>
                            <div><Avatar circle glyph={icon} size='s' /></div>
                        </LayoutGrid>
                    </LayoutPanel.Body>
                </LayoutPanel>
           )
       })
    );

    ```

2. Create a file named `Products.jsx` in the same folder:

    ```XML
    import React from 'react';
    import { List } from './List.jsx';
    import { ProductCollection } from '../../../ui5-mf/uimodule/webapp/model/products.json';
    import { LayoutPanel, LayoutGrid } from 'fundamental-react';

    export const Products = () => (
       <section className="fd-section">
           <LayoutPanel>
               <LayoutPanel.Header>
                   <h3>Items ({ProductCollection.length})</h3>
               </LayoutPanel.Header>
               <LayoutPanel.Body>
                   <LayoutGrid cols={2}>
                       <List items={ProductCollection} />
                   </LayoutGrid>
               </LayoutPanel.Body>
           </LayoutPanel>
       </section>
    );

    export default Products;
    ```

3. Create a `ProductDetail.jsx` file in the same folder:

    ```XML
    import React, { useState, useEffect } from 'react';
    import { ProductCollection } from '../../../ui5-mf/uimodule/webapp/model/products.json';
    import { LayoutPanel, MessageStrip } from 'fundamental-react';

    const NO_MATCH_PRODUCT_MSG = 'This product is not available. Please check again.';

    export const ProductDetail = ({ match }) => {
     const itemId = parseInt(match.params.id);
     const [item, setItem] = useState(null);

     useEffect(()=> {
         setItem(ProductCollection.find(product => product.id === itemId));
     }, [itemId]);

     const renderDetails = () => (
       <LayoutPanel>
           <LayoutPanel.Header>
               <h1 className="fd-section__title">{item.name}</h1>
           </LayoutPanel.Header>
           <LayoutPanel.Filters>
               <div>Price: &euro;{ item.price }</div>
               <div>Stocks: { item.stock }</div>
           </LayoutPanel.Filters>
           <LayoutPanel.Body>
               <p>{ item.description }</p>
           </LayoutPanel.Body>
       </LayoutPanel>
     );

     return (
       <section className="fd-section">
         {item
           ? renderDetails()
           : <MessageStrip type='error'>{ NO_MATCH_PRODUCT_MSG }</MessageStrip>
         }
       </section>
     );
    };
    ```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Add "Products" view to Luigi app)]

In this step, you will add a navigation node in Luigi for the "Products" micro-frontend.

1. Open `react-core-mf/public/luigi-config.js`

2. Add a new "Products" node to your navigation:

    ```JavaScript
    navigation: {
      nodes: () => [
        {
          pathSegment: 'home',
          label: 'Home',
          icon: 'home',
          viewUrl: '/app.html#/home',
          children: [{
            pathSegment: 'products',
            label: 'Products',
            icon: 'list',
            viewUrl: '/app.html#/products'
          }]
        }
      ]
    }
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Add "Product Detail" view to Luigi app)]

In this step, you will add the `ProductDetail.jsx` view to the app. You will be able to show details for each product via a Luigi [dynamic parameter](https://docs.luigi-project.io/docs/navigation-advanced?section=dynamically-changeable-paths), in this case named `:id`.

1. Open `react-core-mf/src/index.js` and add:

    ```JavaScript
    import { ProductDetail } from './views/ProductDetail.jsx';
    ```

2. Add the following route paths to the `<Router>` section:

    ```JavaScript
    <Route path="/products" component={Products} />
    <Route path='/productDetail/:id' component={ProductDetail} />
    ```

3. In `luigi-config.js`, add a child node `:id` to Products:

    ```JavaScript
    pathSegment: 'products',
    label: 'Products',
    icon: 'list',
    viewUrl: '/app.html#/products',
    keepSelectedForChildren: true,
    children: [{
        pathSegment: ':id',
        viewUrl: '/app.html#/productDetail/:id'
    }]
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Use Luigi link manager for routing)]

Instead of using React, in this step we we will use Luigi to provide routing on the micro-frontend side. Luigi Client's [`linkManager`](https://docs.luigi-project.io/docs/luigi-client-api/?section=linkmanager) function is the simplest way to navigate to the `id` page for each product.

1. Open `react-core-mf/src/views/List.jsx`

2. Import [link manager](https://docs.luigi-project.io/docs/luigi-client-api/?section=linkmanager) from Luigi Client:

    ```JavaScript
    import { linkManager } from '@luigi-project/client';
    ```

3. Copy and paste this into the `List.jsx` file:

    ```JavaScript
    const navigateToDetail = (id) => {
       linkManager().navigate('/home/products/' + id);
    }
    ```

4. Add `onClick` event at the `LayoutPanel` component:

    ```JavaScript
    <LayoutPanel key={id} style={panelStyle} onClick={()=>navigateToDetail(id)}>
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Run your core app)]

In this step, you can check if your core app is configured correctly so far by running it locally.

1. Open a terminal/command prompt window. Navigate to the `react-core-mf` folder.

2. Input `npm start`. Your application should be up and running at `http://localhost:3000/`. You should be able to see the homepage and "Products" view.

[VALIDATE_1]
[ACCORDION-END]


---
