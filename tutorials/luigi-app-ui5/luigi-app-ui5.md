---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>javascript]
primary_tag: topic>user-interface
---

# Build Luigi Micro-Frontend with UI5
<!-- description --> Create a micro-frontend with UI5 and connect it to the core React app using Luigi client.

## You will learn
  - How to create a Luigi view with UI5
  - How to import Luigi Client to your view

---

### Add Luigi to UI5 micro-frontend


In this step, you will import Luigi Client to the UI5 micro-frontend so you can use `linkManager` and other API function in the next steps.

1. Open `ui5-mf/webapp/index.html` and add this line in the head section of the file:

    ```HTML
    <!--Inside the head tag -->
    <script src="https://unpkg.com/@luigi-project/client@latest/luigi-client.js"></script>
    ```

2. In the same file, change the UI5 option from `data-sap-ui-frameOptions="trusted"` to `data-sap-ui-frameOptions="allow"`. Directly below, add `data-sap-ui-preload=""`:

    ```HTML
    <script
        id="sap-ui-bootstrap"
        src="https://openui5.hana.ondemand.com/resources/sap-ui-core.js"
        data-sap-ui-theme="sap_fiori_3"
        data-sap-ui-resourceroots='{
                "luigi.ui5": "./"
            }'
        data-sap-ui-oninit="module:sap/ui/core/ComponentSupport"
        data-sap-ui-compatVersion="edge"
        data-sap-ui-async="true"

        <!-- Change these 2 attributes -->
        data-sap-ui-frameOptions="allow"
        data-sap-ui-preload=""
        <!-- To the shown values -->

    ></script>
    ```


### Create "Order History" view


In this step, you will add a new navigation node in Luigi config hosted in the React application, then create the micro-frontend content using UI5.

1. Add an "Order History" node to your Luigi navigation in `react-core-mf/public/luigi-config.js`:

    ```JavaScript
    children: [
              {
                pathSegment: "products",
                label: "Products",
                icon: "product",
                viewUrl: "/sampleapp.html#/microfrontend/products",
                keepSelectedForChildren: true,
                children: [{
                    pathSegment: ':id',
                    viewUrl: '/sampleapp.html#/microfrontend/productDetail/:id',
                    context: { id: ':id' }
                }]
            }, // <--- Don't forget to add a comma here ---> 
            //<---Add the section below to the Luigi config--->
            {
                pathSegment: 'order',
                label: 'Order History',
                icon: 'history',
                viewUrl: 'http://localhost:8080/index.html'
            }
            //<------>
          ],
    ```

2. Open the `ui5-mf/webapp/view/MainView.view.xml` file in your UI5 app, and replace the content with:

    ```XML
    <mvc:View controllerName="luigi.ui5.controller.Main"
      displayBlock="true"
      xmlns="sap.m"
      xmlns:mvc="sap.ui.core.mvc">
      <List
        items="{/ProductCollection}">
        <ObjectListItem
          title="{name}"
          type="Active"
          press="onListItemPress"
          number="{
            parts:[{path:'price'},{path:'currencyCode'}],
            type: 'sap.ui.model.type.Currency',
            formatOptions: {showMeasure: false}
          }"
          numberUnit="{currencyCode}">
          <ObjectAttribute text="Quantity: {orderQuantity}" />
        </ObjectListItem>
      </List>
    </mvc:View>
    ```

3. Open `ui5-mf/webapp/controller/Main.controller.js` and replace it with:

    ```JavaScript
    sap.ui.define(["luigi/ui5/controller/BaseController"], function (Controller) {
        "use strict";

        return Controller.extend("luigi.ui5.controller.Main", {
            onInit: function (Controller) {
                const oModel = new sap.ui.model.json.JSONModel();

                oModel.loadData("../model/products.json");
                this.getView().setModel(oModel);
            },

            onListItemPress: function (oEvent) {
                const id = oEvent.getSource().getBindingContext().getProperty("id");

                LuigiClient.linkManager().openAsModal('/home/products/' + id, { title: 'Product Detail', size: 'm' });
            }
        });
    });
    ```

4. Navigate to `ui5-mf/webapp/model` and create a `products.json` file with the following content.

```JSON
{
    "ProductCollection": [
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
    ]
}
```


### Run UI5 app


You can run the UI5 project to check if it's assembled correctly. Open a terminal/command prompt and navigate to `ui5-mf`, then execute:

```Shell
npm start
```

You should be able to access the app at `http://localhost:8080/index.html`. If you want to see the UI5 project in the context of the larger app, open a new terminal window, navigate to `react-core-mf` and run `npm start`. Refresh your browser to see the changes.



