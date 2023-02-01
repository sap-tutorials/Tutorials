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

1. Open `ui5-mf/uimodule/webapp/index.html` and add this line in the head section of the file:

    ```HTML
    <!--AROUND LINE 22-->
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
        <!-- CHANGE THESE 2 ATTRIBUTES -->
        data-sap-ui-frameOptions="allow"
        data-sap-ui-preload=""
        <!-- TO THE SHOWN VALUES -->
    ></script>
    ```


### Create "Order History" view


In this step, you will add a new navigation node in Luigi, then create the micro-frontend content using UI5.

1. Add an "Order History" node to your Luigi navigation in `luigi-config.js`:

    ```JavaScript
    children: [
              {
                pathSegment: "products",
                label: "Products",
                icon: "product",
                viewUrl: "/sampleapp.html#/products",
                keepSelectedForChildren: true,
                children: [{
                    pathSegment: ':id',
                    viewUrl: '/sampleapp.html#/productDetail/:id',
                    context: { id: ':id' }
                }]
            }, // <--- DON'T FORGET TO ADD A "," HERE!
            //THIS SECTION HAS TO BE ADDED
            {
                pathSegment: 'order',
                label: 'Order History',
                icon: 'history',
                viewUrl: 'http://localhost:8080/index.html'
            }
            //UNTIL HERE
          ],
    ```

2. Open the `ui5-mf/uimodule/webapp/view/MainView.view.xml` file in your UI5 app, and replace the content with:

    ```XML
    <mvc:View controllerName="luigi.ui5.controller.MainView"
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

3. Open `ui5-mf/uimodule/webapp/controller/MainView.controller.js` and replace it with:

    ```JavaScript
    sap.ui.define(["luigi/ui5/controller/BaseController"], function (Controller) {
        "use strict";

        return Controller.extend("luigi.ui5.controller.MainView", {
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


### Run UI5 app


You can run the UI5 project to check if it's assembled correctly. Open a terminal/command prompt and navigate to `ui5-mf`, then execute:

```Shell
npm start
```

You should be able to access the app at `http://localhost:8080/index.html`. If you want to see the UI5 project in the context of the larger app, open a new terminal window, navigate to `react-core-mf` and run `npm start`. Refresh your browser to see the changes.



