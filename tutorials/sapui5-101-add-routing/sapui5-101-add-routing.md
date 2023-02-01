---
parser: v2
auto_validation: true
primary_tag: programming-tool>sapui5
tags: [  tutorial>beginner, programming-tool>html5, programming-tool>sapui5, software-product>sap-btp--cloud-foundry-environment, software-product>sap-business-application-studio  ]
time: 20
author_name: Nico Schoenteich
author_profile: https://github.com/nicoschoenteich
---

# Add Views and Define Routes to Access Them
<!-- description --> Add new views to the SAPUI5 web application and declare them in the manifest.

## You will learn  
- How to add additional views
- How to use data binding
- How to define routes and targets
- How to add additional controller

---

### Add two new views

In SAPUI5, each view is represented by a dedicated file in the `view` folder.

1. Add a new view with a right-click on the `view` folder and select **New File**. Name this file `List.view.xml`.

    <!-- border -->![newFile](./newView.png)

2. The name already suggests that this view will contain a [list](https://sapui5.hana.ondemand.com/#/topic/295e44b2d0144318bcb7bdd56bfa5189) of products. Add the following file content that defines the views and the list. Note the list already uses [data binding](https://sapui5.hana.ondemand.com/#/topic/68b9644a253741e8a4b9e4279a35c247) to show the product entities as list items.

    ```XML
    <mvc:View controllerName="sap.btp.sapui5.controller.List" xmlns:mvc="sap.ui.core.mvc" displayBlock="true" xmlns="sap.m">
      <Page id="listPage" title="{i18n>ListTitle}" >
            <List id="list" items="{/Products}">
              <StandardListItem id="_IDGenStandardListItem1" type="Navigation" press="handleListItemPress" title="{ProductName}"/>
            </List>
        </Page>
    </mvc:View>
    ```


3. **Repeat** step 1 to create another view with the name `Detail.view.xml`. We will use this view to display details of a given product.


4. **Insert** the following content in this new file. In contrast to the list, it uses a relative binding, e.g. the binding path doesn't start with `/` like absolute paths do.


    ```XML
    <mvc:View controllerName="sap.btp.sapui5.controller.Detail" xmlns:mvc="sap.ui.core.mvc" displayBlock="true" xmlns="sap.m">
      <Page id="detail" title="{i18n>DetailTitle}" showNavButton="true" navButtonPress="handleNavButtonPress" >
          <VBox id="_IDGenVBox1">
              <Text id="_IDGenText1" text="{ProductName}" />
              <Text id="_IDGenText2" text="{UnitPrice}" />
              <Text id="_IDGenText3" text="{QuantityPerUnit}" />
              <Text id="_IDGenText4" text="{UnitsInStock}" />
          </VBox>
      </Page>
    </mvc:View>
    ```

> Hint: You don't need to rely on the code editor to edit the views. Right-click on any view and select **Open With... > Layout Editor** to access the [WYSIWYG](https://en.wikipedia.org/wiki/WYSIWYG) layout editor:

><!-- border -->![detailView](detailView.png)


### Add new targets and routes

In this step we'll define so-called [routes and targets](https://sapui5.hana.ondemand.com/#/topic/3d18f20bd2294228acb6910d8e8a5fb5), which are needed for the automated navigation we want to use. Each route defines a (URL) pattern and the target it points to, and each target specifies the view it refers to.

**Change** the pattern for the first route ("RouteView1") from `:?query:` to `RouteView1` (line 16 of the below snippet) in the `webapp/manifest.json` file, so that we can define a new default route.

**Add** the new targets and routes to the `webapp/manifest.json` file.

```JSON[16,21-34,43-57]
{
    "_version": "1.12.0",
    "sap.app": {
        ...
    },
    "sap.ui": {
        ...
    },
    "sap.ui5": {
        ...
        "routing": {
            ...
            "routes": [
                {
                  "name": "RouteView1",
                  "pattern": "RouteView1",
                  "target": [
                    "TargetView1"
                    ]
                },
                {
                    "name": "home",
                    "pattern": "",
                    "target": [
                        "TargetList"
                        ]
                },
                {
                    "name": "detail",
                    "pattern": "product/{productId}",
                    "target": [
                        "TargetDetail"
                        ]
                }
            ],
            "targets": {
                "TargetView1": {
                    "viewType": "XML",
                    "transition": "slide",
                    "clearControlAggregation": false,
                    "viewId": "View1",
                    "viewName": "View1"
                },
                "TargetList": {
                    "viewType": "XML",
                    "transition": "slide",
                    "clearControlAggregation": false,
                    "viewName": "List",
                    "viewId" : "List"
                },
                "TargetDetail": {
                    "viewType": "XML",
                    "transition": "slide",
                    "clearControlAggregation": false,
                    "viewName": "Detail",
                    "viewId": "Detail"
                }
            }
        }
    }
}
```

### Add two new controllers

This is the crucial step of this tutorial that ties everything together. Each view specifies its controller with the `controllerName` property in the first line. Controllers contain the business logic of web apps, bind models to views, and use the router to navigate between views.

You may see a prompt to enable `ESLint extension`, select **Do Not Allow** to proceed as it is not relevant for this tutorial.


1. **Right-click** on the `controller` folder and select **New File** to create a new `List.controller.js` for the list view. The controller defines one method that is the event listener for the press-item event of the product list. It will trigger the navigation to the second view and attach the product ID of the pressed item.

    ```JavaScript
    sap.ui.define([
        "sap/ui/core/mvc/Controller"
    ],
        function (Controller) {
            "use strict";

            return Controller.extend("sap.btp.sapui5.controller.List", {
                handleListItemPress: function (oEvent) {
                    var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
                    var selectedProductId = oEvent.getSource().getBindingContext().getProperty("ProductID");
                    oRouter.navTo("detail", {
                        productId: selectedProductId
                    });
                }
            });
        });
    ```

2. Choose **New File** one more time to create a `Detail.controller.js` file, which corresponds to the detail view.  This control contains methods to handle the inbound navigation, to bind the selected product to the view, and to handle the outbound navigation to get back to the list. **Insert** the following code into the file.

    ```JavaScript
    sap.ui.define([
      "sap/ui/core/mvc/Controller"
    ],

      function (Controller) {
          "use strict";

          return Controller.extend("sap.btp.sapui5.controller.Detail", {
              onInit: function () {
                  var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
                  oRouter.getRoute("detail").attachMatched(this._onRouteMatched, this);
              },
              _onRouteMatched: function (oEvent) {
                  var oArgs, oView;
                  oArgs = oEvent.getParameter("arguments");
                  oView = this.getView();
                  oView.bindElement({
                      path: "/Products(" + oArgs.productId + ")",
                      events: {
                          dataRequested: function () {
                              oView.setBusy(true);
                          },
                          dataReceived: function () {
                              oView.setBusy(false);
                          }
                      }
                  });
              },
              handleNavButtonPress: function (evt) {
                  var oRouter = sap.ui.core.UIComponent.getRouterFor(this);
                  oRouter.navTo("home");
              }
          });
      });
    ```


### Test it

If you already stopped the web app, restart the saved configuration. **Open** the running web app to see the changes.

You should be able to see a list of products and navigate to the detail pages (and back to the list page).


<!-- border -->![demo](./navigation.gif)

---
