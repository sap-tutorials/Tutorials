---
parser: v2
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>javascript]
primary_tag: topic>user-interface
---

# Add Localization to Luigi UI5 Micro-Frontend
<!-- description --> Enable your micro-frontend to be displayed in multiple languages using the Luigi localization features.

## You will learn
  - How to add localization to your UI5 micro-frontend

---


### Update current language with Luigi Client


In this step, you will add a function to get the current language from Luigi Client and then update it, so that the language of the UI5 micro-frontend can be changed accordingly.

 1. Open the `ui5-mf/uimodule/webapp/controller/MainView.controller.js` and replace its content with the code below:

    ```js
    sap.ui.define(["luigi/ui5/controller/BaseController"], function (Controller) {
        "use strict";

        return Controller.extend("luigi.ui5.controller.MainView", {
            onInit: function (Controller) {
                const oModel = new sap.ui.model.json.JSONModel();

                oModel.loadData("../model/products.json");
                this.getView().setModel(oModel);
                //THIS HAS BEEN ADDED - TO UPDATE THE CURRENT LANGUAGE
                const updateCurrentLanguage = () => {
                    const currentLanguage = LuigiClient.uxManager().getCurrentLocale();
                    sap.ui.getCore().getConfiguration().setLanguage(currentLanguage);
                }
                //THIS HAS BEEN ADDED - LISTENER FOR LANGUAGE CHANGES
                LuigiClient.addInitListener(updateCurrentLanguage);
            },

            onListItemPress: function (oEvent) {
                const id = oEvent.getSource().getBindingContext().getProperty("id");
                // GETTING TRANSLATED TITLE TEXT FOR THE MODAL TITLE
                const title = this.getView().getModel("i18n").getResourceBundle().getText("ModalText");

                LuigiClient.linkManager().openAsModal('/home/products/' + id, { title: title, size: 'm' });
            }
        });
    });
    ```


### Add files with multi-language content


In this step, you will create files with the text that is to be changed within the UI5 micro-frontend.

1. Find the folder ​`i18n`​ under the ​`uimodule/webapp`​.  Inside it, create a file called `i18n_de_DE.properties` with the following content:

    ```json
    ModalText = Produkt Details
    Quantity = Anzahl
    ```

2. Create another file called `i18n_en_US.properties` with the following content:

    ```json
    ModalText = Product Details
    Quantity = Quantity
    ```


### Add default language to index.html


1. Edit the `ui5-mf/uimodule/webapp/index.html` file by adding the default language (EN) around line 12, above `data-sap-ui-theme`:

    ```HTML
    <script
        id="sap-ui-bootstrap"
        src="https://openui5.hana.ondemand.com/resources/sap-ui-core.js"
        data-sap-ui-theme="sap_fiori_3"
        data-sap-ui-language='en-US'
        data-sap-ui-resourceroots='{
                "luigi.ui5": "./"
            }'
        data-sap-ui-oninit="module:sap/ui/core/ComponentSupport"
        data-sap-ui-compatVersion="edge"
        data-sap-ui-async="true"
        data-sap-ui-frameOptions="allow"
        data-sap-ui-preload=""
    ></script>
    ```


### Provide translation in UI5


This step involves the standard process in UI5 for providing translation.

1. Edit the ​`ui5-mf/uimodule/webapp/view/Order.view.xml` ​file by marking the translated target text. Replace line 17 with:

    ```XML
    <ObjectAttribute text="{i18n>Quantity}: {orderQuantity}" />
    ```


### Run completed app


Now, your app should be complete and you can run it locally to see if everything works. First, open a terminal/command prompt window and navigate to your project folder.

1. Navigate to the React app and run it:

    ```Shell
    cd react-core-mf
    ```

    and:

    ```Shell
    npm start
    ```

2. In another window, navigate to the UI5 micro-frontend and run it:

    ```Shell
    cd ui5-mf
    ```

    and:

    ```Shell
    npm start
    ```

3. See your completed app at `http://localhost:3000/`. Try changing the language and clicking around. If there are errors, retrace your steps in the tutorial or compare with the finished app on [SAP Samples](https://github.com/SAP-samples/luigi-micro-frontend-application).








---
