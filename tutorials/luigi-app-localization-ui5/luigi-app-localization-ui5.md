---
title: Add Localization to Luigi UI5 Micro-Frontend
description: Enable your micro-frontend to be displayed in multiple languages using the Luigi localization features.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>javascript]
primary_tag: topic>user-interface
---

## Details
### You will learn
  - How to add localization to your UI5 micro-frontend

---


[ACCORDION-BEGIN [Step 1: ](Get current language with Luigi Client)]

In this step, you will add a function to get the current language from Luigi Client and then update it, so that the language of the UI5 micro-frontend can be changed accordingly.

Go to the `Order.controller.js` file and add this below `onInit: function (Controller) {`:

```JavaScript
const updateCurrentLanguage = () => {
  const currentLanguage = LuigiClient.uxManager().getCurrentLocale();
  sap.ui.getCore().getConfiguration().setLanguage(currentLanguage);
}

LuigiClient.addInitListener(updateCurrentLanguage);
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add files with multi-language content)]

In this step, you will create files with the text that is to be changed within the UI5 micro-frontend.

1. Create a folder called ​`i18n`​ under the ​`uimodule/webapp`​ folder.  Inside it, create a file called `i18n_de_DE.properties` with the following content:

    ```
    Quantity = Menge
    ```

2. Create another file called `i18n_en_US.properties` with the following content:

    ```
    Quantity = Quantity
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add default language to index.html)]

Edit the  `ui5-mf/uimodule/webapp/index.html` file  by adding the default language (EN) around line 12, above `data-sap-ui-theme`:

```HTML
 data-sap-ui-language='en-US'
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Provide translation in UI5)]

This step involves the standard process in UI5 for providing translation.

1. Edit the `ui5-mf/uimodule/webapp/manifest.json` file by adding a `model` object into the `sap.ui5` object:

    ```JSON
    "models": {
        "i18n": {
          "type": "sap.ui.model.resource.ResourceModel",
          "settings": {
            "bundleName": "luigi.ui5.i18n.i18n"
          }
        }
      },
    ```

2. Edit the ​`ui5-mf/uimodule/webapp/view/Order.view.xml` ​file by marking the translated target text. Replace line 17 with:

    ```XML
    <ObjectAttribute text="{i18n>Quantity}: {orderQuantity}" />
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run completed app)]

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



[VALIDATE_1]
[ACCORDION-END]




---
