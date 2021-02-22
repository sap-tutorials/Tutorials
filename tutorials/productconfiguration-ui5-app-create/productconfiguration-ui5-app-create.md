---
author_name: Angelica Rodriguez
author_profile: https://github.com/angelicarodriguezv
title: Create an SAPUI5 Application for SAP Variant Configuration and Pricing
description: Build a basic UI5 application that loads a configuration and changes the value of one characteristic using the SAP Variant Configuration and Pricing APIs from SAP API Business Hub.
auto_validation: true
time: 45
tags: [tutorial>beginner, topic>sapui5, products>sap-web-ide]
primary_tag: products>sap-variant-configuration-and-pricing
---


## Details
### You will learn
  - How to use SAP API Business Hub's productivity tools for developers (like sandbox environment and code snippet generator) to easily test cloud services
  - How to use SAP Cloud Platform's trial environment and SAP Business Application Studio to build a small SAPUI5 application
  - How to orchestrate and use the different APIs of the variant configuration and pricing services




---

[ACCORDION-BEGIN [Step 1: ](Create a cloud account)]

Create a free trial account on the SAP Cloud Platform to be able to use the SAP Business Application Studio. Go to [SAP Cloud Platform](https://cloudplatform.sap.com/index.html) and click on **Try now / Log in** and then on **Register for Free**.

![step-1-sap-cloud-platform-2020](step-1-sap-cloud-platform-2020.png)
&nbsp;
![step-1-business-platform](step-1-business-platform.png)

Fill the registration form by providing you name, email and a password. the registration is completed an option will be provided to create the account in either US or Europe, log in and press the button **Click here to start your trail!** and open SAP Business Application Studio.

![step-1-SAP-business-application-studio](Step-1-SAP-Business-Application-Studio.jpg)

Direct link: (https://account.hanatrial.ondemand.com/trial/#/home/trial)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an SAPUI5 application)]

1. In SAP Business Application Studio, click on **Create Dev Space**.

    ![step-2-dev-spaces](step-2-dev-spaces.png)

    For the name, enter **`ProductConfigurationUI`**, choose **SAP Fiori**, and click **Create Dev Space**.

    ![step-2-create-a-new-dev-space](step-2-create-a-new-dev-space.png)

2. When the status changes to **Running**, click on the space name created.

    ![step-2-space-name-created](step-2-space-name-created.png)

3. In the SAP Business Application Studio, create a new project from the template via menu **File** > **New Project from Template**.

    ![step-2-new-project-from-template](step-2-new-project-from-template.png)

    Choose the template **SAP Fiori Freestyle Project** and select **Next**.

    Choose the target running environment **Cloud Foundry** and the **SAPUI5 Application template**, then select **Next**.

    ![step-2-sapui5-application-template](step-2-sapui5-application-template.png)

    For the project name, enter **`ProductConfigurationAPITutorial`**, then select **Next**.

    ![step-2-descriptive-project-name](step-2-descriptive-project-name.png)

    For the HTML5 Application runtime, select **Standalone Approuter**, and select **Next**.

    ![step-2-standalone-approuter](step-2-standalone-approuter.png)

    Enter the **Basic Attributes**:

    |  Basic Attribute      |  Value |    
    |  :-------------      | :-------------
    |  HTML5 module name  | **`ProductConfigurationAPITutorial`**
    |  Add authentication: | **`No`**
    |  Namespace:          | **`sap.tutorial`**
    |  Enable Karma tests: | **`No`**

    Select **Next**.

    ![step-2-basic-attributes](step-2-basic-attributes.png)

    Rename the initial view to `Main`, and choose `No` for adding a data service, and then select **Next**.

    ![step-2-view-name](step-2-view-name.png)

    The application is now created.

4. Open the project in a new workspace.

    ![step-2-open-in-new-space](step-2-open-in-new-space.png)

5. The application will call the Variant Configuration service during the initialization of the form.

    Add an empty `onInit` function to the controller, ``**ProductConfigurationAPITutorial**`` > ``**webapp**`` > ``**controller**`` > ``**Main.controller.js**``, in which the service APIs will be called.

    ![step-2-empty-oninit](step-2-empty-oninit.png)

You will call the following cloud service APIs:

-	`POST /api/v2/configurations` to create a new product configuration.
-	`GET /api/v2/knowledgebases/{kbId}` to read static master data for display (descriptions of a characteristic and its values in our example).
-	`PATCH /api/v2/configurations/{configurationId}/items/{itemId}/characteristics/{characteristicId}` to change a characteristic value.
-	`GET /api/v2/configurations/{configurationId}` to read and displayed the changed configuration results.
- `POST /api/v1/statelesspricing` to get the price based on the chosen characteristic value.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Get pre-generated code)]

In the [API Business Hub] (https://api.sap.com/) search for SAP Variant Configuration, find the **SAP Variant Configuration and Pricing** API Package and select it.

![step-3-API](step-3-API.png)

Once on the API package page, choose **Variant Configuration service**.

!![step-3-API-variant-configuration](step-3-API-variant-configuration.png)

On the API reference, find the `POST /api/v2/configurations` method and click on the **Code Snippet** link.

![step-3-API-reference](step-3-API-reference.png)

Click on the **JavaScript** tab and then click on the **Copy and Close** button to copy the code to your clipboard.

>An `APIKey` is used as an authentication method. Each time an API is called, the `APIKey` needs to be sent in the http request header. Make sure you are logged into SAP API Business Hub when copying the code to your clipboard so that your `APIKey` is automatically added in the generated code. If you need to get your `APIKey`, you can use the **Show API Key** button at the top of the same page.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Load configuration in your application)]

1. Back in SAP Business Application Studio, add the copied code from the API Business Hub to your `onInit` function.

2. Replace in the code the `data` variable by using the example input JSON found for the service on API Hub.

    ![step-4-API-hub](step-4-API-hub.png)

    ```JavaScript
    var data = JSON.stringify({
      "productKey": "CPS_BURGER",
      "date": "2018-08-09",
      "context": [{
        "name": "VBAP-VRKME",
        "value": "EA"
      }]
    });
    ```

    ![step-4-main-controller](step-4-main-controller.png)

>ESLINT errors caused by console statement or hard-coded URL can be switched off in project settings or by inserting `Java /* eslint-disable */` in the first line.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run your application)]

Run your new application to test it.

1. Click **View > Run Configurations**.

    ![step-5-run-configurations-view](step-5-run-configurations-view.png)

2. Select **+** to create a new run configuration.

    ![step-5-new-run-configurations](step-5-new-run-configurations.png)

    >Creating a new run configuration launches the command palette, a text-based mini wizard. The command palette is opened at the top-center of the SAP Business Application Studio window.

3. When the **What would you like to run?** question appears, choose `ProductConfigurationAPITutorial`.

4. For the next steps of the wizard, select the following:

    |  Step     | Parameter                | Value
|  :------- | :-----------------------   :-------------------------------------------------------------------
|  A        | `Select a runnable file` | index.html
|  B        | `Select a UI5 version`   | latest
|  C        | `Enter a name`           | Run ProductConfigurationAPITutorial-ProductConfigurationAPITutorial

5. Expand the run configuration and hover over your run configuration and click the **Run Module** icon.

    ![step-5-sap-business-application-studio](step-5-sap-business-application-studio.png)

6. Wait for the **A service is listening to port 6004** notification and click on the button to open the app.

    >The left side view changes to the debug view and the status bar color changes to orange to indicate that the app is running in debug mode.
If you are running the app for the first time, the button in the notification will say **Expose and Open**. Otherwise, it will say **Open in New Tab**.

    ![step-5-a-service-is-listening](step-5-a-service-is-listening.png)

7. Enter an optional description for the port and press **Enter**.

 You should see a blank application with a title `opened in a new tab`. The pre-generated code puts the results of the API in the browser console. To find the result, open your browser's developer tools and go to the **Console** tab.

![step-5-blank-application](step-5-blank-application.png)

Which field in the response body of service endpoint `/api/v2/configurations` returns the unique identifier of the configuration? This identifier must be provided as input field `configurationId` to the subsequent calls to the other endpoints, e.g. to change a characteristic value.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Display characteristic value)]

The result from the API consists of the configuration, characteristics, and characteristic values. Add a `ComboBox` to display characteristic **`CPS_OPTION_M`** value.
 Open your main view and add a `ComboBox`. The `ComboBox` items and selected item will be set from the result of the API call.

![step-6-main-controller](step-6-main-controller.png)

```JavaScript
<mvc:View controllerName="sap.tutorial.ProductConfigurationAPITutorial.controller.Main" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:mvc="sap.ui.core.mvc" xmlns:core="sap.ui.core"  displayBlock="true" xmlns="sap.m">
        <App id="app">
            <pages>
                <Page title="{i18n>title}">
                    <content>
                        <ComboBox items="{/possible_values}" selectedKey="{/value}">
                            <core:Item key="{valueLow}" text="{valueLow}"/>
                        </ComboBox>
                    </content>
                </Page>
            </pages>
        </App>
</mvc:View>
```

>Ensure that core namespace is declared via `xmlns:core="sap.ui.core"`

Back in the controller file, you will need to define the model used in the view. You need to add the `JSONModel` library to your controller. In the `define` at the top of the controller, add the `JSONModel` library by adding **`sap/ui/model/json/JSONModel`** and defining the `JSONModel` in the controller function.

```JavaScript
sap.ui.define([
    "sap/ui/core/mvc/Controller",
    "sap/ui/model/json/JSONModel"
], function (Controller, JSONModel) {
```

In the `onInit` function, you need to save the current version so that you can access the view associated with the controller in the API call response. Create a new variable called **self** and set it to **this**.

```JavaScript
var self = this;
```

Additionally, you need to create a new `JSONModel` to house the results of the API call. Bind a new empty `JSONModel` to the view.

```JavaScript
this.getView().setModel(new JSONModel({}));
```

To actually bind the result to the model, you need to parse the API response in the `xhr.addEventListener` function. The result from the API comes back as text, so you need to parse it to JSON.

```JavaScript
var jsonResults = JSON.parse(this.responseText);
```

Then, you can set the relevant properties into the model. The `ComboBox` needs `CPS_OPTION_M` possible values and its initial value.

```JavaScript
 var CPS_OPTION_M = jsonResults.rootItem.characteristics.find(function (i) { return i.id === "CPS_OPTION_M"; });
self.getView().getModel().setProperty("/possible_values", CPS_OPTION_M.possibleValues);
if (CPS_OPTION_M.values.length > 0) {
    self.getView().getModel().setProperty("/value", CPS_OPTION_M.values[0].value);
} else {
    self.getView().getModel().setProperty("/value", "");
}
```
>The used find() statement is not supported by Internet Explorer 11.

![step-6-controller-extend](step-6-controller-extend.png)

Save your changes. If you execute the application, you will see a `ComboBox` filled with `CPS_OPTION_M` characteristic possible values, having the selected value be the default value for this characteristic.

![step-6-combobox](step-6-combobox.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Change characteristic value)]

Next, update the configuration if the user changes the value of the `ComboBox`. First, you need to declare a new event on the `ComboBox` control in the view.

![step-7-combobox](step-7-combobox.png)

```JavaScript
<mvc:View controllerName="sap.tutorial.ProductConfigurationAPITutorial.controller.Main" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:mvc="sap.ui.core.mvc" xmlns:core="sap.ui.core"  displayBlock="true" xmlns="sap.m">
       <App id="app">
           <pages>
               <Page title="{i18n>title}">
                   <content>
                       <ComboBox items="{/possible_values}" selectedKey="{/value}" selectionChange="onChange">
                            <core:Item key="{valueLow}" text="{valueLow}"/>
                        </ComboBox>
                    </content>
                </Page>
            </pages>
        </App>
</mvc:View>
```

Back to the main controller, create an empty `onChange` function.

```JavaScript
onChange: function (oEvent) {

}
```

As in step 3, head over to SAP API Business Hub, locate the PATCH **`/api/v2/configurations/{configurationId}/items/{itemId}/characteristics/{characteristicId}`** method and copy the JavaScript code.

![step-7-product-configuration-service](step-7-product-configuration-service.png)

Add the copied `JavaScript` code from API Business Hub to the newly created `onChange` function.

![step-7-java-code](step-7-java-code.png)

Change the data variable declaration to assign the value from the value property of the view model, which is bound to the `ComboBox` value.

```JavaScript
var data = JSON.stringify({
    "values": [{
        "value": this.getView().getModel().getProperty("/value"),
        "selected": true
    }]
});
```
Since this API PATCH method does not return a response body, in the `xhr.addEventListener` call of the `onChange` function, you may change the console log so that the response code is logged instead of the response text.

```JavaScript
console.log(this.status);
```
To fill out all parameters for this API method, you need to add a few fields in the view model, namely `configuration id` and `item id`. Add these new properties on the model in the `xhr.addEventListener` call of the `onInit` function so that the model is filled when the configuration is loaded.

```JavaScript
self.getView().getModel().setProperty("/config_id", jsonResults.id);
self.getView().getModel().setProperty("/item_id", jsonResults.rootItem.id);
```

Once they are added in the model, replace hard-coded `{configurationID}` and `{itemID}` in the generated `url` in the `onChange` function by the values in the model. Likewise, replace hard-coded `{characteristidID}` by `CPS_OPTION_M`.

```JavaScript
xhr.open("PATCH",
    "https://sandbox.api.sap.com/cpservices/prodconf/api/v2/configurations/" +
    this.getView().getModel().getProperty("/config_id") + "/items/" +
    this.getView().getModel().getProperty("/item_id") + "/characteristics/CPS_OPTION_M "
);
```

Almost done! The variant configuration API uses HTTP header fields `etag` and `If-Match` as an optimistic lock. You need to capture the `etag` header in the model from the HTTP response when loading the configuration and send back that value in the `If-Match HTTP` header when updating the configuration. In the **`xhr.addEventListener`** call of the `onInit` function, set the `etag` property of the model with the `etag` value of the response header.

```JavaScript
self.getView().getModel().setProperty("/etag", this.getResponseHeader("etag"));
```

In the same way, you need to capture the `etag` value of the characteristic change response in case the user wants to update the value multiple times. Add the same line in the **`xhr.addEventListener`** call of the `onChange` function.
 Back in the `onChange` function, fill the `If-Match` request header value with the `etag` value of the model.

```JavaScript
xhr.setRequestHeader("If-Match", this.getView().getModel().getProperty("/etag"));
```

![step-7-java-code-oninit](step-7-java-code-oninit.png)

![step-7-java-code-onchange](step-7-java-code-onchange.png)


>Do not forget variable `self` in this and the coming new functions.

Run your application. You should see a `ComboBox` filled with the possible values of characteristic `CPS_OPTION_M`, having the selected value be the default value for this characteristic. If you change the value of the `ComboBox`, the call is made to the API to change the value, and you can see the response code in the JavaScript console, which should be 200.

![step-8-sample-UI5](step-8-sample-UI5.png)

By how much is the `eTag` value in the service response header increased with each change to the characteristic `CPS_OPTION_M?` Check the  development tools of your browser.

[VALIDATE_2]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 8: ](Use value description)]
Currently, the value keys are displayed in the interface. In a real-world scenario, you might want to provide the value descriptions to the user and add a label to the `combo box`. This can be achieved by getting the knowledge base details.

First, create a new method `readKb` in the controller that takes a knowledge base id as parameter.
When creating a configuration (Step 4), the knowledge base id is returned from the API. To get the `KB` details, another API needs to be called. Go to the API Business Hub (as in step 3), locate the `GET/api/v2/knowledgebases/{kbId}` method, copy the JavaScript code then paste it in the `readKb` function.

![step-8-variant-configuration-service-code-snippet](step-8-variant-configuration-service-code-snippet.jpg)
&nbsp;
![step-8-product-configuration-service-code-snippet](step-8-product-configuration-service-code-snippet.jpg)

Modify the URL of the request to include the knowledge base ID function parameter.

```JavaScript
xhr.open("GET", "https://sandbox.api.sap.com/cpservices/prodconf/api/v2/knowledgebases/" + kbId + "?$select=products,classes,characteristics,characteristicSpecifics,bomItems,description");
```
Now you need to bind the possible values to the model by parsing the response text to JSON then retrieving the `possible_values` property of `CPS_OPTION_M`. Also, add the characteristic name to a new model property `/name`.

```JavaScript
var jsonResults = JSON.parse(this.responseText);
var CPS_OPTION_M = jsonResults.characteristics.find(function (i) {
return i.id === "CPS_OPTION_M";
});

self.getView().getModel().setProperty("/possible_values", CPS_OPTION_M.possibleValues);
self.getView().getModel().setProperty("/name", CPS_OPTION_M.name);
```
![step-9-readKb](step-9-readKb.png)

Remember to remove the assignment of `possible_values` to the model in the `addEventListener` function of the `onInit` function.
 Now, you need to call the new function `readKb` when the configuration is created with the knowledge base id, at the `addEventListener` of the `onInit` function.

![step-9-oninit-function](step-9-oninit-function.png)

In the view, add a `SimpleForm` which contains the previous `ComboBox` and a new Label. Set the label text to the new model property `/name`.

Finally, adjust the `ComboBox` control in the view so that elements `id` and `name` of `possible_values` are used as key and text respectively.

![step-9-mvc-view](step-9-mvc-view.png)

```xml
<sap.ui.layout.form:SimpleForm xmlns:sap.ui.layout.form="sap.ui.layout.form" editable="true" layout="ResponsiveGridLayout" id="form0">
<sap.ui.layout.form:content>
<Label text="{/name}" id="label"/>
<ComboBox items="{/possible_values}" selectedKey="{/value}" selectionChange="onChange">
<core:Item key="{id}" text="{name}"/>
</ComboBox>
</sap.ui.layout.form:content>
</sap.ui.layout.form:SimpleForm>
```
Run your application. The value descriptions are provided by the `ComboBox`, and the label text comes from the knowledge base.

![step-10-sample-UI5](step-10-sample-UI5.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 9: ](Calculate pricing)]
In addition to the configuration, SAP Variant Configuration and Pricing also provides a way to calculate the pricing.

In the Burger model, the price is influenced by the menu option. By changing the menu option values, we should see a difference in the calculated price.

For the pricing API to correctly reflect the status of the configuration, the variant condition `characteristic values` needs to be provided. Which means that the configuration needs to be read after updating it in the app.

Locate the GET `/api/v2/configurations/{configurationId}` in the SAP API Business Hub, copy the JavaScript code and paste it in a new method `_getConfig`.
   Modify the URL of the request to include the configuration ID from the model.

```JavaScript
xhr.open("GET", "https://sandbox.api.sap.com/cpservices/prodconf/api/v2/configurations/" + this.getView().getModel().getProperty("/config_id"));
```
In the `xhr.addEventListener` function, assign the configuration JSON in a new attribute `self._config`.

![step-11-getConfig](step-11-getConfig.png)

Call this new method from the `xhr.addEventListener` function of the `onChange` method, so that the configuration is read again as soon as it is modified.

![step-11-onChange](step-11-onChange.png)

Next, add new read-only fields that will be used to show the base price and selected options as well as a button to calculate the price.

![step-11-view](step-11-view.png)

```JavaScript
<mvc:View controllerName="sap.tutorial.ProductConfigurationAPITutorial.controller.Main" xmlns:html="http://www.w3.org/1999/xhtml" xmlns:mvc="sap.ui.core.mvc" xmlns:core="sap.ui.core"  displayBlock="true" xmlns="sap.m">
        <App id="app">
            <pages>
                <Page title="{i18n>title}">
                    <content>
                        <sap.ui.layout.form:SimpleForm xmlns:sap.ui.layout.form="sap.ui.layout.form" editable="true" layout="ResponsiveGridLayout" id="form0">
                        <sap.ui.layout.form:content>
                            <Label text="{/name}" id="label"/>
                            <ComboBox items="{/possible_values}" selectedKey="{/value}" selectionChange="onChange">
                                <core:Item key="{id}" text="{name}"/>
                            </ComboBox>
                            <Label text="{i18n>basePrice}"/>
                            <Input value="{/base_price}" editable="false"/>
                            <Label text="{i18n>selectedOptions}"/>
                            <Input value="{/selected_options}" editable="false"/>
                            <Label text="{i18n>price}"/>
                            <Input value="{/price}" editable="false"/>
                        </sap.ui.layout.form:content>
                        </sap.ui.layout.form:SimpleForm>
                    <Button text="{i18n>getPrice}" type="Accept" press="onPrice"/>
                    </content>
                </Page>
            </pages>
        </App>
</mvc:View>
```

Do not forget to update your **`i18n`** file with the new labels!
```JavaScript
basePrice=Base price
selectedOptions=Selected options
price=Price
getPrice=Get pricing
```

Next, go to the SAP API Business Hub, locate the `/api/v1/statelesspricing` method in the Pricing service, copy the JavaScript code and paste it in a new method `onPrice`.

![step-9-api-pricing-service](step-9-api-pricing-service.png)

There are a lot of parameters in this API that you would fill out in a real-world application. But for this tutorial, hard code all values except for the `KOMP-VARCOND` attribute of the item 000010.
 Get the variant condition values from the configuration and assign them to the `KOMP-VARCOND` attribute in the request data.

```JavaScript
var varCond = this._config.rootItem.characteristics.find(function (i) {
    return i.id === "CPS_VARCOND";
});
var varCondValues = [];
for (var i = 0; i < varCond.values.length; i++) {
    varCondValues.push(varCond.values[i].value);
}
```

In the `xhr.addEventListener` function of the `onPrice` function, get the net value as well as the value of the condition purposes `ZSS1` and `ZSS2`, which in this test model means **Base Price** and **Selected** Options, and assign them to the JSON model.

```JavaScript
var jsonResults = JSON.parse(this.responseText);

self.getView().getModel().setProperty("/price", jsonResults.netValue);
self.getView().getModel().setProperty("/base_price", jsonResults.conditionsWithPurpose.find(function (i) {
    return i.purpose === "ZSS1";
}).value);
self.getView().getModel().setProperty("/selected_options", jsonResults.conditionsWithPurpose.find(function (i) {
    return i.purpose === "ZSS2";
}).value);
```
![step-11-on-price-1](step-11-on-price-1.jpg)
...
![step-11-onPrice-3](step-11-onPrice-3.png)
...
![step-11-onPrice-4](step-11-onPrice-4.png)

Run your application. The selected options and price values will change depending on the menu option you choose. The below result is excluding the `External Conditions` in the input for stateless pricing API. If the `External Conditions` are included then the results may vary.

![step-12-sample-UI5](step-12-sample-UI5.png)

Congratulations! You have successfully completed the tutorial. We hope that you find it useful and it helps you to start discovering our services.


>In the example above, possible values of the characteristic are read at the end only from the knowledge base. List of possible values can change during runtime, therefore in a real world example possible values from configuration results must be considered.

>In the example above, the sandbox environment of the API Hub is used with an API key when calling the services. In the productive environment, OAuth authentication with client credentials would be used.

>In the example above, the configuration service is called without providing session context. For optimal performance and resource consumption, please ensure that the cookie retrieved with each configuration creation (implementation in the `onInit` event listener of the function in our example) is used when calling the other configuration service APIs for the same configuration session (functions `onChange`, `readKB`, and `getConfig` in our example).

```JavaScript
self.getView().getModel().setProperty("/cookie", this.getResponseHeader("set-cookie"));

xhr.setRequestHeader("Cookie", this.getView().getModel().getProperty("/cookie"));
```

Please read the development guide (https://help.sap.com/viewer/p/SAP_VARIANT_CONFIGURATION_AND_PRICING) for more information about how to use the services.


[DONE]
[ACCORDION-END]


---
