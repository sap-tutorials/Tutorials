---
title: Change Point Detection with SAP Leonardo Machine Learning Foundation in SAPUI5
description: Discover how to implement SAP Leonardo Machine Learning Functional Service in a SAPUI5 application
primary_tag: products>sap-leonardo-machine-learning-foundation
tags: [ tutorial>beginner, topic>sapui5, topic>html5, topic>machine-learning, products>sap-leonardo-machine-learning-foundation, products>sap-api-management, products>sap-cloud-platform, products>sap-web-ide]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - [Sign up for an free trial account on the SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
 - Select your next tutorial from these SAP Leonardo Machine Learning groups: [SAP API Business Hub](https://www.sap.com/developer/groups/ml-fs-api-hub.html), [Java](https://www.sap.com/developer/groups/ml-fs-java.html) or [SAPUI5](https://www.sap.com/developer/groups/ml-fs-sapui5.html)
 - Select a tutorial group from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)

## Details
### You will learn  
In this tutorial, you will learn how to quickly integrate the **Time Series Change Point Detection** SAP Leonardo Machine Learning Functional Services published from the SAP API Business Hub sandbox in a SAPUI5 application.

The **Time Series Change Point Detection** service allows you to calculates and returns the probability of the change point for each data point in the provided univariate or multivariate time series.

You will then be able to substitute the **Time Series Change Point Detection** services with any other SAP Leonardo Machine Learning Functional Services that consumes text content.

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Get Your Sandbox URL)]

In order to consume the **Time Series Change Point Detection** Machine Learning Functional Services, you will first need to get the service URI, your API Key and the request and response parameters.

Go to [https://api.sap.com/](https://api.sap.com) and click on the **Browse** tile.

![SAP API Business Hub](01.png)

Then you will be able to search for the **SAP Leonardo Machine Learning - Functional Services**, then click on the package found.

![SAP API Business Hub](02.png)

Click on **Artifacts**, then click on the **Time Series Change Point Detection API**.

![SAP API Business Hub](03.png)

As you can notice the **Time Series Change Point Detection API** has only one resource (or service): `/inference_sync`.

Now click on the **Overview** tab.

> **Note**: the term *inference* refers to the application phase (scoring) an existing model (as opposed to the training or inception phase) and *sync* for synchronous.

![SAP API Business Hub](04.png)

As displayed on the screen, the sandbox URL for the **Time Series Change Point Detection API** where we need to append the API resource:

```JSON
https://sandbox.api.sap.com/ml/changepointdetection/inference_sync
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get Your API key )]

When using any of the APIs outside of the SAP API Business Hub, an application key will be needed in every request header of your APIs calls.

To get to your API key, click on the ![key](00-key.png) icon in the top right corner of the page. Click on the key icon.

The following pop-up should appear. Click on the **Copy API Key** button and save it in a text editor.

![SAP API Business Hub](05.png)

Now, let's build a SAPUI5 application! But before doing so let's first add the destination to connect to the SAP API Business Hub.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Access the SAP Cloud Platform Cockpit)]

Go to your [***SAP Cloud Platform Cockpit***](https://account.hanatrial.ondemand.com/cockpit#/region/neo-eu1-trial/overview) account and access "Your Personal Developer Account".

![SAP Cloud Platform Cockpit](06.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure your destination)]

You will need to create a destination in your SAP Cloud Platform account that allow will your applications to connect to external APIs such as the SAP API Business Hub.

On the left side bar, you can navigate in **Connectivity** > **Destinations**.

![Your Personal Developer Account](07.png)

On the ***Destinations*** overview page, click on **New Destination**

![Destinations](08.png)

Enter the following information:

Field Name           | Value
-------------------- | --------------
Name                 | `sapui5ml-api`
Type                 | `HTTP`
Description          | `SAP Leonardo Machine Learning APIs`
URL                  | `https://sandbox.api.sap.com/ml`
Proxy Type           | `Internet`
Authentication       | `NoAuthentication`

Then you will need to add the following properties to the destination:

Property Name          | Value
---------------------- | --------------
`WebIDEEnabled`        | `true`

Click on **Save**

![New Destinations](09.png)

You can use the **Check Connectivity** button ![HTML5 Applications](00-check.png) next to the new **Destination** to validate that the URL can be accessed.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open the Web IDE)]

On the left side bar, you can navigate in **Services**, then using the search box enter `Web IDE`.

![Web IDE](10.png)

Click on the tile, then click on **Open SAP Web IDE**.

![Web IDE](11.png)

You will get access to the **SAP Web IDE** main page:

![Web IDE](12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create your application using the SAPUI5 template)]

Click on **New Project from Template** in the ***Create Project*** section or use the **File** > **New** > **Project from Template**.

![Project](12.png)

Select the **SAPUI5 Application** tile, then click on **Next**

![Project](13.png)

Enter the following information, then click on **Next**

Field Name           | Value
-------------------- | --------------
Project Name         | `sapui5ml-changepointdetection`
Namespace            | `demo`

![Project](14.png)

Enter the following information, then click on **Finish**

Field Name           | Value
-------------------- | --------------
View Type            | `XML`
View Name            | `demo`

![Project](15.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Extend the application resource roots)]

In order to ease the use of the provided code, we will add a new SAPUI5 resource roots. The main reason for this is that the rule used to generate the initial resource root by the project template has change many time over the time.

Edit the `index.html` file located under **`Workspace`** > **`sapui5ml`** > **`webapp`** and add the below element to the existing `data-sap-ui-resourceroots` property around line 15 (don't forget the comma in between the existing element and the new one).

```JavaScript
"sapui5ml": ""
```

It should eventually look something like this:

```JavaScript
data-sap-ui-resourceroots='{"demosapui5ml-changepointdetection": "", "sapui5ml": ""}'
```

Click on the ![Save Button](00-save.png) button (or press CTRL+S).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Configure your SAPUI5 application)]

In order to use the previously configured destination, we need to add its declaration into the `neo-app.json` file along with the header white list configuration that will prevent HTTP header parameters to be filtered out.

Edit the `neo-app.json` file located under **`Workspace`** > **`sapui5ml-changepointdetection`** and replace the current content with the below code.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JSON
{
  "welcomeFile": "/webapp/index.html",
  "routes": [{
    "path": "/resources",
    "target": {
      "type": "service",
      "name": "sapui5",
      "entryPath": "/resources"
    },
    "description": "SAPUI5 Resources"
  }, {
    "path": "/test-resources",
    "target": {
      "type": "service",
      "name": "sapui5",
      "entryPath": "/test-resources"
    },
    "description": "SAPUI5 Test Resources"
  }, {
    "path": "/ml",
    "target": {
      "type": "destination",
      "name": "sapui5ml-api"
    },
    "description": "ML API destination"
  }],
  "sendWelcomeFileRedirect": true,
  "headerWhiteList": [
    "APIKey"
  ]
}
```

> ### **Note:** `headerWhiteList`
>
>By default, headers element like the `APIKey` will be blocked when used in a SAPUI5 control like the `FileUploader`. This is the reason why we add it to the white list.
>

&nbsp;

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Store your API setting in a JSON model)]

There are multiple options to achieve this goal. Here we will use a pre-loaded JSON model configured in the `manifest.json` file.

Create a new file named `demo.json` under **`Workspace`** > **`sapui5ml-changepointdetection`** > **`webapp`** > **`model`**, copy the below code and make sure you replace `<<<<< COPY YOUR API KEY >>>>>` by your the API key we retrieved in step 2.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JSON
{
  "url": "/ml/changepointdetection/inference_sync",
  "APIKey": "<<<<< COPY YOUR API KEY >>>>>",
  "options": {
    "separator": "|",
    "series_separator": "#"
  }
}
```

Edit the `manifest.json` file located under **`Workspace`** > **`sapui5ml-changepointdetection`** > **`webapp`** and locate the `models` section (around line 55), and update the section like this:

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JSON
"models": {
  "i18n": {
    "type": "sap.ui.model.resource.ResourceModel",
    "settings": {
      "bundleName": "demosapui5ml-changepointdetection.i18n.i18n"
    }
  },
  "demo": {
    "type": "sap.ui.model.json.JSONModel",
    "preload": true,
    "uri": "model/demo.json"
  }
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Extend the main SAPUI5 view)]

The view will contain a table to display the results along with a canvas to display the selected image (if a single one is selected) and 2 buttons, one to import a snapshot and the other one to take snapshot using the webcam (if any, this button won't be visible on mobile device because it is not supported).

Edit the `demo.view.xml` file located under **`Workspace`** > **`sapui5ml-changepointdetection`** > **`webapp`** > **`view`** and replace the existing code with the below code.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```XML
<mvc:View xmlns:html="http://www.w3.org/1999/xhtml" xmlns:mvc="sap.ui.core.mvc" xmlns:form="sap.ui.layout.form" xmlns="sap.m"
  controllerName="sapui5ml.controller.demo" displayBlock="true">
  <App>
    <pages>
      <Page title="Change Point Detection">
        <content>
          <HBox width="100%">
            <items>
              <VBox width="100%" direction="Column">
                <items>
                  <HBox width="100%">
                    <items>
                      <VBox alignItems="End" width="50%">
                        <items>
                          <Text textAlign="End" class="sapUiSmallMargin" text="Number of data point"/>
                          <Text textAlign="End" class="sapUiSmallMargin" text="Number of series"/>
                        </items>
                      </VBox>
                      <VBox alignItems="Start" width="50%">
                        <items>
                          <StepInput value="{demo>/options/numDataPoints}" required="true"/>
                          <StepInput value="{demo>/options/numDataSeries}" required="true"/>
                        </items>
                      </VBox>
                    </items>
                  </HBox>
                  <VBox width="100%" alignItems="Center">
                    <items>
                      <Button text="Generate Sample Data" press="onGenerate"/>
                    </items>
                  </VBox>
                </items>
              </VBox>
              <VBox width="100%" alignContent="Start" alignItems="Start" justifyContent="Start">
                <items>
                  <HBox width="100%" visible="{= ${demo>/hasData} === true}">
                    <items>
                      <VBox alignItems="End" width="50%">
                        <items>
                          <Text textAlign="End" class="sapUiSmallMargin" text="Values separator"/>
                          <Text textAlign="End" class="sapUiSmallMargin" text="Series separator for multivariate time series"/>
                        </items>
                      </VBox>
                      <VBox alignItems="Start" width="50%">
                        <items>
                          <Input type="Text"
                            value="{ path : 'demo>/options/separator', type : 'sap.ui.model.type.String', constraints : { minLength: 1, maxLength: 1 } }"/>
                          <Input type="Text"
                            value="{ path : 'demo>/options/series_separator', type : 'sap.ui.model.type.String', constraints : { minLength: 1, maxLength: 1 } }"/>
                        </items>
                      </VBox>
                    </items>
                  </HBox>
                  <VBox width="100%" alignItems="Center" visible="{= ${demo>/hasData} === true}">
                    <items>
                      <Button text="Execute Service" press="onExecute"/>
                    </items>
                  </VBox>
                </items>
              </VBox>
            </items>
          </HBox>
          <HBox width="100%" alignItems="Start" justifyContent="Start" visible="{= ${demo>/hasData} === true}">
            <items>
              <VBox width="50%">
                <items>
                  <Table columns="{demo>/cols}" items="{path: 'demo>/rows'}">
                    <items>
                      <ColumnListItem cells="{demo>cols}">
                        <cells>
                          <Input value="{demo>}"/>
                        </cells>
                      </ColumnListItem>
                    </items>
                    <columns>
                      <Column>
                        <header>
                          <Text text="{demo>colid}"/>
                        </header>
                      </Column>
                    </columns>
                  </Table>
                </items>
              </VBox>
              <VBox width="50%" visible="{= ${demo>/hasResult} === true}">
                <items>
                  <Table columns="{demo>/resultCols}" items="{path: 'demo>/resultRows'}" visible="{= ${demo>/hasResult} === true}">
                    <items>
                      <ColumnListItem cells="{demo>cols}">
                        <cells>
                          <Input value="{demo>}" editable="false"/>
                        </cells>
                      </ColumnListItem>
                    </items>
                    <columns>
                      <Column>
                        <header>
                          <Text text="Result series: {demo>colid}"/>
                        </header>
                      </Column>
                    </columns>
                  </Table>
                </items>
              </VBox>
            </items>
          </HBox>
        </content>
      </Page>
    </pages>
  </App>
</mvc:View>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Extend the main SAPUI5 controller)]

Edit the `demo.controller.js` file located under **`Workspace`** > **`sapui5ml-changepointdetection`** > **`webapp`** > **`controller`** and replace the existing code with the below code.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JavaScript
sap.ui.define([
  "sap/ui/core/mvc/Controller",
  "sap/m/MessageBox",
  "sap/m/Image"
], function(Controller, MessageBox, Image) {
  "use strict";
  return Controller.extend("sapui5ml.controller.demo", {

    onGenerate: function(oControlEvent) {
      // get the current view
      var oView = this.getView();

      // start the busy indicator
      var oBusyIndicator = new sap.m.BusyDialog();
      // keep a reference in the view to close it later
      oBusyIndicator.open();

      // get a random number
      var rnd = Math.random();

      // get the expected data points count
      var iRowCount = oView.getModel("demo").getProperty("/options/numDataPoints");
      var iColCount = oView.getModel("demo").getProperty("/options/numDataSeries");

      // build the array to store the generated data
      var oSeriesCol = new Array(iColCount);
      var oSeriesRow = new Array(iRowCount);
      for (var iCol = 0; iCol < iColCount; iCol++) {
        var oCol = {
          "colid": iCol
        };
        oCol.rows = new Array(iRowCount);
        oSeriesCol[iCol] = (oCol);
      }
      for (var iRow = 0; iRow < iRowCount; iRow++) {
        var oRow = {
          "rowid": iRow
        };
        oRow.cols = new Array(iColCount);
        oSeriesRow[iRow] = (oRow);
      }

      // generate the data
      for (var iColValue = 0; iColValue < iColCount; iColValue++) {
        for (var iRowValue = 0; iRowValue < iRowCount; iRowValue++) {
          var value = (Math.cos((1 + iRowValue) * rnd * (1 + iColValue) * 20) * 100).toFixed(2);
          oSeriesCol[iColValue].rows[iRowValue] = (value);
          oSeriesRow[iRowValue].cols[iColValue] = (value);
        }
      }

      // save it in the model
      oView.getModel("demo").setProperty("/cols", oSeriesCol);
      oView.getModel("demo").setProperty("/rows", oSeriesRow);
      oView.getModel("demo").setProperty("/hasData", true);
      oBusyIndicator.close();
    },

    onExecute: function(oControlEvent) {
      // start the busy indicator
      var oBusyIndicator = new sap.m.BusyDialog();
      // keep a reference in the view to close it later
      oBusyIndicator.open();

      // get the current view
      var oView = this.getView();

      // get service settings and options
      var url = oView.getModel("demo").getProperty("/url");
      var APIKey = oView.getModel("demo").getProperty("/APIKey");
      var separator = oView.getModel("demo").getProperty("/options/separator");
      var seriesSeparator = oView.getModel("demo").getProperty("/options/series_separator");

      // generate the options as JSON stringify
      var options = {
        "separator": oView.getModel("demo").getProperty("/options/separator"),
        "series_separator": oView.getModel("demo").getProperty("/options/series_separator")
      };
      oView.getModel("demo").setProperty("/optionsJs", JSON.stringify(options));

      // convert the input data in the proper format using the separator & seriesSeparator options
      var text = "";
      var series = oView.getModel("demo").getProperty("/cols");
      for (var i = 0; i < series.length; i++) {
        text += series[i].rows.join(separator);
        if (i < series.length - 1) {
          text += seriesSeparator;
        }
      }
      // call the service and define call back methods
      $.ajax({
        headers: {
          'Accept': 'application/json',
          'APIKey': APIKey
        },
        url: url,
        type: "POST",
        data: $.param({
          "options": "{\"separator\":\"" + oView.getModel("demo").getProperty("/options/separator") + "\", \"series_separator\":\"" +
            oView.getModel("demo").getProperty("/options/series_separator") + "\"}",
          "texts": text
        }),
        async: false,
        success: function(data) {
          try {
            //get the result size
            var iRowCount = data.predictions[0].split(",").length;
            var iColCount = data.predictions.length;

            var oSeriesCol = new Array(iColCount);
            var oSeriesRow = new Array(iRowCount);

            // build the array to store the result data
            for (var iCol = 0; iCol < data.predictions.length; iCol++) {
              var oCol = {
                "colid": iCol
              };
              oCol.rows = new Array(iRowCount);
              oSeriesCol[iCol] = (oCol);
            }
            for (var iRow = 0; iRow < iRowCount; iRow++) {
              var oRow = {
                "rowid": iRow
              };
              oRow.cols = new Array(iColCount);
              oSeriesRow[iRow] = (oRow);
            }

            // get the reslt data
            for (var iColValue = 0; iColValue < data.predictions.length; iColValue++) {
              var row = data.predictions[0].split(",");
              for (var iRowValue = 0; iRowValue < row.length; iRowValue++) {
                oSeriesCol[iColValue].rows[iRowValue] = row[iRowValue];
                oSeriesRow[iRowValue].cols[iColValue] = row[iRowValue];
              }
            }

            // save it in the model
            oView.getModel("demo").setProperty("/resultCols", oSeriesCol);
            oView.getModel("demo").setProperty("/resultRows", oSeriesRow);
            oView.getModel("demo").setProperty("/hasResult", true);
            oBusyIndicator.close();
          } catch (err) {
            MessageBox.show("Caught - [ajax error] : " + err.message);
          }
          oBusyIndicator.close();
        },
        error: function(request, status, error) {
          MessageBox.show("Caught - [ajax error] : " + request.responseText);
          oBusyIndicator.close();
        }
      });
      oBusyIndicator.close();
    }
  });
});
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Test the application)]

Click on the **Run** icon ![Run Applications](00-run.png) or press `ALT+F5`.

Click on the **Generate Sample Data** button to get a cosine data series generated, then click on **Execute Service**.

The service will be called, and the result displayed in a table.

You will notice that the longer the series is, the lower the probability will be. This is because the algorithm had learned the patter in place within our data.

![Result](16.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 13: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Solution: ](Project files)]

In case you are having problems when running the application, the complete project code can be found on the SAP Tutorial public [GitHub repository](https://github.com/SAPDocuments/Tutorials/tree/master/tutorials/ml-fs-sapui5-series-changepoint-detection/sapui5ml-changepointdetection).

However, this is not a repository you can clone and run the code.

You have to import the `sapui5ml-imageclassifier` directory content into your existing project directory.

Make sure you check the [LICENSE](https://github.com/SAPDocuments/Tutorials/blob/master/LICENSE.txt) before starting using its content.

[DONE]
[ACCORDION-END]

## Next Steps
 - Select your next tutorial from these SAP Leonardo Machine Learning groups: [SAP API Business Hub](https://www.sap.com/developer/groups/ml-fs-api-hub.html), [Java](https://www.sap.com/developer/groups/ml-fs-java.html) or [SAPUI5](https://www.sap.com/developer/groups/ml-fs-sapui5.html)
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)
