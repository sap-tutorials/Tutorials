---
title: SAP HCP predictive services, Use the synchronous Forecast HCP predictive service from a SAPUI5 application
description: You will extend your application with the use the synchronous mode from the "Forecast" HCP predictive service
tags: [ tutorial>intermediate, products>sap-hana, products>sap-hana-cloud-platform, products>sap-hana-cloud-platform-predictive-services, topic>predictive, topic>sapui5 ]
---

## Prerequisites
  - **Proficiency:** Intermediate
  - **Tutorials:** [Manage your "Data Set" in the HCP predictive service from a SAPUI5 application](http://go.sap.com/developer/tutorials/hcpps-sapui5-ps-dataset-manage.html)

## Next Steps
  - [Use the asynchronous Forecast HCP predictive service from a SAPUI5 application](http://go.sap.com/developer/tutorials/hcpps-sapui5-ps-forecast-asynchronous.html)

## Details
### You will learn
  - How to implement the synchronous mode of the "Forecast" HCP predictive service in a SAPUI5 application

### Time to Complete
It should take around **10 minutes** to complete this tutorial.

---

1. Log into the [***SAP HANA Cloud Platform Cockpit***](http://account.hanatrial.ondemand.com/cockpit) with your free trial account and access "Your Personal Developer Account".

    Click on your ***HCP Account*** identifier (which ends with *trial*) as highlighted on the below screenshot.

    ![SAP HANA Cloud Platform Cockpit](1.png)

1. On the left side bar, you can navigate in **Applications** > **HTML5 Applications**.

    ![HTML5 Applications](2.png)

1. Click on the **Edit Application** ![HTML5 Applications](3-1.png) icon for the `hcppredictiveservicesdemo` application.

    ![HTML5 Applications](3.png)

1. This will open the ***SAP Web IDE*** where you have previously created the `hcppredictiveservicesdemo` application using the project template.

    ![HTML5 Applications](4.png)

1. Create a new file called `ForecastForm.fragment.xml` in the `hcppredictiveservicesdemo\webapp\fragment\forecast` and add the following content.

    This fragment will be used to display a form where the user can enter the parameters to run the "Forecast" service. As we will be reusing the same form for the asynchronous mode, it is a better practice to create it as a fragment.

    ```XML
    <core:FragmentDefinition xmlns:core="sap.ui.core" xmlns="sap.m" xmlns:form="sap.ui.layout.form">
      <form:SimpleForm editable="false" layout="ResponsiveGridLayout" class="editableForm">
        <form:content>
          <Label text="Identifier" labelFor="idInputForecastDatasetID" class="sapUiSmallMarginTop sapUiSmallMarginBottom"></Label>
          <Input id="idInputForecastDatasetID" value="{/dataSetData/ID}" enabled="false"/>
          <Label text="Date Column" labelFor="idInputForecastDateColumn" class="sapUiSmallMarginTop sapUiSmallMarginBottom"></Label>
          <Select id="idInputForecastDateColumn" forceSelection="true"
            items="{ path: '/dataSetData/variables', sorter: { path: 'name' }, filters: [{path: 'storage', operator: 'EQ', value1: 'date'}]}">
            <core:Item key="{name}" text="{name}"/>
          </Select>
          <Label text="Forecast Column" labelFor="idInputForecastTargetColumn" class="sapUiSmallMarginTop sapUiSmallMarginBottom"></Label>
          <Select id="idInputForecastTargetColumn" forceSelection="true"
            items="{ path: '/dataSetData/variables', sorter: { path: 'name' }, filters: [{path: 'value', operator: 'EQ', value1: 'continuous'}, {path: 'storage', operator: 'EQ', value1: 'number'}]}">
            <core:Item key="{name}" text="{name}"/>
          </Select>
          <Label text="Reference Date" labelFor="idInputForecastReferenceDate" class="sapUiSmallMarginTop sapUiSmallMarginBottom"></Label>
          <DatePicker id="idInputForecastReferenceDate" value="{/dataSetData/referenceDate}" placeholder="Enter Date ..."/>
          <Label text="Number of Forecast" labelFor="idInputForecastNumberOfForecasts" class="sapUiSmallMarginTop sapUiSmallMarginBottom"></Label>
          <Input id="idInputForecastNumberOfForecasts" value="{/dataSetData/numberOfForecasts}"/>
        </form:content>
      </form:SimpleForm>
    </core:FragmentDefinition>
    ```

1. Create a new file called `ForecastResult.fragment.xml` in the `hcppredictiveservicesdemo\webapp\fragment\forecast` and add the following content.

    This fragment will be used to display the "Forecast" services results, including the prediction performance metrics.

    ```XML
    <core:FragmentDefinition xmlns:core="sap.ui.core" xmlns="sap.m" xmlns:form="sap.ui.layout.form" xmlns:table="sap.ui.table">
      <!-- A table with the Data set details. It will be populated when the button is pressed-->
      <form:SimpleForm editable="false" layout="ResponsiveGridLayout" class="editableForm">
        <form:content>
          <Label text="Data Set Identifier"/>
          <Text text="{/dataSetData/ID}"/>
          <Label text="Name"/>
          <Text text="{/dataSetData/name}"/>
          <Label text="Number Of Columns"/>
          <Text text="{/dataSetData/numberOfColumns}"/>
          <Label text="Number Of Rows"/>
          <Text text="{/dataSetData/numberOfRows}"/>
        </form:content>
      </form:SimpleForm>
      <!-- A table with the Model performances details. It will be populated when the button is pressed-->
      <form:SimpleForm editable="false" layout="ResponsiveGridLayout" class="editableForm">
        <form:content>
          <Label text="Quality Rating"></Label>
          <Text text="{/forecastResultData/modelPerformance/qualityRating}"/>
          <Label text="MAPE"></Label>
          <Text text="{/forecastResultData/modelPerformance/mape}"/>
        </form:content>
      </form:SimpleForm>
      <!-- A table with the forecasted data. It will be populated when the button is pressed-->
      <table:Table rows="{/forecastResultData/forecasts}" enableBusyIndicator="true" selectionMode="Single" visibleRowCount="5" width="100%">
        <table:columns>
          <table:Column sortProperty="date" filterProperty="date">
            <Label text="Date"/>
            <table:template>
              <Text text="{date}"/>
            </table:template>
          </table:Column>
          <table:Column sortProperty="forecastValue" filterProperty="forecastValue">
            <Label text="Forecasted value"/>
            <table:template>
              <Text text="{forecastValue}"/>
            </table:template>
          </table:Column>
          <table:Column>
            <Label text="Error Bar Lower Bound"/>
            <table:template>
              <Text text="{errorBarLowerBound}"/>
            </table:template>
          </table:Column>
          <table:Column>
            <Label text="Error Bar Higher Bound"/>
            <table:template>
              <Text text="{errorBarHigherBound}"/>
            </table:template>
          </table:Column>
        </table:columns>
      </table:Table>
    </core:FragmentDefinition>
    ```

1. Create a new file called `ForecastSynchronous.view.xml` in the `hcppredictiveservicesdemo\webapp\view\forecast` and add the following content.

    The view embeds the fragment created previously to display a form where the user can select the dataset to be used for the forecast call and additional service parameters.

    ```XML
    <mvc:View controllerName="demo.controller.forecast.ForecastSynchronous" xmlns:html="http://www.w3.org/1999/xhtml"
      xmlns:mvc="sap.ui.core.mvc" xmlns="sap.m" xmlns:core="sap.ui.core" xmlns:form="sap.ui.layout.form">
      <Panel expandable="true" expanded="false" headerText="Forecast with the HCP predictive services (Synchronous)" class="sapUiResponsiveMargin"
        width="auto" height="auto">
        <form:SimpleForm editable="true" layout="ResponsiveGridLayout" class="editableForm">
          <form:content>
            <Button text="Retrieve List" type="Default" press="onDataSetGetList"/>
          </form:content>
        </form:SimpleForm>
        <Panel expandable="false" expanded="true" visible="{= ${/function} === 'ForecastSynchronous'}">
          <Panel expandable="false" expanded="true" visible="{= typeof ${/dataSetListData} !== 'undefined'}">
            <core:Fragment fragmentName='demo.fragment.dataset.DatasetList' type='XML'/>
          </Panel>
          <Panel expandable="false" expanded="true" visible="{= typeof ${/dataSetData} !== 'undefined'}">
            <core:Fragment fragmentName='demo.fragment.forecast.ForecastForm' type='XML'/>
            <Button text="Forecast in Synchronous Mode" type="Default" press="onForecastSynchronous"/>
          </Panel>
          <Panel expandable="false" expanded="true" visible="{= typeof ${/forecastResultData} !== 'undefined'}">
            <core:Fragment fragmentName='demo.fragment.forecast.ForecastResult' type='XML'/>
          </Panel>
        </Panel>
      </Panel>
    </mvc:View>
    ```

1. Open the `ForecastSynchronous.controller.js` file in the `hcppredictiveservicesdemo\webapp\controller\forecast` directory and add the following after the last function (make sure you include a comma between each functions) .

    The controller includes the functions used to process the 'Press' events on the controls added in the view and process the `AJAX` calls to the HCP predictive services.
    It 'extends' the `DataSetList` JavaScript file created earlier as our view uses the `DatasetList` fragment.

    ```JavaScript
    sap.ui.define([
      "sap/ui/core/mvc/Controller",
      "sap/m/MessageToast",
      "demo/fragment/dataset/DatasetList"
    ], function(Controller, MessageToast, DatasetList) {
      "use strict";

      jQuery.sap.require("demo.fragment.dataset.DatasetList");

      return Controller.extend("demo.controller.forecast.ForecastSynchronous", {
        onDataSetGetList: function() {
          DatasetList.prototype.onDataSetGetList.apply(this, arguments);
          sap.ui.getCore().getModel().setProperty("/function", "ForecastSynchronous");
        },
        onDataSetListSelectionChanged: function(oControlEvent) {
          DatasetList.prototype.onDataSetListSelectionChanged.apply(this, arguments);
          sap.ui.getCore().getModel().setProperty("/function", "ForecastSynchronous");
        },
        onForecastSynchronous: function() {
          // set the busy indicator to avoid multi clicks
          var oBusyIndicator = new sap.m.BusyDialog();
          oBusyIndicator.open();

          // get the service parameters value
          var sDatasetID = this.getView("demo-forecast-form").byId("idInputForecastDatasetID").getValue();
          var sTargetColumn = this.getView().byId("idInputForecastTargetColumn").getSelectedKey();
          var sDateColumn = this.getView().byId("idInputForecastDateColumn").getSelectedKey();
          var sNumberOfForecasts = this.getView().byId("idInputForecastNumberOfForecasts").getValue();
          var sReferenceDate = this.getView().byId("idInputForecastReferenceDate").getValue();
          // define the service parameters
          var param = {
            datasetID: sDatasetID,
            targetColumn: sTargetColumn,
            dateColumn: sDateColumn,
            numberOfForecasts: sNumberOfForecasts,
            referenceDate: sReferenceDate
          };

          // call the service and define call back methods
          $.ajax({
            headers: {
              'Accept': 'application/json',
              'Content-Type': 'application/json'
            },
            url: "/HCPps/api/analytics/forecast/sync",
            type: "POST",
            data: JSON.stringify(param),
            dataType: "json",
            async: false,
            success: function(data) {
              try {
                //Save data set description data in the model
                sap.ui.getCore().getModel().setProperty("/forecastResultData", undefined);
                sap.ui.getCore().getModel().setProperty("/forecastJobData", undefined);
                sap.ui.getCore().getModel().setProperty("/forecastResultData", data);
                sap.ui.getCore().getModel().setProperty("/function", "ForecastSynchronous");
                oBusyIndicator.close();
              } catch (err) {
                MessageToast.show("Caught - onHCPpsForecastSynchyronous[ajax success] :" + err.message);
              }
              oBusyIndicator.close();
            },
            error: function(request, status, error) {
              MessageToast.show("Caught - onHCPpsForecastSynchyronous[ajax error] :" + request.responseText);
              oBusyIndicator.close();
            }
          });
        }
      });
    });
    ```

1. Edit the `demo.view.xml` file located in the `hcppredictiveservicesdemo\webapp\view` and replace the existing code by the following one:

    Here we simply extend the main view.

    ```XML
    <mvc:View controllerName="demo.controller.demo"
      xmlns:html="http://www.w3.org/1999/xhtml"
      xmlns:mvc="sap.ui.core.mvc"
      xmlns="sap.m">
      <App>
        <pages>
          <Page title="Developing with HCPps and SAPUI5">
            <content>
              <mvc:XMLView viewName="demo.view.forecast.ForecastSynchronous"/>
              <mvc:XMLView viewName="demo.view.dataset.DatasetManage"/>
              <mvc:XMLView viewName="demo.view.dataset.DatasetRegister"/>
              <mvc:XMLView viewName="demo.view.odata.ODataDisplay"/>
            </content>
          </Page>
        </pages>
      </App>
    </mvc:View>
    ```

1. You can save all modified files by pressing `CTRL+SHIFT+S`. Then, click on the **Run** icon ![Run Applications](0-run.png) or press `ALT+F5`.

    Click on **Retrieve List**, select an entry in the table, then click on **Forecast in Synchronous Mode**.

    Et voilà!

    ![TimeSeries Demo Applications](5.png)

## Next Steps
  - [Use the asynchronous Forecast HCP predictive service from a SAPUI5 application](http://go.sap.com/developer/tutorials/hcpps-sapui5-ps-forecast-asynchronous.html)
