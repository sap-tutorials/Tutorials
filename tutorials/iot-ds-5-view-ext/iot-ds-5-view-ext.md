---
title: Extend Decision Support Application with Custom SAPUI5 View
description: Extend the SAP IoT Decision Support application using SAP Web IDE by adding a custom SAPUI5 view to display device event payload data.
author_name: Jitendra Sharma
author_profile: https://github.com/JitendraSharma01
auto_validation: true
time: 20
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, topic>sapui5, products>sap-internet-of-things, products>sap-business-technology-platform, products>sap-web-ide ]
---

## Prerequisites
 - Complete tutorial [Building the Decision Support UI in the SAP Web IDE](iot-ds-3-create-ui)
 - Complete tutorial [Build, Deploy and Test Notification and Decision Support application](iot-ds-4-build-test)

## Details
### You will learn
  - How to extend a decision support application with custom SAPUI5 view
  - How to expose device event payload data in decision support application

---

[ACCORDION-BEGIN [Step 1: ](Configure externalized data in decision support definition)]

  In order to display device event payload data to a custom SAPUI5 view, the person configuring the decision support action must explicitly expose the data by defining externalized data configuration in decision support definition.

  1. From SAP IoT launchpad, launch the **Decision Support Definition** application.

    ![Select Decision Support from SAP Fiori launchpad](/images/launchpad_tile_ds_1.png)

  2. Select `High Greenhouse Temperature` from the list of decision support definitions.

  3. In the **Definition** page, create a new entry for **Externalized Data**.

    ![Create new externalized data](/images/ds_config_ext_data_1.png)

  4. Enter a value for the field **Alias**. In this example, you'll enter the value `DEVICE`. This value will be used in the controller of the custom SAPUI5 view.  

  5. Enter the binding path for this alias.  This path should be defined according to the JSON structure of the payload defined in the decision support action.

    Here is the payload that you have defined in the decision support action in step 3 of [Model the needed Decision Support based on IoT data](iot-ds-1-define-actions).

    ![Display of a sample payload](/images/ds_config_ext_data_2_1.png)

    The value of the field **Data Binding** should have the format:

    `{ Path of exposed data in JSON payload }`

    In this example, you would like to expose the data under `properties.device`, you enter the value `{properties.device}` in the field.

    ![Enter configuration for externalized data](/images/ds_config_ext_data_3.png)

  6. Select the field **User Interface Relevant**.

  7. Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add view extension to application descriptor)]

  1. Open the project with decision support application in SAP Web IDE.

  2. Open the ``manifest.json`` in the **Code Editor**.

    ![Open manifest.json in code editor](/images/ds_view_ext_1.png)

  3. Update the value of JSON key ``sap.ui5.extends.extensions.sap.ui.viewExtensions.sap.suite.ui.generic.template.ObjectPage.view.Details`` as follows:

    ```JSON
    "BeforeSubSection|RSInsts|ColumnFacet": {
      "className": "sap.ui.core.mvc.View",
      "viewName": "<HTML5 Module Name>.ext.view.EventInfo",
      "viewData": {
        "settings": {
          "modelName": "sharedJSON"
        }
      },
      "type": "XML",
      "sap.ui.generic.app": {
        "title": "",
        "enableLazyLoading": true
      }
    }
    ```

  4. Replace `<HTML5 Module Name>` with the name of the decision support HTML5 module.  You can find this name from the ``sap.app.id`` property in the same manifest.json.  In this example, it's `greenhousedecisionsupport`.

    ![Highlight of application id](/images/ds_view_ext_3.png)

    After replacing `<HTML5 Module Name>`:

    ![Add view extension configuration](/images/ds_view_ext_2.png)

  5. **Save** the changes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add files for custom view)]

  1. Under the `webapp` folder add an `ext` folder.

    ![Add new folder in project](/images/ds_view_ext_4.png)

    ![Enter name for new folder](/images/ds_view_ext_5.png)

  2. Inside `ext` folder, add 2 additional folders `controller` and `view`.

  3. Inside the `controller` folder, add a new file named `EventInfo.controller.js`.

    ![Add new file to project](/images/ds_view_ext_6.png)

    ![Enter name for new file](/images/ds_view_ext_7.png)

  4. Copy the following content into the file:

    ```JavaScript
    (function () {
    	"use strict";
    	/*global sap, jQuery */

    	sap.ui.controller("<HTML5 Module Name>.ext.controller.EventInfo", {
    		onBeforeRendering:function() {
    			if (!this._initModel) {
    				this._initModel = true;
    				var sharedModel = this.getView().getModel("sharedJSON");
    				this.getView().setModel(sharedModel);
    				this.getView().bindElement("/extData/<EXTERNALIZED_DATA_ALIAS>");
    			}
    		}
    	});
    })();            
    ```

  5. Replace `<HTML5 Module Name>` with sap.app.id in manifest.json.  It's the same name used in Step 2.

  6. Replace `<EXTERNALIZED_DATA_ALIAS>` with the alias defined in Step 1 of this tutorial.

  7. **Save** the file.

    ![Updated controller file](/images/ds_view_ext_8.png)  

  8. Inside the `view` folder, add a new file named `EventInfo.view.xml` with the following content:

    ```XML
    <mvc:View controllerName="<HTML5 Module Name>.ext.controller.EventInfo"
    	xmlns:mvc="sap.ui.core.mvc" xmlns="sap.m"
    	xmlns:f="sap.ui.layout.form"
    	xmlns:core="sap.ui.core">
    		<f:SimpleForm editable="false" layout="ColumnLayout" title="" columnsM="2" columnsL="3" columnsXL="4">
    			<f:content>
            <core:Title text="{@i18n>deviceInformation}" />
    				<Label text="{@i18n>Exception}" />
    				<Text text="{exception}" />
    				<Label text="{@i18n>ServicePriorityCode}" />
    				<Text text="{ServicePriorityCode}" />
    				<Label text="{@i18n>ProcessingTypeCode}" />
    				<Text text="{ProcessingTypeCode}" />
    				<Label text="{@i18n>Customer}" />
    				<Text text="{CustomerID}" />
    				<core:Title text="{@i18n>additionalInfo}" />
    				<Label text="{@i18n>InstallationPointId}" />
    				<Text text="{InstallationPointID}" />
    				<Label text="{@i18n>DataOriginTypeCode}" />
    				<Text text="{DataOriginTypeCode}" />
    			</f:content>
    		</f:SimpleForm>
    </mvc:View>    
    ```

  9. Replace `<HTML5 Module Name>` with sap.app.id defined in `manifest.json`.

  10. **Save** the file.

    ![Update view xml file](/images/ds_view_ext_9.png)  


  For additional information on SAPUI5 model view controller programming model, see [Model View Controller](https://sapui5.hana.ondemand.com/#/topic/91f233476f4d1014b6dd926db0e91070.html).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Add translation texts)]

  1. Add the following content in the `i18n.properties` file in the `webapp/i18n` folder:

    ```Properties
    #XFLD
    deviceInformation=Device Information

    #XFLD
    Exception=Exception Code

    #XFLD
    ServicePriorityCode=Service Priority

    #XFLD
    ProcessingTypeCode=Processing Type

    #XFLD
    Customer=Customer Id

    #XFLD
    additionalInfo=Additional Information

    #XFLD
    InstallationPointId=Installation Point

    #XFLD
    DataOriginTypeCode=Data Origin Type  
    ```

  2. **Save** the file.

    ![Updated translation file](/images/ds_view_ext_11.png)  

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test custom SAPUI5 view)]

Please follow in instructions in [Build, Deploy and test Notification and Decision Support application](iot-ds-4-build-test).

>**A new Notification has to be generated in order for the new Externalized Data definition to take effect.**  If you open a notification that was generated before the Externalized Data definition was defined, the fields in the custom view will not display any values.

Decision support application with a custom SAPUI5 view displaying data from the device event payload.

![Decision Support runtime with view extension](/images/ds_view_ext_10.png)  


[VALIDATE_1]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Add custom image to page header)]

  1. Import an image to `webapp/asset` folder of the decision support application in SAP Web IDE.

    ![Import a new image file to project](/images/ds_view_ext_12.png)  

  2. Select the image from your system.  Click **OK**.

    ![Select the image file from local file system](/images/ds_view_ext_13.png)  

  3. Open the file `annotations.xml` in `webapp` folder.

  4. Add the following annotation in the section `HeaderInfoType`.

    ```XML
    <PropertyValue Property="ImageUrl" String="asset/your image name"/>
    ```

    ![Update annotation](/images/ds_view_ext_14.png)  

  5. **Save** the file.

  6. To test the changes, please follow the instructions in [Build, Deploy and test Notification and Decision Support application](http://www.google.com).

    ![Decision Support runtime with header image](/images/ds_view_ext_15.png)  


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add navigation to action history details (optional))]

You can provide a link as part of the action history entry that allows a user to navigate to a page which has additional information about the result of the executed option. To enter a link for action history:

  1. From SAP IoT launchpad, select the **Decision Support Definition** tile.

    ![Select Decision Support in SAP Fiori launchpad](/images/launchpad_tile_ds_1.png)

  2. Select a decision support configuration.

  3. Select an existing option under **Possible Actions** section.

  4. In the details page of the selected option, click **Edit** on the header toolbar.

  5. Enter a link in the field **Action History Intent or URL**.

    ![Configuration of action history](/images/ds_option_hist_intent_1.png)

    * The link can be an URL (starts with http or https) or a SAP Fiori intent (e.g. MySalesOrder-display)

    * The link can include values returned in the response of the executed option using the notation {result.somefield}. Please consult the documentation of each service for the field names in the response.

        For example, `ObjectID` is a name of one of the fields returned in the response from the executed option and its value is bound to the parameter `internalID` of the link to a service ticket: `https://mytenant.crm.ondemand.com/sap/ap/ui/clogin?internalID=` **{result.ObjectID}** `&type=COD_SRQ_AGENT_TT`.

    * The string can also include values from event payload as in the other fields in decision support configuration. Please see step 3 of [Model the Needed Decision Support Based on IoT Data](iot-ds-1-define-actions) for more information on the definition of the event payload.

        For example:
        `https://mytenant.crm.ondemand.com/sap/ap/ui/clogin?internalID=` **{properties.device.CustomerID}** `&type=COD_SRQ_AGENT_TT`.

  6. Click **Save**

  7. A link **View Object** will be displayed on the action history entry for the configured option.

    ![Display of View Object link in action history](/images/ds_option_hist_intent_2.png)

[DONE]
[ACCORDION-END]

---
