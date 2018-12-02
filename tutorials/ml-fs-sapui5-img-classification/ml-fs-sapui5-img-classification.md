---
title: Image Classification with SAPUI5
description: Discover how to implement SAP Leonardo Machine Learning Foundation service in a SAPUI5 application
auto_validation: true
primary_tag: products>sap-leonardo-machine-learning-foundation
tags: [ tutorial>beginner, topic>sapui5, topic>html5, topic>machine-learning, products>sap-leonardo-machine-learning-foundation, products>sap-api-management, products>sap-cloud-platform, products>sap-web-ide]
time: 20
---

## Prerequisites  
 - [Sign up for an free trial account on the SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
 - Select your next tutorial from these SAP Leonardo Machine Learning groups: [SAP API Business Hub](https://www.sap.com/developer/groups/ml-fs-api-hub.html), [Java](https://www.sap.com/developer/groups/ml-fs-java.html) or [SAPUI5](https://www.sap.com/developer/groups/ml-fs-sapui5.html)

## Details
### You will learn  
In this tutorial, you will learn how to quickly integrate the **Image Classification** SAP Leonardo Machine Learning Functional Services published from the SAP API Business Hub sandbox in a SAPUI5 application.

The **Image Classification** service allows you to calculates and returns a list of classifications/labels along with their probabilities for a given image.

You will then be able to substitute the **Image Classification** services with any other SAP Leonardo Machine Learning Functional Services that consumes images content.

---


[ACCORDION-BEGIN [Step 1: ](Get The API Sandbox URL And API Key)]

In order to consume the **Image Classifier Service** SAP Leonardo Machine Learning Foundation service, you will first need to get the service URI and your API key, request and response parameters.

Go to [https://api.sap.com/](https://api.sap.com).

![SAP API Business Hub](01.png)

Then you will be able to search for the **SAP Leonardo Machine Learning - Functional Services**, then click on the package found.

![SAP API Business Hub](02.png)

Select **Inference Service for Customizable Image Classification**.

![SAP API Business Hub](03.png)

You can also access the page directly from the following address:

 - <https://api.sap.com/api/image_classification_api/resource>

![SAP API Business Hub](06.png)

To get to your API Sandbox URL, click on the **Details** tab.

The API Sandbox URL should be:

```JSON
https://sandbox.api.sap.com/ml/imageclassification
```

To get to your API key, click on the **Show API Key** button.

You will be prompted to login if you are not yet.

Then, the following pop-up should appear. Click on the **Copy Key and Close** button and save it in a text editor.

![SAP API Business Hub](06-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Analyze the service)]

As you can notice the API has only one resource (or service): `/inference_sync`.

Now click on the `/inference_sync` link to expand the section.

> **Note**: the term *inference* refers to the application phase (scoring) an existing model (as opposed to the training or inception phase) and *sync* for synchronous.

As stated in the description, the service accepts either:

 - an archive file with a zip/tar extensions containing multiple image files
 - a single image
 - a list of image as input

The service returns a classification list with its scores (confidence).

The supported image formats are ***JPEG***, ***PNG***, ***TIF*** or ***BMP*** (the actual content format is validated, so renaming files may simply not work).

The input file, files or archive file will be sent as a `FormData` query parameter in the service request.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Access the SAP Cloud Platform Cockpit)]

Log into the <a href="https://account.hanatrial.ondemand.com/cockpit#/region/neo-eu1-trial/overview" target="new"><b>SAP Cloud Platform Cockpit Neo Trial</b></a> with your free trial account on **Europe (Rot) - Trial** and access ***Your Personal Developer Account***.

Click on your ***SAP Cloud Platform Account*** identifier (which ends with *trial* by default) as highlighted on the below screenshot.

![SAP Cloud Platform Cockpit](07-1.png)

You are now in your ***SAP Cloud Platform developer*** account!

![Your Personal Developer Account](07-2.png)

> If you are unclear with what is your SAP Cloud Platform account name, you can refer to the following blog entry: [SAP Cloud Platform login, user name, account id, name or display name: you are lost? Not anymore!](https://blogs.sap.com/2017/01/31/sap-hana-cloud-platform-trial-login-name-user-name-account-name-account-identifier-you-are-lost-not-anymore/)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure your destination)]

You will need to create a destination in your SAP Cloud Platform account that allow will your applications to connect to external APIs such as the SAP API Business Hub.

On the left side bar, you can navigate in **Connectivity** > **Destinations**.

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

You should receive a ***connection established*** message with potentially a ***404: Not Found*** response which is normal.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open the Web IDE)]

On the left side bar, you can navigate in **Services**, then using the search box enter `Web IDE`.

![Web IDE](10.png)

Click on the tile, then click on **Go to Service**.

![Web IDE](11.png)

You will get access to the **SAP Web IDE** main page:

![Web IDE](12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create a project using the SAPUI5 template)]

Click on **New Project from Template** in the ***Create Project*** section or use the **File** > **New** > **Project from Template**.

![Project](12.png)

Select the **SAPUI5 Application** tile, then click on **Next**

![Project](13.png)

Enter the following information, then click on **Next**

Field Name           | Value
-------------------- | --------------
Project Name         | `sapui5ml-imageclassifier`
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

```
data-sap-ui-resourceroots='{"xxxx": "", "sapui5ml": ""}'
```

Click on the ![Save Button](00-save.png) button (or press CTRL+S).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Configure your SAPUI5 application)]

In order to use the previously configured destination, we need to add its declaration into the `neo-app.json` file along with the header white list configuration that will prevent HTTP header parameters to be filtered out.

Edit the `neo-app.json` file located under **`Workspace`** > **`sapui5ml-imageclassifier`** and replace the current content with the below code.

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
    "path": "/ml-dest",
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

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Store your API setting in a JSON model)]

There are multiple options to achieve this goal. Here we will use a pre-loaded JSON model configured in the `manifest.json` file.

Create a new file named **`demo.json`** under **`Workspace`** > **`sapui5ml-imageclassifier`** > **`webapp`** > **`model`**, copy the below code and make sure you replace `<<<<< COPY YOUR API KEY >>>>>` by your the API key we retrieved in step 2.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JSON
{
  "url" : "/ml-dest/imageclassification/classification",
  "APIKey":"<<<<< COPY YOUR API KEY >>>>>"
}
```

Edit the `manifest.json` file located under **`Workspace`** > **`sapui5ml-imageclassifier`** > **`webapp`** and locate the `models` section (around line 55), and update the section like this:

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JSON
"models": {
  "i18n": {
    "type": "sap.ui.model.resource.ResourceModel",
    "settings": {
      "bundleName": "demosapui5ml-imageclassifier.i18n.i18n"
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

The view will contain a canvas to display the selected image along with table to display the results.

Edit the `demo.view.xml` file located under **`Workspace`** > **`sapui5ml-imageclassifier`** > **`webapp`** > **`view`** and replace the existing code with the below code.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```XML
<mvc:View xmlns:mvc="sap.ui.core.mvc" xmlns:table="sap.ui.table" xmlns:unified="sap.ui.unified" xmlns="sap.m"
	xmlns:custom="http://schemas.sap.com/sapui5/extension/sap.ui.core.CustomData/1" controllerName="sapui5ml.controller.demo"
	displayBlock="true">
	<App>
		<pages>
			<Page title="Image Classification">
				<content>
					<Carousel pages="{demo>/result-imageclassifier}" width="100%" visible="{= ${demo>/resultVisible-imageclassifier} === true}">
						<pages>
							<VBox width="100%" direction="Column" alignItems="Center">
								<ScrollContainer height="100%" width="100%" horizontal="true" vertical="true" focusable="true">
									<Image tooltip="canvas" class="sapUiLargeMargin" src="{demo>fileURL}"/>
									<Label text="File name: {demo>name}" class="sapUiLargeMargin"></Label>
									<table:Table rows="{demo>results}" enableBusyIndicator="true" selectionMode="Single" visibleRowCount="5">
										<table:columns>
											<table:Column sortProperty="label" filterProperty="label">
												<Label text="Label"/>
												<table:template>
													<Link text="{demo>label}" href="https://www.google.com/search?q={demo>label}&amp;newwindow=1&amp;tbm=isch" target="search"/>
												</table:template>
											</table:Column>
											<table:Column sortProperty="score" filterProperty="score">
												<Label text="Score"/>
												<table:template>
													<Text text="{demo>score}"/>
												</table:template>
											</table:Column>
										</table:columns>
									</table:Table>
								</ScrollContainer>
							</VBox>
						</pages>
					</Carousel>
				</content>
				<footer>
					<Toolbar width="100%">
						<content>
							<unified:FileUploader buttonOnly="true" buttonText="Upload Picture" sameFilenameAllowed="true" multiple="false" fileType="zip,png,jpeg,jpg,bmp,tiff,tif" mimeType="application/x-zip-compressed,application/zip,application/octet-stream,image/png,image/jpg,image/jpeg,image/bmp,image/tiff" typeMissmatch="fileTypeMissmatch" change="fileUploadChange" uploadStart="fileUploadStart" uploadComplete="fileUploadComplete" name="files" uploadUrl="{demo>/url}" useMultipart="true" sendXHR="true" uploadOnChange="true">
								<unified:headerParameters>
									<unified:FileUploaderParameter name="APIKey" value="{demo>/APIKey}"/>
									<unified:FileUploaderParameter name="Accept" value="application/json"/>
								</unified:headerParameters>
							</unified:FileUploader>
							<unified:FileUploader buttonOnly="true" buttonText="Upload Picture with Ajax" sameFilenameAllowed="true" multiple="true" fileType="zip,png,jpeg,jpg,bmp,tiff,tif" mimeType="application/x-zip-compressed,application/zip,application/octet-stream,image/png,image/jpg,image/jpeg,image/bmp,image/tiff" typeMissmatch="fileTypeMissmatch" change="onPressImageClassifier" custom:mode="ajax"></unified:FileUploader>
							<unified:FileUploader buttonOnly="true" buttonText="Upload Picture with XHR"  sameFilenameAllowed="true" multiple="true" fileType="zip,png,jpeg,jpg,bmp,tiff,tif" mimeType="application/x-zip-compressed,application/zip,application/octet-stream,image/png,image/jpg,image/jpeg,image/bmp,image/tiff" typeMissmatch="fileTypeMissmatch" change="onPressImageClassifier" custom:mode="xhr"></unified:FileUploader>
						</content>
					</Toolbar>
				</footer>
			</Page>
		</pages>
	</App>
</mvc:View>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Extend the main SAPUI5 controller)]

Edit the `demo.controller.js` file located under **`Workspace`** > **`sapui5ml-imageclassifier`** > **`webapp`** > **`controller`** and replace the existing code with the below code.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JavaScript
sap.ui.define([
  "sap/ui/core/mvc/Controller",
  "sap/m/MessageBox"
], function(Controller, MessageBox) {
  "use strict";

  var defaultFileSrcUrl = "/resources/sap/ui/documentation/sdk/images/logo_ui5.png";

  return Controller.extend("sapui5ml.controller.demo", {
    fileTypeMissmatch: function(oControlEvent) {
      MessageBox.show("Wrong file type!");
    },
    fileUploadChange: function(oControlEvent) {
      // start the busy indicator
      var oBusyIndicator = new sap.m.BusyDialog();
      oBusyIndicator.open();

      // init the src file, name & url
      this.srcFileURL = null;
      this.srcFileName = null;
      this.srcFile = null;

      // keep a reference of the uploaded file name and create a url out of that when this is an image
      this.srcFile = oControlEvent.getParameters().files[0];
      this.srcFileName = this.srcFile.name;
      if (this.srcFile.type.match("image.*")) {
        this.srcFileURL = URL.createObjectURL(this.srcFile);
      }
      // keep a reference in the view to close it later
      this.oBusyIndicator = oBusyIndicator;
    },
    fileUploadComplete: function(oControlEvent) {
      // get the current view
      var oView = this.getView();

      var service = "imageclassifier";

      // clear previous results from the model
      oView.getModel("demo").setProperty("/result-" + service, null);
      oView.getModel("demo").setProperty("/resultVisible-" + service, false);

      var srcFileIsImage = this.srcFile.type.match('image.*');

      var processResult = function(oController, data, fileName, url) {
        // set the image urls to default if part of a zip
        if (!srcFileIsImage) {
          for (var i = 0; i < data.predictions.length; i++) {
            data.predictions[i].name = fileName + "  /  " + data.predictions[i].name;
            data.predictions[i].fileURL = defaultFileSrcUrl;
          }
        } else {
          data.predictions[0].fileURL = URL.createObjectURL(oController.srcFile);
        }

        // merge with existing results
        var result = oController.getView().getModel("demo").getProperty("/result-" + service);
        if (result) {
          result.push.apply(result, data.predictions);
        } else {
          result = data.predictions;
        }
        oController.getView().getModel("demo").setProperty("/result-" + service, result);

        // display the result table
        oController.getView().getModel("demo").setProperty("/resultVisible-" + service, true);
      };

      if (oControlEvent.getParameters().status === 200) {
        // get the response as JSON and process the results
        processResult(this, JSON.parse(oControlEvent.getParameters().responseRaw), this.srcFile.name, this.srcFileURL);
      } else {
        oView.getModel("demo").setProperty("/resultVisible", false);
        MessageBox.show("Error " + oControlEvent.getParameters().status + " : " + JSON.parse(oControlEvent.getParameters().responseRaw).error_description);
      }
      this.oBusyIndicator.close();
      MessageBox.show("Process completed!\n Target URL: " + oView.getModel("demo").getProperty("/url"));
    },

    onPressImageClassifier: function(oControlEvent) {
      // get the current controller & view
      var oView = this.getView();

      // start the busy indicator
      this.oBusyIndicator = new sap.m.BusyDialog();
      this.oBusyIndicator.open();

      this.requestCount = 0;

      var service = "imageclassifier";
      var url = oView.getModel("demo").getProperty("/url");
      var type = "POST";
      var apiKey = oView.getModel("demo").getProperty("/APIKey");

      // clear previous results from the model
      oView.getModel("demo").setProperty("/result-" + service, null);
      oView.getModel("demo").setProperty("/resultVisible-" + service, false);

      var srcFile = null;
      var srcFileURL = null;
      var srcFileIsImage = false;

      var processResult = function(oController, data, fileName) {
        if (!srcFileIsImage) {
          for (var i = 0; i < data.predictions.length; i++) {
            data.predictions[i].name = fileName + "  /  " + data.predictions[i].name;
            data.predictions[i].fileURL = defaultFileSrcUrl;
          }
        } else {
          data.predictions[0].fileURL = srcFileURL;
        }

        var result = oController.getView().getModel("demo").getProperty("/result-" + service);
        if (result) {
          result.push.apply(result, data.predictions);
        } else {
          result = data.predictions;
        }
        oController.getView().getModel("demo").setProperty("/result-" + service, result);

        // display the result table
        oController.getView().getModel("demo").setProperty("/resultVisible-" + service, true);
      };

      // keep a reference of the uploaded files
      var mode = oControlEvent.getSource().data("mode");
      for (var fileIndex = 0; fileIndex < oControlEvent.getParameters().files.length; fileIndex++) {
        srcFile = oControlEvent.getParameters().files[fileIndex];
        if (srcFile.type.match("image.*")) {
          srcFileIsImage = true;
          srcFileURL = URL.createObjectURL(srcFile);
        } else {
          srcFileIsImage = false;
        }
        // create the form data to be sent in the request
        var formData = new window.FormData();
        formData.append("files", srcFile, srcFile.name);

        // increase request countor to close busy indicator
        this.requestCount++;

        // call the service
        this.callService(this, service, url, type, mode, apiKey, formData, processResult);
      }
    },
    callService: function(oController, service, url, type, mode, apiKey, formData, fnPrecessResult) {
      var ajaxSuccess = function(data, status, jqXHR) {
        // get the response as JSON and process the results
        fnPrecessResult(oController, data, formData.values().next().value.name);

        // close the busy indicator if all request have completed
        oController.requestCount--;
        if (oController.requestCount === 0) {
          // close the busy indicator
          oController.oBusyIndicator.close();
          MessageBox.show("Process completed!\n Target URL: " + url);
        }
      };
      var ajaxError = function(jqXHR, status, message) {
        oController.getView().getModel("demo").setProperty("/resultVisible-" + service, null);
        MessageBox.show("Error for file : " + formData.values().next().value.name + " \n status: " + status + "\n message: " + JSON.parse(jqXHR.responseText).error_description);
        oController.oBusyIndicator.close();
      };
      var xhrReadyStateChange = function() {
        if (this.readyState === this.DONE) {
          if (this.status === 200) {
            // set the response as JSON in the demo model
            var data = JSON.parse(this.response);
            var fileName = formData.values().next().value.name;
            var file = formData.get("files");
            fnPrecessResult(oController, data, file, fileName);
            // fnPrecessResult(oController, data, formData.values().next().value.name);
          } else {
            oController.getView().getModel("demo").setProperty("/resultVisible-" + service, null);
            MessageBox.show("Error for file : " + formData.values().next().value.name + " \n status: " + this.status + "\n message: " + JSON.parse(this.responseText).error_description);

          }
          // close the busy indicator if all request have completed
          oController.requestCount--;
          if (oController.requestCount <= 0) {
            // close the busy indicator
            oController.oBusyIndicator.close();
            MessageBox.show("Process completed!\n Target URL: " + url);
          }
        }
      };      
      if (mode === "ajax") {
        $.ajax({
          type: type,
          url: url,
          headers: {
            "Accept": "application/json",
            "APIKey": apiKey
          },
          success: ajaxSuccess,
          error: ajaxError,
          contentType: false,
          async: true,
          data: formData,
          cache: false,
          processData: false
        });
      } else if (mode === "xhr") {
        var xhr = new XMLHttpRequest();
        xhr.withCredentials = false;
        xhr.addEventListener("readystatechange", xhrReadyStateChange);
        xhr.open(type, url, false); // setting request method & API endpoint, the last parameter is to set the calls as synchyronous
        xhr.setRequestHeader("Accept", "application/json"); // adding request headers
        xhr.setRequestHeader("APIKey", apiKey); // API Key for API Sandbox
        xhr.send(formData); // sending request
      } else {
        oController.oBusyIndicator.close();
      }
    }
  });
});
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Test the application)]

Click on the **Run** icon ![Run Applications](00-run.png) or press `ALT+F5`.

In the bar at the bottom, click on **Upload Picture** to pick a single local picture.

The service will be called, and the result displayed in a table.

You can also try with a zip that contains multiple images. This will enable the Carousel, but images won't be displayed.

![Result](16.png)

> ### **Note:**
> If you are experiencing issue like a 405 error or the following message :
>
>**This service requires at least 1 file. Please put your file(s) into the files field of the POST request**
>
>Here are a few things you can check:
>
> - Make sure the Mock Server is not enable
> - Disable the mock data in your run configuration
> - Make that when running the application, the URL is the ***`index.html`*** and not the ***`extended_runnable_file.html`***. And if not replace ***`extended_runnable_file.html`*** by ***`index.html`***.
>
>If none of this solves your problem, you can also debug the `sendFilesWithXHR` function in `sap.ui.unified.FileUploader-dbg.js`.
>
>We are currently investigating this issue related to XHR use in the SAPUI5 `FileUploader` control. (see GitHub issue: https://github.com/SAPDocuments/Tutorials/issues/1864)
>
&nbsp;

You can also try the  **Upload Picture with Ajax** and **Upload Picture with XHR** which will use direct XHR or Ajax request instead.

It will also allow you to call the service with multiple local images without a zip archive.

These button will trigger multiple synchronous service requests.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Validation)]

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Solution: ](Project files)]

In case you are having problems when running the application, the complete project code can be found on the SAP Tutorial public [GitHub repository](https://github.com/SAPDocuments/Tutorials/tree/master/tutorials/ml-fs-sapui5-img-classification/sapui5ml-imageclassifier).

However, this is not a repository you can clone and run the code.

You have to import the `sapui5ml-imageclassifier` directory content into your existing project directory.

Make sure you check the [LICENSE](https://github.com/SAPDocuments/Tutorials/blob/master/LICENSE.txt) before starting using its content.

[DONE]
[ACCORDION-END]


### Additional Information
 - Select your next tutorial from these SAP Leonardo Machine Learning groups: [SAP API Business Hub](https://www.sap.com/developer/groups/ml-fs-api-hub.html), [Java](https://www.sap.com/developer/groups/ml-fs-java.html) or [SAPUI5](https://www.sap.com/developer/groups/ml-fs-sapui5.html)
