---
title: Image Classification ML service with SAPUI5
description: Discover how to implement SAP Leonardo Machine Learning Functional Service in a SAPUI5 application
primary_tag: products>sap-leonardo-machine-learning
tags: [ tutorial>beginner, topic>sapui5, topic>html5, topic>machine-learning, products>sap-leonardo-machine-learning, products>sap-api-management, products>sap-cloud-platform, products>sap-web-ide]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - [Sign up for an free trial account on the SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)

## Next Steps
 - Select your next tutorial from these SAP Leonardo Machine Learning groups: [SAP API Business Hub](https://www.sap.com/developer/groups/ml-fs-api-hub.html), [Java](https://www.sap.com/developer/groups/ml-fs-java.html) or [SAPUI5](https://www.sap.com/developer/groups/ml-fs-sapui5.html)
 - Select a tutorial group from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)

## Details
### You will learn  
In this tutorial, you will learn how to quickly integrate the **Image Classification** SAP Leonardo Machine Learning Functional Services published from the SAP API Business Hub sandbox in a SAPUI5 application.

The **Image Classification** service allows you to calculates and returns a list of classifications/labels along with their probabilities for a given image.

You will then be able to substitute the **Image Classification** services with any other SAP Leonardo Machine Learning Functional Services that consumes images content.

### Time to Complete
**20 Min**

---

[ACCORDION-BEGIN [Step 1: ](Get Your Sandbox URL)]

In order to consume the **Image Classification** Machine Learning Functional Services, you will first need to get the service URI, your API Key and the request and response parameters.

Go to [https://api.sap.com/](https://api.sap.com) and click on the **Browse** tile.

![SAP API Business Hub](01.png)

Then you will be able to search for the **SAP Leonardo Machine Learning - Functional Services**, then click on the package found.

![SAP API Business Hub](02.png)

Click on **Artifacts**, then click on the **Image Classification API**.

![SAP API Business Hub](03.png)

On the **Resource** tab, you can notice the **Image Classification API** has only one resource (or service): `/inference_sync`.

If you expand the `/inference_sync` resource and look for the ***Parameters*** section, you will not that the service request require the following:

- **`files`** (required) : The list of file(s) to be uploaded. Either:

    - one image file (image formats, such as `.jpeg`, `.png`, `.tif`, or `.bmp`)
    - one archive file containing multiple image files (format `.zip`, `.tar.gz`, or `tar`)

![SAP API Business Hub](04-0.png)

Now click on the **Overview** tab.

> **Note**: the term *inference* refers to the application phase (scoring) an existing model (as opposed to the training or inception phase) and *sync* for synchronous.

![SAP API Business Hub](04-1.png)

As displayed on the screen, the sandbox URL for the **Image Classification API** where we need to append the API resource:

```JSON
https://sandbox.api.sap.com/ml/imageclassifier/inference_sync
```

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get Your API key)]

When using any of the APIs outside of the SAP API Business Hub, an application key will be needed in every request header of your APIs calls.

To get to your API key, click on the ![key](00-key.png) icon in the top right corner of the page. Click on the key icon.

The following pop-up should appear. Click on the **Copy API Key** button and save it in a text editor.

![SAP API Business Hub](05.png)

Now, let's build a SAPUI5 application! But before doing so let's first add the destination to connect to the SAP API Business Hub.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Access the SAP Cloud Platform Cockpit)]

Go to your [***SAP Cloud Platform Cockpit***](http://account.hanatrial.ondemand.com/cockpit) account and access "Your Personal Developer Account".

![SAP HANA Cloud Platform Cockpit](06.png)

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

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open the Web IDE)]

On the left side bar, you can navigate in **Services**, then using the search box enter `Web IDE`.

![Web IDE](10.png)

Click on the tile, then click on **Open SAP Web IDE**.

![Web IDE](11.png)

You will get access to the **SAP Web IDE** main page:

![Web IDE](12.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create your application using the SAPUI5 template)]

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

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Extend the application resource roots)]

In order to ease the use of the provided code, we will add a new SAPUI5 resource roots. The main reason for this is that the rule used to generate the initial resource root by the project template has change many time over the time.

Edit the `index.html` file located under **`Workspace`** > **`sapui5ml`** > **`webapp`** and add the below element to the existing `data-sap-ui-resourceroots` property around line 15 (don't forget the comma in between the existing element and the new one).

```JavaScript
"sapui5ml": ""
```

It should eventually look something like this:

```JavaScript
data-sap-ui-resourceroots='{"demosapui5ml-imageclassifier": "", "sapui5ml": ""}'
```

Click on the ![Save Button](00-save.png) button (or press CTRL+S).

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
>By default, headers element like the `APIKey` will be blocked when used in a SAPUI5 control like the `FileUploader`.
>This is the reason why we add it to the white list.
>

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Store your API setting in a JSON model)]

There are multiple options to achieve this goal. Here we will use a pre-loaded JSON model configured in the `manifest.json` file.

Create a new file named `demo.json` under **`Workspace`** > **`sapui5ml-imageclassifier`** > **`webapp`** > **`model`**, copy the below code and make sure you replace `<<<<< COPY YOUR API KEY >>>>>` by your the API key we retrieved in step 2.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JSON
{
	"url" : "/ml/imageclassifier/inference_sync",
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
					<Carousel pages="{demo>/result-imageclassifier}" width="100%" visible="{= typeof ${demo>/resultVisible-imageclassifier} !== 'undefined'}">
						<pages>
							<VBox width="100%" direction="Column" alignItems="Center">
								<Image class="sapUiLargeMargin" src="{demo>fileURL}"/>
								<Label text="File name: {demo>name}" class="sapUiLargeMargin"></Label>
								<table:Table rows="{demo>results}" enableBusyIndicator="true" selectionMode="Single" visibleRowCount="5">
									<table:columns>
										<table:Column sortProperty="label" filterProperty="label">
											<Label text="Label"/>
											<table:template>
												<Link text="{demo>label}" href="https://www.google.fr/search?q={label}&amp;newwindow=1&amp;tbm=isch" target="search"/>
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

[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Extend the main SAPUI5 controller)]

Edit the `demo.controller.js` file located under **`Workspace`** > **`sapui5ml-imageclassifier`** > **`webapp`** > **`controller`** and replace the existing code with the below code.

Then click on the ![Save Button](00-save.png) button (or press CTRL+S).

```JavaScript
sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast"
], function(Controller, MessageToast) {
	"use strict";

	var defaultFileSrcUrl = "/resources/sap/ui/documentation/sdk/images/logo_ui5.png";

	return Controller.extend("sapui5ml.controller.demo", {
		fileTypeMissmatch: function(oControlEvent) {
			MessageToast.show("Wrong file type!");
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
				processResult(this, JSON.parse(oControlEvent.getParameters().responseRaw), this.srcFile.nam, this.srcFileURL);
			} else {
				oView.getModel("demo").setProperty("/resultVisible", false);
				MessageToast.show("Error " + oControlEvent.getParameters().status + " : " + oControlEvent.getParameters().responseRaw);
			}
			this.oBusyIndicator.close();
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
				}
			};
			var ajaxError = function(jqXHR, status, message) {
				oController.getView().getModel("demo").setProperty("/resultVisible-" + service, false);
				MessageToast.show("Error for file : " + formData.values().next().value.name + " \n status: " + status + "\n message: " + message);
			};
			var xhrReadyStateChange = function() {
				if (this.readyState === this.DONE) {
					if (this.status === 200) {
						// get the response as JSON and process the results
						fnPrecessResult(oController, JSON.parse(this.response), formData.values().next().value.name);
					} else {
						oController.getView().getModel("demo").setProperty("/resultVisible-" + service, false);
						MessageToast.show("Error for file : " + formData.values().next().value.name + " \n status: " + this.status + "\n message: " +
							this.response);
					}
					// close the busy indicator if all request have completed
					oController.requestCount--;
					if (oController.requestCount === 0) {
						oController.oBusyIndicator.close();
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
					async: false,
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

[ACCORDION-END]

[ACCORDION-BEGIN [Solution: ](Project files)]

In case you are having problems when running the application, the complete project code can be found on the SAP Tutorial public [GitHub repository](https://github.com/SAPDocuments/Tutorials/tree/master/tutorials/ml-fs-sapui5-img-classification/sapui5ml-imageclassifier).

However, this is not a repository you can clone and run the code.

You have to import the `sapui5ml-imageclassifier` directory content into your existing project directory.

Make sure you check the [LICENSE](https://github.com/SAPDocuments/Tutorials/blob/master/LICENSE.txt) before starting using its content.

[ACCORDION-END]

## Next Steps
 - Select your next tutorial from these SAP Leonardo Machine Learning groups: [SAP API Business Hub](https://www.sap.com/developer/groups/ml-fs-api-hub.html), [Java](https://www.sap.com/developer/groups/ml-fs-java.html) or [SAPUI5](https://www.sap.com/developer/groups/ml-fs-sapui5.html)
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorials.html)
