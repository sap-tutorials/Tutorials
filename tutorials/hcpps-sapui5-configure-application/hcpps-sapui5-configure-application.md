---
title: Configure a SAPUI5 application from the project template
description: Configure the destination connectivity to be used in the SAPUI5 application to interact with the HANA XS OData service and the SAP Cloud for predictive services
primary_tag: products>sap-cloud-platform-predictive-service
tags: [ tutorial>intermediate, products>sap-cloud-platform-predictive-service, products>sap-cloud-platform, topic>sapui5 ]
---

## Prerequisites
  - **Proficiency:** Intermediate
  - **Tutorials:** [Test the "Outliers" service using a REST client](http://www.sap.com/developer/tutorials/hcpps-rest-ps-outliers.html)

## Next Steps
  - [Visualize your predictive demo datasets in a SAPUI5 application using an HANA XS OData service](http://www.sap.com/developer/tutorials/hcpps-sapui5-odata.html)

## Details
### You will learn
  - How to add a destination to your SAP Cloud Platform account
  - How to create a SAPUI5 application from a template using the SAP Web IDE
  - How to add a destination to your SAPUI5 application
  - Run your SAPUI5 application
>
> **Note:**
The intent of the following tutorials is not to focus on SAPUI5 but to use it as mean to execute the SAP Cloud for predictive services.
For more content on SAPUI5, you can check the dedicated SAPUI5 tutorials or the online SAPUI5 documentation available here: https://sapui5.hana.ondemand.com

### Time to Complete
  **10 minutes**

> In order to ease the readability of this tutorial, we have used tokens to replace long URLs.
Therefore you can replace any occurrence of the token by the value listed above.
>
> Token               | Value
------------------- | -------------
<code><b>&lt;Account name&gt;</b></code>  | your SAP Cloud Platform account name. On a developer trial account, it should end by `trial`
<code><b>&lt;C4PA URL&gt;</b></code> | `https://aac4paservices<`<code><b>Account name</b></code>`>.hanatrial.ondemand.com/com.sap.aa.c4pa.services`
<code><b>&lt;HANA instance id&gt;</b></code>  | as created previously, should be `mdc`
<code><b>&lt;HANA URL&gt;</b></code> | `https://<`<code><b>HANA instance id</b></code>`><`<code><b>Account name</b></code>`>.hanatrial.ondemand.com`
<code><b>&lt;HANA User&gt;</b></code> | `HCPPSTRIAL`
<code><b>&lt;HANA Password&gt;</b></code> | `Welcome17Welcome17`
>
> If you are unclear with what is your SAP Cloud Platform account name, you can refer to the following blog entry: [SAP HANA Cloud Platform login, user name, account id, name or display name: you are lost? Not anymore!](https://blogs.sap.com/2017/01/31/sap-hana-cloud-platform-trial-login-name-user-name-account-name-account-identifier-you-are-lost-not-anymore/)

[ACCORDION-BEGIN [Step 1: ](Create your destination)]
Log into the [***SAP HANA Cloud Platform Cockpit***](http://account.hanatrial.ondemand.com/cockpit) with your free trial account and access "Your Personal Developer Account".

Click on your ***SAP Cloud Platform Account*** identifier (which ends with *trial*) as highlighted on the below screenshot.

![SAP HANA Cloud Platform Cockpit](01.png)

On the left side bar, you can navigate in **Connectivity** > **Destinations**.

![Your Personal Developer Account](02.png)

On the ***Destinations*** overview page, click on **New Destination**

![Destinations](03.png)

Enter the following information:

Field Name           | Value
-------------------- | --------------
Name                 | `HCPOData`
Type                 | `HTTP`
Description          | `OData Service Destination`
URL                  | `<HANA URL>`
Proxy Type           | `Internet`
Authentication       | `Basic Authentication`
User                 | `<HANA User>`
Password             | `<HANA Password>`

Then you will need to add the following properties to the destination:

Property Name          | Value
---------------------- | --------------
`WebIDEUsage`          | `odata_gen`
`WebIDEEnabled`        | `true`

Click on **Save**

![New Destinations](04.png)

Click on **New Destination**

Enter the following information:

Field Name           | Value
-------------------- | --------------
Name                 | `HCPps`
Type                 | `HTTP`
Description          | `HCP predictive service Destination`
URL                  | `<C4PA URL>`
Proxy Type           | `Internet`
Authentication       | `AppToAppSSO`

> Make sure you update the URL with your SAP Cloud Platform Account identifier.

-

Then you will need to add the following properties to the destination:

Property Name          | Value
---------------------- | --------------
`WebIDEEnabled`        | `true`

Click on **Save**

![New Destinations](05.png)

You can use the **Test Connectivity** button ![HTML5 Applications](0-check.png) next to each **Destination** to validate our configuration.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open the Web IDE)]

On the left side bar, you can navigate in **Services**, then using the search box enter `Web IDE`.

![Web IDE](06.png)

Click on the tile, then click on **Open SAP Web IDE**.

![Web IDE](07.png)

You will get access to the **SAP Web IDE** main page:

![Web IDE](08.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create your application using the SAPUI5 template)]

Click on **New Project from Template** in the ***Create Project*** section

![Project](09.png)

Select the **SAPUI5 Application** tile, then click on **Next**

![Project](10.png)

Enter the following information, then click on **Next**

Field Name           | Value
-------------------- | --------------
Project Name         | `hcppredictiveservicesdemo`
Namespace            | `demo`

![Git](11.png)

Enter the following information, then click on **Finish**

Field Name           | Value
-------------------- | --------------
View Type            | `XML`
View Name            | `demo`

![Git](12.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add the destinations to your application)]

Your template project is created! Let's continue, and add the "destinations" previously created in the SAP Cloud Platform cockpit.

Open the `neo-app.json` file and update the file like this:

```json
{
	"welcomeFile": "index.html",
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
			"path": "/HCPOData",
			"target": {
				"type": "destination",
				"name": "HCPOData"
			},
			"description": "HCPOData destination"
		}, {
			"path": "/HCPps",
			"target": {
				"type": "destination",
				"name": "HCPps"
			},
			"description": "HCPps destination"
		}
	]
}
```

Click on the ![Save Button](0-save.png) button (or press CTRL+S)

> **Note:**  there are multiple ways to add your destinations in a SAPUI5 application depending on the type of destinations. Here, we will simply add them manually in the `neo-app.json` file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Update the namespace definition)]

**Due to recent updates in the SAPUI5 template in the Web IDE, the namespace that you have set in the wizards might not be properly reflected in the created application.
To avoid any problem with the tutorials code, we will use a new namespace.**

Open the `index.html` file in the `webapp` directory and add the following `resourceroots` to the existing list.

Replace the following line of code

```js
data-sap-ui-resourceroots='{"xxxxx": ""}'
```
by

```javascript
data-sap-ui-resourceroots='{"xxxxx": "", "sapui5demo": ""}'
```

> **Note: The '`xxxxx`' represent the current application namespace that was generated by the template wizard.
It should be equal to `demo`, but some version of the template will generate something like '`demohcppredictiveservicesdemo`' or '`hcppredictiveservicesdemo`'.
Take a note of that value as you will need to replace a few occurrences of "`xxxxx`" by this value in the code provided the next steps**

-

Click on the ![Save Button](0-save.png) button (or press CTRL+S)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Update the default view : `demo.view.xml`)]

The default view created is located in `webapp\view\demo.view.xml`. We will update the view to prepare for the next steps.

Open the `webapp\view\demo.view.xml` file and replace the existing code with the following code:

```xml
<mvc:View controllerName="xxxxx.controller.demo" xmlns:html="http://www.w3.org/2000/xhtml"
	xmlns:mvc="sap.ui.core.mvc" displayBlock="true" xmlns="sap.m"
	xmlns:custom="http://schemas.sap.com/sapui5/extension/sap.ui.core.CustomData/1">
	<SplitApp id="SplitAppDemo" initialMaster="master" initialDetail="detail">
		<masterPages>
			<Page id="master" title="Domains">
				<content>
					<List itemPress="onMasterListItemPress">
						<items>
							<StandardListItem title="HANA XS OData" type="Navigation" custom:to="sub_master_odata"/>
							<StandardListItem title="Dataset Services" type="Navigation" custom:to="sub_master_dataset"/>
							<StandardListItem title="Forecast Services" type="Navigation" custom:to="sub_master_forecast"/>
						</items>
					</List>
				</content>
			</Page>
			<Page id="sub_master_odata" title="HANA XS OData" showNavButton="true" navButtonPress="onPressMasterBack">
				<content>
					<List itemPress="onDetailListItemPress">
						<items>
							<StandardListItem title="Cash Flow" type="Active" custom:to="detail_odata_cashflow"/>
							<StandardListItem title="Census" type="Active" custom:to="detail_odata_census"/>
							<StandardListItem title="E-Commerce Transaction" type="Active" custom:to="detail_odata_transaction"/>
						</items>
					</List>
				</content>
			</Page>
			<Page id="sub_master_dataset" title="Dataset Services" showNavButton="true" navButtonPress="onPressMasterBack">
				<content>
					<List itemPress="onDetailListItemPress">
						<items>
							<StandardListItem title="Register" type="Active" custom:to="detail_dataset_register"/>
							<StandardListItem title="Manage" type="Active" custom:to="detail_dataset_manage"/>
						</items>
					</List>
				</content>
			</Page>
			<Page id="sub_master_forecast" title="Forecast Services" showNavButton="true" navButtonPress="onPressMasterBack">
				<content>
					<List itemPress="onDetailListItemPress">
						<items>
							<StandardListItem title="Synchronous" type="Active" custom:to="detail_forecast_synchronous"/>
							<StandardListItem title="Asynchronous" type="Active" custom:to="detail_forecast_asynchronous"/>
						</items>
					</List>
				</content>
			</Page>
		</masterPages>
		<detailPages>
			<Page id="detail" title="Test the SAP Cloud Platform Predictive Service in a SAPUI5 application">
				<content></content>
			</Page>
		</detailPages>
	</SplitApp>
</mvc:View>
```

> **Note: The '`xxxxx`' represent your application namespace that was generated by the template wizard. Make sure you are replacing the '`xxxxx`' by your value as described in step 5.**

-

Click on the ![Save Button](0-save.png) button (or press CTRL+S).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Update the default controller : `demo.controller.js`)]

The default controller is located in `webapp\controller\demo.controller.js`. We will update the view to prepare for the next steps.

Open the `webapp\controller\demo.controller.js` file and replace the existing code with the following code:

```js
sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/m/MessageToast"
], function(Controller, MessageToast) {
	"use strict";

	return Controller.extend("xxxxx.controller.demo", {
		onInit: function() {
			if (typeof sap.ui.getCore().getModel() === 'undefined') {
				sap.ui.getCore().setModel(new sap.ui.model.json.JSONModel());
			}
		},
		getSplitAppObj: function() {
			var result = sap.ui.getCore().byId(this.createId("SplitAppDemo"));
			if (!result) {
				MessageToast.show("SplitApp object can't be found", {
					duration: 5000
				});
			}
			return result;
		},
		onMasterListItemPress: function(oEvent) {
			var sToPageId = oEvent.getParameter("listItem").getCustomData()[0].getValue();
			this.getSplitAppObj().toMaster(this.createId(sToPageId));
		},
		onPressMasterBack: function() {
			this.getSplitAppObj().backMaster();
		},
		onDetailListItemPress: function(oEvent) {
			var sToPageId = oEvent.getParameter("listItem").getCustomData()[0].getValue();
			this.getSplitAppObj().toDetail(this.createId(sToPageId));
		}
	});
});

```

> **Note: The '`xxxxx`' represent your application namespace that was generated by the template wizard. Make sure you are replacing the '`xxxxx`' by your value as described in step 5.**

-

Click on the ![Save Button](0-save.png) button (or press CTRL+S)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run the application)]

From the menu bar, select **Run** > **Run As** > **Web Application**  or use the ![Run Button](0-run.png) **Run** button.

![Run Button](13.png)

This will open a web page with the following content:

![Run Button](14.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Solution: ](Created and modified files)]

In case you are having problems when running the application, please find bellow the created and modified files:

  - [`neo-app.json`](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpps-sapui5-configure-application/solution-neo-app.json.txt)
  - [`webapp\controller\demo.controller.js`](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpps-sapui5-configure-application/solution-controller-demo.controller.js.txt)
  - [`webapp\view\demo.view.xml`](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpps-sapui5-configure-application/solution-view-demo.view.xml.txt)

[DONE]
[ACCORDION-END]

## Next Steps
  - [Visualize your predictive demo datasets in a SAPUI5 application using an HANA XS OData service](http://www.sap.com/developer/tutorials/hcpps-sapui5-odata.html)
