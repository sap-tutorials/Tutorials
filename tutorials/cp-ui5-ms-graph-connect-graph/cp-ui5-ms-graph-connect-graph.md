---
title: Connect to Microsoft Graph
description: Connect your application to the Microsoft Graph via the Microsoft Authentication Library (MSAL) for JS
auto_validation: true
primary_tag: topic>javascript
tags: [  tutorial>intermediate, topic>cloud, topic>sapui5 ]
time: 35
---


## Details
### You will learn  
  - How to integrate the Microsoft Authentication Library (MSAL) into a SAPUI5 application
  - How to interact with Microsoft Graph
  - How to use the SAPUI5 date formatter in XML views

---

[ACCORDION-BEGIN [Step ](Register app in Microsoft Application Registration Portal)]

Go to <https://apps.dev.microsoft.com/#/appList> and click on **Add an app** on the upper right corner.

![portal apps overview](./ms-portal-apps.png)

You should now give your application a certain name. This name will not appear anyway and has no technical implication.

Replace `Your App name goes here` with an application name of your choice and proceed with **Create**.

![portal app name](./ms-portal-app-name.png)

You are now forwarded to the configuration of your application. Navigate to the **Platforms** sections and add the URL of your SAPUI5 application. You can also add `localhost:8000` in case you wish to test the application locally as well.

![portal-app-platform-details](./ms-portal-callbacks.png)
To enable your application to search for emails navigate to `Microsoft Graph Permissions`, click on **Add** and select the **`Mail.read`** permission.

![portal app name](./ms-portal-add-permission.png)

Save the changes and scroll to the top to save app id in your clipboard or a new file, you will need it in the next step.

![appid](./appid.png)

> You (and the users of your application) can revoke the given permissions at <https://myapps.microsoft.com>

[VALIDATE_1]
[ACCORDION-END]
[ACCORDION-BEGIN [Step](Create Microsoft Graph config file)]

Create a new folder named **`msal`** in the `webapp` folder and add a file named **`msalconfig.js`** with the following content to this folder:

```javascript
/* eslint-disable sap-no-global-define */
/* eslint-disable sap-no-hardcoded-url */

window.msalconfig = {
    clientID: "<YOUR APP ID>",
    redirectUri: location.origin,
    graphBaseEndpoint: "https://graph.microsoft.com/beta/",
    userInfoSuffix: "me/",
    queryMessagesSuffix: "me/messages?$search=\"$1\"&$top=150",
    graphAPIScopes: ['User.Read', 'Mail.Read']
};
```

Replace the placeholder (`<YOUR APP ID>`) with the id you saved in the previous step.

This file contains constants you need later in this tutorial.

> The optional `eslint` comments in the first two lines prevent that the SAP Web IDE will show neglectable warnings.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Load Microsoft Authentication Library)]


```html
<script src="https://secure.aadcdn.microsoftonline-p.com/lib/0.2.3/js/msal.min.js"></script>
<script src="msal/msalconfig.js" type="text/javascript"></script>
```

> You can find the most [recent version](https://www.npmjs.com/package/msal)  of this library at npm and fetch it from this source.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step ](Initialize the MSAL client)]

1. Replace the definition header of the `MainView.controller.js` with this code to suppress warning in the SAP Web IDE and to inject more SAPUI5 controls via dependency injection.

    ```javascript
    /* global msalconfig, Msal */
    sap.ui.define(["sap/ui/core/mvc/Controller", "sap/m/MessageToast", "sap/ui/model/json/JSONModel"],
      function (Controller, MessageToast, JSONModel){
    ```

2. The `onInit` hook initializes the connection to the Microsoft Graph API once the controller is initialized. Add this hook above the first function in the controller:
```javascript
onInit: function () {
  this.oUserAgentApplication = new Msal.UserAgentApplication(msalconfig.clientID, null,
    function (errorDesc, token, error, tokenType) {
      if (errorDesc) {
        var formattedError = JSON.stringify(error, null, 4);
        if (formattedError.length < 3) {
          formattedError = error;
        }
        MessageToast.show("Error, please check the $.sap.log for details");
        $.sap.log.error(error);
        $.sap.log.error(errorDesc);
      } else {
        this.fetchUserInfo();
      }
    }.bind(this), {
      redirectUri: msalconfig.redirectUri
    });
  //Previous version of msal uses redirect url via a property
  if (this.oUserAgentApplication.redirectUri) {
    this.oUserAgentApplication.redirectUri = msalconfig.redirectUri;
  }
  // If page is refreshed, continue to display user info
  if (!this.oUserAgentApplication.isCallback(window.location.hash) && window.parent === window && !window.opener) {
    var user = this.oUserAgentApplication.getUser();
    if (user) {
      this.fetchUserInfo();
    }
  }
},
```

[VALIDATE_4]
[ACCORDION-END]
[ACCORDION-BEGIN [Step ](Add a login button)]
Add a new button to the header of the first page in the `MainView.view.xml` file.
```xml
<headerContent>
  <Button
    icon="sap-icon://person-placeholder"
    text="{= ${session>/givenName} === undefined ? 'Login' : 'Logout ' + ${session>/givenName} }"
    press="onSwitchSession"/>
</headerContent>
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step ](Add a session model)]
1. This previous step used a model named `session`. Open the `manifest.json` file and make sure you use the **Descriptor Editor** to declare the **`session`** model.
![openmanifest](./openmanifest.png)
2. Open the tab **Models** and click the plus button.
![addmodel](./addmodel.png)
3. Name the model **`session`** and make sure it's type is **`JSON`**.
![modeldialog](./modeldialog.png)


[VALIDATE_6]
[ACCORDION-END]
[ACCORDION-BEGIN [Step](Authenticate with Microsoft Graph)]
Add the following functions to the `MainView.controller.js` to make use of the imported MSAL library.

1. Add the function to authorize the web app when user clicks the login/logout button.
```javascript
//************* MSAL functions *****************//
onSwitchSession: function (oEvent) {
	var oSessionModel = oEvent.getSource().getModel('session');
	var bIsLoggedIn = oSessionModel.getProperty('/displayName');
	if (bIsLoggedIn) {
		this.oUserAgentApplication.logout();
		return;
	}
	this.fetchUserInfo();
},
// INSERT CODE IN SUB-STEP 2 HERE
```
2. Copy the following to the controller to read the user information and save them to the session model.
```javascript
fetchUserInfo: function () {
	this.callGraphApi(msalconfig.graphBaseEndpoint + msalconfig.userInfoSuffix, function (response) {
		$.sap.log.info("Logged in successfully!", response);
		this.getView().getModel('session').setData(response);
	}.bind(this));
},
// INSERT CODE IN SUB-STEP 3 HERE
```
3. This helper function checks whether the user is authorized and redirects him to the Microsoft authorization website if necessary. It also adds the required authorized token to all requests.
```javascript
callGraphApi: function (sEndpoint, fnCb) {
	var user = this.oUserAgentApplication.getUser();
	if (!user) {
		this.oUserAgentApplication.loginRedirect(msalconfig.graphAPIScopes);
	} else {
		this.oUserAgentApplication.acquireTokenSilent(msalconfig.graphAPIScopes)
			.then(function (token) {
					$.ajax({
							url: sEndpoint,
							type: "GET",
							beforeSend: function (xhr) {
								xhr.setRequestHeader("Authorization", "Bearer " + token);
							}
						})
						.then(fnCb)
						.fail(function (error) {
							MessageToast.show("Error, please check the log for details");
							$.sap.log.error(JSON.stringify(error.responseJSON.error));
						});
				}.bind(this),
				function (error) {
					if (error) {
						this.oUserAgentApplication.acquireTokenRedirect(msalconfig.graphAPIScopes);
					}
				}.bind(this));
	}
},
// INSERT CODE IN STEP 9 HERE
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step](Make details searchable)]
Replace the content of the form (of the second page) with the following SAPUI5 controls. This snippet defines links instead of simple text view, which are only enabled if the user has been authorized by the MSAL lib.
```xml
<Label text="Purchase Order ID" width="100%">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Link press="onPressLink" enabled="{= !!${session>/givenName} }" text="{POId}"/>
<Label text="Supplier Name">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Link  press="onPressLink" enabled="{= !!${session>/givenName} }" text="{SupplierName}"/>
<Label text="OrderedByName">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Link press="onPressLink" enabled="{= !!${session>/givenName} }" text="{OrderedByName}"/>
<Label text="DeliveryAddress">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Text text="{DeliveryAddress}"/>
<Label text="GrossAmount">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Link press="onPressLink" enabled="{= !!${session>/givenName} }" text="{GrossAmount}"/>
<Label text="CurrencyCode">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Link press="onPressLink" enabled="{= !!${session>/givenName} }" text="{CurrencyCode}"/>
<Label text="ItemCount">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Text text="{ItemCount}"/>
<Label text="Changed At">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Text text="{ChangedAt}"/>
<Label text="DeliveryDateEarliest">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Text text="{DeliveryDateEarliest}"/>
<Label text="LaterDelivDateExist">
  <layoutData>
    <layout:GridData span="L4 M4"/>
  </layoutData>
</Label>
<Text text="{LaterDelivDateExist}"/>
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step](Search emails via the MSAL lib)]
Implement the event handler of the details page, in the `MainView.controller.js` file, which will trigger a search via the MSAL lib. This function will also format the search results and save them in a new model named **`msData`**.
```javascript
onPressLink: function (oEvent) {
  var sLinkText = oEvent.getSource().getText();
  var oApp = this.getView().getContent()[0];
  this.callGraphApi(msalconfig.graphBaseEndpoint + msalconfig.queryMessagesSuffix.replace("$1", sLinkText), function(results) {
            results.value = results.value.map(function(o) {
                o.bodyPreview = o.bodyPreview.replace(sLinkText, '<strong>'+sLinkText+'</strong>');
                return o;
            });
            var oResultsPage = oApp.getPages()[2].setModel(new JSONModel(results), 'msData');
            oApp.to(oResultsPage.getId());
        });
},
// INSERT CODE IN STEP 11 HERE
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step](Add a new page to the existing application)]
Add a third page in the `MainView.view.xml` file to display the search results returned by the Microsoft Graph.
```xml
<Page
	id="results"
	title="Results"
	navButtonPress="onNavButtonPress"
	showNavButton="true">
	<List
		noDataText="No emails found"
		items="{msData>/value}" >
		<FeedListItem
			sender="{msData>subject}"
			showIcon="false"
			info="{= ${msData>sender/emailAddress/address} + ' ('+${msData>sender/emailAddress/name} +')'}"
			timestamp="{
        path : 'msData>createdDateTime',
        type : 'sap.ui.model.type.DateTime',
        formatOptions : {
          source : {
          	UTC: true,
            pattern : 'yyyy-MM-dd\'T\'hh\':\'mm\':\'ss\'Z\''
          },
          style: 'full',
          relative: true
        }
      }"
			text="&lt;br/&gt; {msData>bodyPreview}"
			senderPress="onOpenEmail"
			convertLinksToAnchorTags="All">
		</FeedListItem>
	</List>
</Page>
```

[VALIDATE_10]
[ACCORDION-END]
[ACCORDION-BEGIN [Step](Open found emails in a new tab)]
Implement a callback function in the `MainView.controller.js` file to handle a click on the title of a search result, which will open the corresponding email in `Outlook.com`.
```javascript
onOpenEmail: function (oEvent) {
  var sEmail = oEvent.getSource().getBindingContext('msData').getProperty('webLink');
  window.open(sEmail, '_blank');
},
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step](Re-deploy the application)]
Deploy the latest version of the application to the SAP Cloud Platform Neo environment. Right click on the project and select the **Deploy**  operation and **SAP Cloud Platform** as the target. You can confirm the default values with the **Deploy** button.

Now you are able to click on a property on the details page to search for this term. You will be redirected to the search results page, which will preview the found mails. Click on the subject of an email to open it in the `outlook.com` web application.

![redeployed application](./redeploy.png)

> **IMPORTANT:** Please make sure that there are mails in your outlook account (any folder is fine) which match the keywords you might search for. Otherwise your search request will return an empty result set.

[VALIDATE_12]
[ACCORDION-END]




---
