---
title: Secure your application with XSUAA (Forecast App)
description: Understand and implement the basics of XSUAA to secure your application
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>intermediate, products>sap-hana\, express-edition, topic>machine-learning]
---

## Prerequisites
 - [Use Machine Learning to Build a Forecasting application using the XS advanced development model](https://developers.sap.com/group.hxe-aa-forecast.html)

## Details
### You will learn
- How to create a XSUAA service with its security descriptor
- How to configure users and role in the XSA Administration Cockpit
- How to add the required security configuration to your MTA project

For more details, check the [Maintaining Application Security in XS Advanced](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/2.0.04/en-US/35d910ee7c7a445a950b6aad989a5a26.html) documentation.

### Time to Complete
**10 Min**

[ACCORDION-BEGIN [Step 1: ](Open the SAP HANA XS Advanced Cockpit)]

Open the SAP HANA XS Advanced Cockpit, and login using the **`XSA_ADMIN`** credentials.

![XS Advanced Cockpit](01-01.png)

As a reminder the default URL for the SAP HANA XS Advanced Cockpit is:  `https://hxehost:51036/cockpit`.

A link to the SAP HANA XS Advanced Cockpit can also be found on the XSA Controller page at: `https://hxehost:39030`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create a new user)]

You will now create a new user that will be used to connect to your application.

On the left side select **User Management**, then click on the **New User** Button.

![XS Advanced Cockpit](02-01.png)

Enter the following details and click on **Save**.

Property      | Value
--------------|-------------------
User Name     | **`FORECAST`**
Email         | `FORECAST@some.domain` (or real email)
Password      | `Welcome18`

![XS Advanced Cockpit](02-02__2020-08-13_16-07-42.png)

> ### **Note:** You will be prompted to change the FORECAST user password upon the first successful login attempt.
> You can also login at `https://hxehost:39032/uaa-security` to change the password using a separate web browser session.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Configure the XSA UAA service)]

First, you will be adding a XSA UAA service to your development space with an Application Security Descriptor.

The Application Security Descriptor defines the details of the authentication methods and authorization types to use for access to your application.

Here is a basic Application Security Descriptor that you will be using, as you won't need to define multiple scope and role templates in the end.

```JSON
{
  "xsappname": "forecast",
  "scopes": [{
    "name": "$XSAPPNAME.USER",
    "description": "Forecast User Application Scope"
  }],
  "role-templates": [{
    "name": "ForecastUser",
    "description" : "Forecast User Role Template",
    "scope-references": ["$XSAPPNAME.USER"]
  }]
}
```
> ### **Note:** In order to complete this steps, you will be using the XS CLI client which is locally installed with your SAP HANA 2.0, express edition instance.
>
However, you may prefer to run commands remotely (from your local desktop for example).
>
To do so, you can complete the [XS CLI Client installation](https://developers.sap.com/tutorials/hxe-ua-install-xs-xli-client.html)

As an `hxeadm` OS user execute the following series of XS CLI command:

#### Login to the `HANAExpress` organization and `development` space:

```shell
xs login -o HANAExpress -s development -u XSA_ADMIN
```

#### Create the XSUAA service instance:

```shell
xs create-service xsuaa space forecast-uaa -c '{"xsappname": "forecast", "scopes": [{ "name": "$XSAPPNAME.USER", "description": "Forecast User Application Scope" }], "role-templates": [{ "name": "ForecastUser", "description" : "Forecast User Role Template", "scope-references": ["$XSAPPNAME.USER"]}]}'
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create and Assign the Role)]

Once the XSUAA service is created, you can proceed with the role collection creation, configuration and assignment to the *FORECAST* user.

Execute the following series of XS CLI command:

#### Create a role collection:

```shell
xs create-role-collection FORECAST_USER 'Forecast Application User Role Collection'
```

#### Add the `Forecast` User role the role collection:

```shell
xs update-role-collection FORECAST_USER --add-role ForecastUser --app forecast -s development -t ForecastUser
```

#### Assign the `Forecast` User role the FORECAST user:

```shell
xs assign-role-collection FORECAST_USER FORECAST
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Open the Web IDE)]

Open the Web IDE, and login using the **`XSA_DEV`** credentials.

Switch to the **Development** tool using the ![Web IDE Development](00-development.png) icon.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the Application Security Descriptor)]

The Application Security Descriptor defines the details of the authentication methods and authorization types to use for access to your application.

The descriptor file is name **`xs-security.json`**.

The `xs-security.json` file uses the JSON notation to define the security options for an application; the information in the application-security file is used at application-deployment time, for example, for authentication and authorization.

Applications can perform:
 - scope checks (functional authorization checks with a boolean result), and
 - checks on attribute values (instance-based authorization checks).

Expand the **`forecast`** project folder.

Create a new file **`xs-security.json`**.

This is the full path of the created file: `forecast/xs-security.json`.

Paste the following content:

```JSON
{
  "xsappname": "forecast",
  "scopes": [{
    "name": "$XSAPPNAME.USER",
    "description": "Forecast User Application Scope"
  }],
  "role-templates": [{
    "name": "user",
    "description" : "Forecast User Role Template",
    "scope-references": ["$XSAPPNAME.USER"]
  }]
}
```

Save the file using the ![save](00-save_hxe2-0-04.png) icon from the menu.

As your application will be basic, you won't need to define multiple scope and role templates.

For more details, check the [The Application Security Descriptor](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/2.0.04/en-US/3bfb120045694e21bfadb1344a693d1f.html) documentation.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Add the XSUAA service as a Resource)]

In order to leverage the XSUAA service in your modules, you first need to add it as a resource in your project using the **`mta.yaml`** file.

Expand the **`forecast`** folder and open the **`mta.yaml`** file with the **MTA Editor** using a the right contextual menu and the **Open with** option.

![Web IDE](07-01.png)

Switch to the **Resources** tab.

Use the ![Web IDE](00-webide-plus.png) icon to add a new entry.

Enter the name of the XSUAA service **`forecast-uaa`** and select **`org.cloudfoundry.existing-service`** as type.

Add the following parameter:

Key           | Value
--------------|------------
`config-path` | `./xs-security.json`

![Web IDE](07-02__2020-08-13_17-16-41.png)


Save the file using the ![save](00-save_hxe2-0-04.png) icon from the menu.

This will result in the addition of the following content in the **`mta.yaml`** under the `resources` section:

```YAML
- name: forecast-uaa
  type: org.cloudfoundry.existing-service
  parameters:
    config-path: ./xs-security.json
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Add Module dependencies to XSUAA)]

Now, that the XSUAA service is defined as a resource in your project, you can add the dependency in your Node.js and HTML5 modules.

Open the **`mta.yaml`** file with the **MTA Editor**.

Switch to the **Modules** tab.

Select the **`js`** Node.js module, and locate the **Requires** section.

Use the ![Web IDE](00-webide-plus.png) icon to add a new entry.

Select the **`forecast-uaa (resource)`** entry from the drop down list.

![Web IDE](08-01__2020-08-13_16-47-08.png)

Save the file using the ![save](00-save_hxe2-0-04.png) icon from the menu.

Select the **`html`** HTML5 module, and locate the **Requires** section.

Use the ![Web IDE](00-webide-plus.png) icon to add a new entry.

Select the **`forecast-uaa (resource)`** entry from the drop down list.

![Web IDE](08-02__2020-08-13_16-49-13.png)

Save the file using the ![save](00-save_hxe2-0-04.png) icon from the menu.

This will result in the addition of the following content in the **`mta.yaml`** under the *requires* section for each module:

```YAML
- name: forecast-uaa
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Enable Destination Auth Token Forwarding)]

In your application, the **`html`** module uses the **`js`** module via the **`js_api (provider)`** as a destination.

Open the **`mta.yaml`** file with the **MTA Editor**.

Select the **`html`** HTML5 module, and locate the **Requires** section.

Select the **`js_api` (provider)** entry.

Use the ![Web IDE](00-webide-plus.png) icon for the **Properties of `js_api`** to add a new property.

Enter the following:

Key                | Value
-------------------|-------------------
`forwardAuthToken` | `true`

![Web IDE](09-01__2020-08-13_16-54-39.png)

Save the file using the ![save](00-save_hxe2-0-04.png) icon from the menu.

This will result in the following update in the **`mta.yaml`** under the `js_api` section for the `html` module:

```YAML
- name: js_api
  properties:
    name: forecast_api
    url: '~{url}'
    timeout: 3000000
    forwardAuthToken: true
  group: destinations
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Enable Node.js Module Authentication)]

By default, the authentication is disabled in your **`js`** module.

Expand the **`forecast\js`** folder and open the **`server.js`** file.

Comment the **anonymous** attribute in the **options** like this:

```js
...
var options = {
  // anonymous : true, // remove to authenticate calls
  ...
};
...
```

Save the file using the ![save](00-save_hxe2-0-04.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Validate your Node.js Module configuration)]

Right click on the **`js`** folder and select **Build**.

![Web IDE](11-01.png)

The console should display at the end the following message:

```
...
(Builder) Build of /forecast/js completed successfully.
```

Right click on the **`js`** folder and select **Run > Run as Node.js Application**.

Once the application is started, click on the application URL:

![Web IDE](11-02.png)

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Enable HTML5 Module Authentication)]

Just like with the **`js`** module, the authentication is disabled in your **`html`** module.

Expand the **`forecast\html`** folder.

Open the **`xs-app.json`** file.

Replace the current file content with the following code:

```JSON
{
	"welcomeFile": "webapp/index.html",
	"authenticationMethod": "route",
	"routes": [{
		"source": "/xsjs/(.*)(.xsjs)",
		"destination": "forecast_api",
		"csrfProtection": false,
		"authenticationType": "xsuaa",
		"scope": "$XSAPPNAME.USER"
	}, {
		"source": "/xsodata/(.*)(.xsodata)",
		"destination": "forecast_api",
		"csrfProtection": true,
		"authenticationType": "xsuaa",
		"scope": "$XSAPPNAME.USER"
	}, {
		"source": "^/(.*)$",
		"localDir": "resources",
		"scope": "$XSAPPNAME.USER"
	}]
}
```

The following modifications were applied:

 - the **`authenticationMethod`** is now set to **`route`** instead of **`none`** to force request to be authenticated,
 - the **`authenticationType`** is now set to **`xsuaa`** instead of **`none`** to enable the XSUAA service use,
 - an additional route for all requests is added to require the **`$XSAPPNAME.USER`** scope to be checked for any request.

Save the file using the ![save](00-save_hxe2-0-04.png) icon from the menu.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Validate your HTML5 Module configuration)]

Right click on the **`html`** folder and select **Build**.

![Web IDE](13-01.png)

The console should display at the end the following message:

```
...
(Builder) Build of /forecast/html completed successfully.
```

Right click on the **`html`** folder and select **Run > Run as Web Application**.

Once the application is started, click on the application URL.

![Web IDE](13-02.png)

Provide an answer to the question below then click on **Validate**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Run the application)]

Right click on the HTML5 Module application URL and use the **Open link in incognito/private window** mode from your browser.

![Web IDE](13-02.png)

You should now be able to get to the login page.

![Web IDE](14-01.png)

Use the **FORECAST** user created earlier.

You will be prompted to change its password on the first successful login, if you haven't changed the initial password yet.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Commit your changes)]

On the icon bar located on the right side of the Web IDE, click on the **Git Pane** icon ![Web IDE](00-webide-git.png).

Click on **Stage All**, enter a commit comment `Secure with XSUAA`, then click on **Commit and Push > origin master**.

[DONE]
[ACCORDION-END]
