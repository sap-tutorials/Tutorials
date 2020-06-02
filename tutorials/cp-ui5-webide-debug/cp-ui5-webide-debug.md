---
title: Debug Your SAPUI5 App with SAP Web IDE
description: Learn how to debug and test your web apps with the Google Chrome DevTools.
auto_validation: true
time: 20
tags: [ tutorial>beginner, topic>html5, topic>cloud, topic>javascript, products>sap-cloud-platform-for-the-cloud-foundry-environment, products>sap-web-ide]
primary_tag: topic>sapui5
---

## Details
### You will learn
  - How to do add breakpoints to your JavaScript code
  - How to log data to the console
  - How to use the Google Chrome DevTools

---

[ACCORDION-BEGIN [Step : ](Add a message page)]

Add a simple message page to let the users know you are still working on this app.

Replace the existing page in the file `tutorial/ui/webapp/view/View1.view.xml` with
```JavaScript [5]
<mvc:View controllerName="sapcp.tutorial.cf.ui.controller.View1" xmlns:mvc="sap.ui.core.mvc" displayBlock="true" xmlns="sap.m">
	<Shell id="shell">
		<App id="app">
			<pages>
				<MessagePage showHeader="false" description="More content to come" text="Stay tuned!" icon="sap-icon://home"/>
			</pages>
		</App>
	</Shell>
</mvc:View>
```


![message](./messagepage.png)



[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Change the log level)]

Add the following line to the `index.html` file to change to log level.
```HTML [14]
<!DOCTYPE html>
<html>
	<head>
		<meta charset="utf-8">
	    <meta name="viewport" content="width=device-width, initial-scale=1.0">
		<title>ui</title>
		<script id="sap-ui-bootstrap"
			src="https://sapui5.hana.ondemand.com/resources/sap-ui-core.js"
			data-sap-ui-theme="sap_fiori_3"
			data-sap-ui-resourceroots='{"sapcp.tutorial.cf.ui": "./"}'
			data-sap-ui-compatVersion="edge"
			data-sap-ui-oninit="module:sap/ui/core/ComponentSupport"
			data-sap-ui-async="true"
			data-sap-ui-logLevel="debug"
			data-sap-ui-frameOptions="trusted">
		</script>
	</head>
	<body class="sapUiBody">
		<div data-sap-ui-component data-name="sapcp.tutorial.cf.ui" data-id="container" data-settings='{"id" : "ui"}'></div>
	</body>
</html>
```
![loglevel](./loglevel.png)

> The log level acts like a filter and hides all log messages below the defined severity.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Write a log message to the console)]

Import the logger to the file `tutorial/ui/webapp/controller/View1.controller.xml` and add this `onBeforeRendering` as well. This `Log` object allows you to write messages to the console.

Replace the highlighted lines from this snippet:
```JavaScript [2-4,10-14]
sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/base/Log"
], function (Controller, Log) {
	"use strict";

	return Controller.extend("sapcp.tutorial.cf.ui.controller.View1", {
		onInit: function () {

		},

		onBeforeRendering: function () {
			Log.info("A random log message");
		},
	});
});
```

The controller code should look like this now:
![logger](./logger.png)


> This `onBeforeRendering` method is called every time the View is rendered, before the Renderer is called and the HTML is placed in the DOM-Tree. It can be used to perform clean-up-tasks before re-rendering.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: ](Add a breakpoint)]

Add this `onAfterRendering` hook to the same file to place a breakpoint in your code. A breakpoint will cause your app to stop when the execution thread reaches it. This gives you the chance to inspect the state of you app.
```JavaScript [16-18]
sap.ui.define([
	"sap/ui/core/mvc/Controller",
	"sap/base/Log"
], function (Controller, Log) {
	"use strict";

	return Controller.extend("sapcp.tutorial.cf.ui.controller.View1", {
		onInit: function () {

		},

		onBeforeRendering: function () {
			Log.info("A random log message");
		},

		onAfterRendering: function () {
			debugger
		}
	});
});
```

This `onAfterRendering` method is called every time the View is rendered, after the HTML is placed in the DOM-Tree. It can be used to apply additional changes to the DOM after the Renderer has finished.


![debugger](./debugger.png)

> Don't worry if the SAP Web IDE complains about the "debugger" keyword. This warning is valid as breakpoints should not be shipped in production-ready code but for the sake of learning, this is fine.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step: ](Run the latest version)]

Running your application has several advantages over deploying it. Among others, it's faster, doesn't require a "build" step and won't minify your JavaScript code base

1. To run the UI module, **right-click** on the module and select **Run** and **Run Configurations**.

    ![runconfig](./runconfig.png)


2. In the dialog, select **+** and **Run as Web Application** to add a new run configuration for you app.

    ![runconfigwebapp](./runconfigwebapp.png)


3. Specify the path to your main file with **`ui/webapp/index.html`** and select **Run on Cloud Foundry** to choose the target environment. Confirm your input with **Save and Run** .

    ![runconfigsave](./runconfigsave.png)


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Keep track of the progress in the console)]

You can see status messages during the start up of the module in the console of the Web IDE.

![runprocess](./runprocess.png)

The initial start up might take a few minutes, but only for the very first time.
After that, your changes can be run almost instantly when you click the green run button next time.

You might be prompted for you SAP Cloud Platform credentials when you try to access the running app. Use the same credentials as for the SAP Cloud Platform cockpit here.

![auth](./auth.png)

> In case you run into the following service broker error:
```
Could not create the '<appname>' instance of the 'destination' service type for the 'dest_mta_sapui5training' resource.
CF-ServiceBrokerBadResponse(10001): Service broker error: Quota limit exceeded. Instance creation not allowed
```
Please refer to this [community question](https://answers.sap.com/questions/12684501/error-while-running-sapui5-app-in-webide-service-b.html) and remove the existing service instances.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step: ](Stop at the breakpoint)]
Open the Chrome DevTools (or the [dev tools of your favorite browser](https://www.lifewire.com/web-browser-developer-tools-3988965)) by clicking **F12**. **Refresh the page** to run the script one more time.

You should now see that the app reached the breakpoint (the dev tools automatically switched to the `Sources` tab).

![stopped](./stopped.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Check the log message)]

Click **F8** to jump over the breakpoint and switch to the `Console` tab.
Now you should see your error message printed in red. Click on the small triangle on the left side to expand the error message.

Can you spot the line in which you invoked the error message?

![testlog](./testlog.png)

[VALIDATE_1]
[ACCORDION-END]

---
