---
title: Use SAP Wizard to Create Your First Android Application
description: Create an Android application that will be connected to an OData backend.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-android
tags: [  tutorial>beginner, operating-system>android, products>sap-cloud-platform-sdk-for-android ]
time: 15
---

## Details
### You will learn  
  - How to create an Android app using the Android Studio integrated SAP Wizard

---

[ACCORDION-BEGIN [Step 1: ](Set up development tools)]

If Android Studio is running, close it.

![Close Android Studio](close.png)

If there is an existing project at `C:\AndroidStudioProjects\WizApp`, delete it.

![Delete existing project](delete-old-project.png)

If there is an emulator running and an app on it named Wiz App, delete it (Long press, **App info**, then **UNINSTALL**).

![App Info](app-info.png)
![Uninstall App](uninstall.png)

Check if the emulator has airplane mode enabled.  

![Airplane mode enabled](airplane-mode-enabled.png)

If so, turn it off by swiping down from the top of the emulator and clicking the airplane icon.  

![Airplane mode disabled](airplane-mode-disabled.png)

All set, let's begin!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create and run the project)]
Open **Android Studio**.

![Android Studio](android-studio.png)

Choose **Start a new SAP Cloud Platform Android project**.

![New project](new-project.png)

> The SAP Cloud Platform SDK has been pre-installed on this machine from <a target="_blank" href="https://www.sap.com/developer/trials-downloads/additional-downloads/sap-cloud-platform-sdk-for-android-15508.html">Trial Downloads</a>. For further installation details see <a target="_blank" href="https://help.sap.com/doc/c2d571df73104f72b9f1b73e06c5609a/Latest/en-US/docs/user-guide/getting-started/installing.html">Installing the SAP Cloud Platform SDK for Android</a>.


The wizard can save the server connection details for multiple servers. A preconfigured account has been provided.

![Server connection](server-connection.png)

> If the values are missing, the following values can be used:

| Field | Value |
|:----|:----|
| Account Name | `SAP Cloud Platform Mobile Services Trial` |
| Admin API URL | `https://hcpms-p2000464045trial.hanatrial.ondemand.com/` |
| Admin UI URL | `https://hcpmsadmin-p2000464045trial.dispatcher.hanatrial.ondemand.com/?hc_reset` |
| Username | `p2000464045` |
| Password | `SCP!2pwd` |

On the **Cloud Configuration** tab, select **Use Existing** and set the Application ID to **`com.sap.wizapp`**.

![Cloud configuration](cloud-configuration.png)

The application configuration has already been created and has added multiple features of the SAP Cloud Platform Mobile Services.

![App features](appFeatures.png)

The SAP Cloud Platform Mobile Services provides a sample backend destination named `com.sap.edm.sampleservice.v2` that is being used here to provide data for the application. It contains product categories, product, supplier, customer and sales order data.


![OData services](odata-services.png)


On the **Project Configuration** tab, provide the following configuration data:

| Field | Value |
|:----|:----|
| Project Name | `Wiz App` |
| Project Namespace | `com.sap.wizapp` |
| Project Location | `C:\AndroidStudioProjects\WizApp` |
| Use Discovery Service for Application bootstrapping | uncheck |
| Generate Master-Detail Application | check |
| Enable Logging | check |
| Enable Upload | check |
| Enable Push | check and browse to `C:\AndroidStudioProjects\google-services.json` |


![Project configuration](project-configuration.png)

After clicking **Finish**, a project is created.

![Project created](project-created.png)

Click the **Run** toolbar icon.

![Run the project](run.png)

Choose the emulator to run the app on.

![Deployment target](choose-emulator.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Explore the app)]
The welcome screen is shown the first time the app is run.

![Welcome screen](welcome-screen.png)

Sign in with the following credentials:

| Field | Value |
|:----|:----|
| User Name | `p2000464045` |
| Password | `SCP!2pwd` |


![Authentication screen](authentication-screen.png)

The credentials are used to authenticate against the SAP Cloud Platform Identity Service, are  securely stored by the app, and do not need to be re-entered.

The passcode (or fingerprint if enabled) screen provides an additional layer of security for your app.

![Passcode screen](passcode-screen.png)

The first screen of the app shows the different entities that are in the sample OData service.

![Entities screen](entities-screen.png)

Tap on `Products` to reach the below list screen.

![Products screen](products-screen.png)

Tap on a list item to show an editable detail screen.

![Category detail](product-detail.png)

Congratulations!  You have created your first Android app using the SAP Cloud Platform SDK for Android!

[VALIDATE_1]
[ACCORDION-END]

---
