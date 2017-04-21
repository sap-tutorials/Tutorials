---
title: Create an application definition
description: Create an application definition that enables you to manage the application. The application definition includes a unique application identifier, connections to the back-end data source, and optionally, other application-specific settings.
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Enable SAP Cloud Platform mobile service for development and operations](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-setup.html)

## Next Steps
 - [Implement Fiori for iOS controls](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-fioriuikit.html)

## Details
### You will learn  
How to setup the application for a mobile application that allows you to manage the application, connections to the back-end data source, and optionally, other application-specific settings.

### Time to Complete
**15 Min**.

---

The SAP Cloud Platform mobile service for development and operations provides various mobile centric services, such as authentication, device registration, logging and proxying of back-end services. All these services are app specific. This allows having several that are setup using different settings.

Technically it would be possible to skip this step and access an OData services directly, but leveraging the SAP Cloud Platform mobile service for development and operations has several advantages:

- Your app is always communicating with the same Mobile Services end-point. If you want to use another service, you don't have to change the app, as the eventual end-point stays hidden for the app. It suffices to point your mobile services end-point to the new service.
- Mobile services contains a device registration feature that allows a device to be de-authorized when the device is e.g. lost.
- Mobile services have features that allow the user not to have to re-authenticate every time they launch the app, as long as a token is securely stored and re-used when the application is launched again.
- Mobile services provide out-of-the-box features to configure your app on an app-specific, user-specific or device-specific level. It is e.g. possible to set a particular device of a user to debugging-level to gain better insight into what is happening with the application
- Mobile services provides analytics features to monitor usage of your application per platform, device type or user group.

This tutorial described how you can configure a mobile app in SAP Cloud Platform mobile service for development and operations.

[ACCORDION-BEGIN [Step 1: ](Make sure mobile services are enabled in your account)]

Got to the SAP Cloud Platform cockpit and click on **Services** in the main menu. In the `Services` page, make sure that under `Mobile Services` the `Development & Operations` service is enabled. If it is not enabled, enable it by clicking on it an pressing the **Enable** button.

![Enable Mobile Services](image-1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Go to the Mobile Services cockpit and bookmark it)]

After you clicked on the `Development & Operations` tile in the services list, you will see a link called **Go to Service**. Follow this link to go to the mobile services cockpit.

As it is likely that you need to return to this cockpit after you have created the application ID, it is advisable to bookmark the page by pressing **⌘D** (Control-D on Windows).

![Mobile Services Cockpit](image-2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create the application)]

To create the application, click on the **Applications** tile to go to the list of applications that you have already defined. When you do this for the first time, this list will be empty. A new application can be created by clicking on the **Create Application** button:

![Applications list](image-3.png)

A dialog will open, allowing you to enter the application details:

![Applications details](image-4.png)

Enter the details below:

Field Name                | Value
:-------------            | :-------------
Application ID            | `com.sap.tutorial.demoapp`
Name                      | `Demo Application`
type                      | `Native`
Ignore case for User Name | `checked`
Security Configuration    | `Basic`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Go back to the cockpit)]

After entering the application details press the **Save** button to save the new application. When the application is saved, you will be able to define more application configuration details. You will do this in the next tutorial, in which you will define the back-end connection. For now, just go back to the Mobile Services Cockpit by clicking on the `Home` button in the main menu.

When you have the cockpit on your screen, you can now see that you have defined one Application:

![One application in Mobile Services Cockpit](image-5.png)

[DONE]
[ACCORDION-END]

## Next Steps
 - [Implement Fiori for iOS controls](https://www.sap.com/developer/tutorials/fiori-ios-hcpms-fioriuikit.html)
