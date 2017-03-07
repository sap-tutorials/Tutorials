---
title: Configure an SAP Web IDE project for hybrid builds
description: Learn how to modify the Device Configurations settings of a mobile web project for a hybrid build
tags: [products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, tutorial>intermediate ]
---

## Prerequisites  
- **Proficiency:** Intermediate
- **Tutorials:** [Create an app in SAP Cloud Platform mobile services](http://www.sap.com/developer/tutorials/hcpms-create-hybrid-app.html)


## Next Steps
- [Connecting SAP Web IDE to the Hybrid App Toolkit](http://www.sap.com/developer/tutorials/hcpms-webide-hat-connection.html)

## Details
### You will learn  
In this tutorial, set the target device configurations in SAP Web IDE which will be used when the project is compiled by the Hybrid App Toolkit.

### Time to Complete
**5 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Open SAP Web IDE)]

Open SAP Web IDE in a browser window.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open project settings)]

Right-click on the `northwind` project folder and select **Project Settings**

![Project settings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-config/2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Edit device configuration)]

Select **Device Configuration** in the Project Settings window.

![Project settings](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcpms-webide-hybrid-config/3.png)

In the Application section, fill in the information shown below.

Field Name          | Value
:------------------ | :-------------
App Name            | `Northwind `
App ID              | `com.northwind.hybrid`
Description         | `hybrid version of northwind web app`
Version             | `1.0.0`

> Note: The App ID field here must match the string you entered for **Application ID** when creating the HCPms application.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure build options)]

In the **Build Options** section, click the radio button for **Release Mode**.

In the **Platforms** section, select the options applicable for your development machine:
- 	Windows: Android
- 	Mac OS: iOS or Android

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure plug-ins)]

In the **Plugins** section, you specify the plugins the application will use.  For this exercise, do not check any of the standard Cordova plugins. Click the **Kapsel** tab, and select **Logon Manager**.

After selecting Logon Manager, the plugins section will update to allow you to select the type of server the app will connect to. Click the radio button next to **SAP Cloud Platform mobile services** and ensure your **HCPms Host** URL is correct.

> Note: Do not use the `hcpmsadmin-pxxxxxxxxx` form of the URL here.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Save and close)]

Leave the Preferences section unchanged, click **Save** and then **Close**.

[DONE]
[ACCORDION-END]


## Next Steps
- [Connecting SAP Web IDE to the Hybrid App Toolkit](http://www.sap.com/developer/tutorials/hcpms-webide-hat-connection.html)
