---
title: Import your SAP Build project into SAP Web IDE
description: Once your prototype is complete, you can import it as a template into SAP Web IDE to convert it to SAPUI5 code.
auto_validation: false
primary_tag: products>build>build
tags: [  tutorial>beginner, topic>sapui5, products>build>build, products>sap-web-ide, products>sap-web-ide-plug-ins ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Details
### You will learn  
Learn how to import a completed and published prototype from SAP Build into SAP Web IDE. This will help you convert your prototype into a live application. This will be your guide or template for creating the real UI5/Fiori version.

> Please note that an imported SAP Build prototype **can not** be used for a production application for **free accounts** on SAP Build. The version of SAP Build used in these tutorials is for prototyping, designing, and trial purposes only. If your company has **purchased SAP Build**, you **can** use the following procedure to import your applications into your productive SAP Cloud Platform account.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Enable SAP Build services)]
Open your [SAP Cloud Platform account](https://account.hanatrial.ondemand.com/cockpit).

In your SAP Cloud Platform cockpit, go to the **Services**.

![Service tab in SAP Cloud Platform cockpit](1.png)

In the **Search Bar**, search for _BUILD_.

![Search in services](2.png)

Click on the **Build** service tile under the User Experience section.

![SAP Build tile in services](3.png)

**Enable the service**.

![SAP Build service overview](4.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Import the destinations)]
**Download** the four destinations files by **right clicking and selecting Save As** for each of the files.

- [BUILD Production Destination](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/build-import-webide/BUILD_Production)
- [SAPUI5 Destination](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/build-import-webide/sapui5-private)
- [SAPUI5 Private Destination](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/build-import-webide/sapui5-private-build)
- [SAP Web IDE Plugin](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/build-import-webide/Web_IDE_PLUGIN)

In your SAP Cloud Platform cockpit, under Connectivity, select **Destinations**.

![Destinations highlighted in cockpit](7.png)

You can import each of the downloaded destination files. To import, select **Import Destination**.

![import destination link](8.png)

Choose the `BUILD_Production*` file. Click **Save**.

![destination import 1](9a.png)

Select **Import Destination** again. Choose the `sapui5-private` destination. Click **Save**.

![destination import 2](9b.png)

Select **Import Destination** again. Choose the `sapui5-private-build` destination. Click **Save**.

![destination import 3](9c.png)

Select **Import Destination** again. Choose the `Web_IDE_PLUGIN` destination. Click **Save**.

![destination import 4](9d.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Open SAP Web IDE)]
In your SAP Cloud Platform cockpit, navigate back to **Services**.

Locate the **SAP Web IDE** service and **Go to Service**.

![SAP web IDE service launch link](10.png)

This will launch your SAP Web IDE.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enable the SAP Build plugin)]
In your SAP Web IDE, click on the **Gear Icon** to open the preferences view.

![gear icon highlighted in Web IDE](11.png)

Select **Plugins**.

![Plugins selected in preferences](12.png)

In the drop down menu, select **BUILD Destinations**. **Enable the Import from BUILD** plugin by clicking the switch on the right hand side.

![SAP Build filter in plugins](13.png)

Click **Save**.

![save highlighted in plugins](14.png)

Your SAP Web IDE will refresh. Click **Refresh**.

![refresh message for web IDE](15.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a new project)]
Go back to the code view by **clicking the angle brackets icon** (`</>`).

![code icon highlighted](16.png)

In the code view, create a new project by selecting **File > New > Project from Template**.

![file path for creating new project](17.png)

Under the _Category_ drop down, select **BUILD Project**. Select the **BUILD Project** tile. Click **Next**.

![Template selection for SAP Build projects](18.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Name the project)]
Give your project a **Name**. Click **Next**.

![name for project](19.png)

When prompted, enter your **BUILD Username and Password**. This will be your email address and password you used when signing up for SAP Build in the previous tutorial.

> **SAP EMPLOYEES**: If you are an SAP Employee, your SSO password will not work. To continue, you must follow the directions to [reset your SAP Build password](https://jam4.sapjam.com/groups/EwzMfO4LtSxrAjXwDipkgC/documents/ru6GmtH0bYRGEKqctpp8ye/slide_viewer). You are NOT resetting your SSO password, just the password to access SAP Build. If you have followed the above steps and you are still unable to login, you can alternatively download the project from SAP Build and import into SAP Web IDE.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Select a project to import)]


If you successfully logged in, you will see a list of your prototypes that you can import. Under the _BUILD System Information_, select **standard**.

Select **Inventory Dashboard** from the list of prototypes available. You always want to only import _High Fidelity_ prototypes.

![SAP Build system information selection on select build prototype step](20.png)

Click **Finish**.

![Finish button on select build prototype](21.png)

A new project will be created in your workspace.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run your application)]
**Right click** on your project name.

From the menu options, select **Run > Run as > Web Application**.

![right click path for selecting run as web application](22.png)

If prompted, select `testFLPService.html` as the file to run. Click **OK**.

![file selection in prompt](23.png)

Your application is now running on SAPUI5!

![live application in UI5 and Web IDE](24.png)

[VALIDATE_9]
[ACCORDION-END]
