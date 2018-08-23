---
title: Import your TechEd BUILD project into SAP Web IDE
description: Once your prototype is complete, you can import it as a template into SAP Web IDE to convert it to UI5 code
auto_validation: true
primary_tag: products>sap-build
tags: [  tutorial>beginner, topic>sapui5, products>sap-build, products>sap-web-ide, products>sap-web-ide-plug-ins ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Details
### You will learn  
Learn how to import a completed and published prototype from BUILD into SAP Web IDE. This will help you convert your prototype into a live application. This will be your guide or template for creating the real UI5/Fiori version.

> Please note that an imported BUILD prototype should not be used for a production application. The version of BUILD used in these tutorials is for prototyping and designing purposes only. The code generated is not optimized for performance and security. Stay tuned to BUILD for updates about BUILD Apps, a drag and drop tool to create production ready applications!   

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Enable BUILD Services)]
Open your [SAP Cloud Platform account](https://account.hanatrial.ondemand.com/cockpit).

In your SAP Cloud Platform cockpit, go to the **Services**.

![service tab in SAP Cloud Platform cockpit](1.png)

In the **Search Bar**, search for _BUILD_.

![build search in services](2.png)

Click on the **BUILD** service tile under the User Experience section.

![build tile in services](3.png)

**Enable the service**.

![build service overview](4.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable SAP Web IDE Full-Stack)]
If you haven't enabled SAP Web IDE Full-Stack yet, you will need to do that as well.

In **Services**, search for _Web IDE_.

> NOTE: SAP Web IDE will be deprecated at the end of 2018. Please use SAP Web IDE Full-Stack for all current and future development, and begin migrating any projects from SAP Web IDE to SAP Web IDE Full-Stack to avoid any loses of code.

![services search for web IDE](5.png)

Click on the **SAP Web IDE Full-Stack** tile and **enable the service**.

![Web IDE Service Full-Stack overview](6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Open SAP Web IDE Full-Stack)]
In the **SAP Web IDE Full-Stack** service, scroll down to the _Take Action_ section. Click on **Go to Service**.

![SAP web IDE service launch link](10.png)

This will launch your SAP Web IDE Full-Stack.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enable the BUILD plugin)]
In your SAP Web IDE Full-Stack, click on the **Gear Icon** to open the preferences view.

![gear icon highlighted in Web IDE](11.png)

Select **Features** under the Workspace Preferences.

![Features selected in preferences](12.png)

In the search bar, type in **Build**. **Enable the Import Prototype from BUILD** feature by clicking the switch on the right hand side to change it from Off to On.

![BUILD filter in features list](13.png)

Click **Save**.

![save highlighted in features](14.png)

Your SAP Web IDE Full-Stack will refresh top enable the new features. Click **Refresh**.

![refresh message for SAP Web IDE Full-Stack](15.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a new project)]
Go back to the code view by **clicking the angle brackets icon** (`</>`).

![code icon highlighted](16.png)

In the code view, create a new project by selecting **File > New > Project from Template**.

![file path for creating new project](17.png)

Under the _Category_ drop down, select **BUILD Project**. Select the **BUILD Project** tile. Click **Next**.

![template selection for build projects](18.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Name the project)]
In the **Name** field, enter `te2018lotteryapp`. Click **Next**.

![name for project](19.png)

When prompted, enter your **BUILD Username and Password**. This will be your email address and password you used when signing up for BUILD in the previous tutorial.

> **SAP EMPLOYEES**: If you are an SAP Employee, your SSO password will not work. To continue, you must follow the directions to [reset your BUILD password](https://jam4.sapjam.com/groups/EwzMfO4LtSxrAjXwDipkgC/documents/ru6GmtH0bYRGEKqctpp8ye/slide_viewer). You are NOT resetting your SSO password, just the password to access BUILD. If you have followed the above steps and you are still unable to login, you can alternatively download the project from Build and import into SAP Web IDE.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Select a project to import)]


If you successfully logged in, you will see a list of your prototypes that you can import. Under the _BUILD System Information_, make sure the _BUILD System Information_ is set to **standard** Click on the **Select Prototype** button.

![Build system information selection on select build prototype step](20.png)

In the list of prototypes, select the **Inventory Dashboard** from the list. You always want to only import _High Fidelity_ prototypes.

![Select prototype options](20a.png)

Click **Finish**.

![finish button on select build prototype](21.png)

A new project will be created in your workspace.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Run your application)]
**Right click** on your project name.

From the menu options, select **Run > Run as > Web Application**.

![right click path for selecting run as web application](22.png)

If prompted, select `testFLPServiceMockService.html` as the file to run. Click **OK**.

![file selection in prompt](23.png)

Your application is now running on SAPUI5!

![live application in UI5 and Web IDE](24.png)

[VALIDATE_9]
[ACCORDION-END]
