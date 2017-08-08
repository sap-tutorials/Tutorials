---
title: Import your BUILD project into Web IDE
description: Once your prototype is complete, you can import it as a template into Web IDE to convert it to UI5 code.
primary_tag: products>build>build
tags: [  tutorial>beginner, topic>sapui5, products>build>build, products>sap-web-ide, products>sap-web-ide-plug-ins ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [Collect User Feedback in BUILD](http://www.sap.com/developer/tutorials/build-collect-feedback.html)


## Next Steps
 - Select a tutorial group from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)

## Details
### You will learn  
Describe what the user will learn from your tutorial and what the outcome will be.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Enable BUILD Services)]
If you haven't already, [Sign Up for a SAP Cloud Platform account](http://www.sap.com/developer/tutorials/hcp-create-trial-account.html).

In your SAP Cloud Platform cockpit, go to the **Services**.

![service tab in SAP Cloud Platform cockpit](1.png)

In the **Search Bar**, search for _BUILD_.

![build search in services](2.png)

Click on the **BUILD** service tile under the User Experience section.

![build tile in services](3.png)

**Enable the service**.

![build service overview](4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable Web IDE)]
If you haven't enabled Web IDE yet, you will need to do that as well.

In **Services**, search for _Web IDE_.

![services search for web IDE](5.png)

Click on the **SAP Web IDE** tile and **enable the service**.

![Web IDE Service overview](6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Import the destinations)]
In your SAP Cloud Platform cockpit, under Connectivity, select **Destinations**.

![Destinations highlighted in cockpit](7.png)

**Download** the four destinations files.

- [BUILD Production Destination](BUILD_Production)
- [SAPUI5 Destination](sapui5-private)
- [SAPUI5 Private Destination](sapui5-private-build)
- [Web IDE Plugin](Web_IDE_PLUGIN)

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

[ACCORDION-BEGIN [Step 4: ](Open Web IDE)]
In your SAP Cloud Platform cockpit, navigate back to **Services**.

Locate the **SAP Web IDE** service and **Go to Service**.

![web IDE service launch link](10.png)

This will launch your Web IDE.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Enable the BUILD plugin)]
In your Web IDE, click on the **Gear Icon** to open the preferences view.

![gear icon highlighted in Web IDE](11.png)

Select **Plugins**.

![Plugins selected in preferences](12.png)

In the drop down menu, select **BUILD Destinations**. **Enable the Import from BUILD** plugin by clicking the switch on the right hand side.

![BUILD filter in plugins](13.png)

Click **Save**.

![save highlighted in plugins](14.png)

Your Web IDE will refresh. Click **Refresh**.

![refresh message for web IDE](15.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create a new project)]
Go back to the code view by **clicking the angle brackets icon** (`</>`).

![code icon highlighted](16.png)

In the code view, create a new project by selecting **File > New > Project from Template**.

![file path for creating new project](17.png)

Under the _Category_ drop down, select **BUILD Project**. Select the **BUILD Project** tile. Click **Next**.

![template selection for build projects](18.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Name the project)]
Give your project a **Name**. Click **Next**.

![name for project](19.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Select a project to import)]
When prompted, enter your **BUILD Username and Password**.

If successful, you will see a list of your prototypes that you can import. Under the _BUILD System Information_, select **standard**.

Select **Inventory Dashboard** from the list of prototypes available. You always want to only import _High Fidelity_ prototypes.

![Build system information selection on select build prototype step](20.png)

Click **Finish**.

![finish button on select build prototype](21.png)

A new project will be created in your workspace.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Run your application)]
**Right click** on your project name.

From the menu options, select **Run > Run as > Web Application**.

![right click path for selecting run as web application](22.png)

If prompted, select `testFLPService.html` as the file to run. Click **OK**.

![file selection in prompt](23.png)

Your application is now live in UI5!

![live application in UI5 and Web IDE](24.png)

[DONE]
[ACCORDION-END]


## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
