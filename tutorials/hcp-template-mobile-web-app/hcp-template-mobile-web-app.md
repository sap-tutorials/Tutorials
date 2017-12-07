---
title: Build an app from an SAP Web IDE template
description: Build an app using the SAP Web IDE template wizard
primary_tag: products>sap-cloud-platform
tags: [ products>sap-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, topic>sapui5, tutorial>beginner ]
---
## Prerequisites
- **Proficiency:** Beginner
- **Tutorials:** [Create a Destination on SAP Cloud Platform](http://www.sap.com/developer/tutorials/hcp-create-destination.html)

## Next Steps
- [Deploy an app to SAP Cloud Platform](http://www.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html)

## Details

### You will learn
The SAP Web IDE (Integrated Development Environment) has a project template wizard, which makes it easy to create new applications quickly. In this tutorial you will build an app that reads product and supplier information from an OData source and displays it in a responsive mobile web app.

You will be setting a few configurations, then filling out a few forms in this step, but the end result is an application which can be run on a mobile device.  Ready?  Let's get started...

### Time to Complete
**10 min**


[ACCORDION-BEGIN [Step 1: ](Log into SAP Cloud Platform)]

1. Go to <https://account.hanatrial.ondemand.com> and log in to your SAP Cloud Platform cockpit.

![SAP Cloud Platform login page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_1.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open SAP Web IDE)]

To open SAP Web IDE, click on the **Services** tab in the navigation bar, scroll down and then click the **SAP Web IDE** tile to open the SAP Web IDE service page.

![SAP Cloud Platform Subscriptions page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_2.png)


On the service page, click on the **Open SAP Web IDE** link to open Web IDE in a new browser tab.

![SAP Web IDE status page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_3.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enable Hybrid App Toolkit plugin)]

The first configuration step is to enable the Hybrid App Toolkit plugin (this is required for Web IDE to show the hybrid template you will use later). In the Web IDE tab, click on the **Tools** menu, then **Preferences**.

![SAP Web IDE Tools menu](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_4.png)


In the Preferences page, click on **Plugins** on the left, scroll down to find the **Hybrid App Toolkit** plugin then enable it by clicking on the slider.

![SAP Web IDE plugins options](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_5.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Save the change)]

6. Click **Save**, and you will see a dialog box explaining that Web IDE will refresh. The purpose of the refresh is that after selecting the Hybrid App Toolkit plugin, Web IDE will download with the hybrid app configured templates you will use in this tutorial.

![mSAP Web IDE plugins reload page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_6.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open new project wizard)]

Once SAP Web IDE reloads, close the **Tips and Tricks** dialog box, then click on **File > New > Project from Template** to open the new project creation wizard.

![SAP Web IDE creating a new project from template](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_7.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Choose the template)]

On the **Template Selection** page, click on the **Category** pulldown menu (where you see **Featured**) and select **SAPUI5 Mobile Application**. When the mobile templates are displayed, select the **SAPUI5 Master Detail Kapsel Application** template, then click **Next**.

![SAP Web IDE template selection filtering](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_8a.png)

![Selecting a template in Web IDE](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_8b.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Name the project)]

On the **Basic Information** page of the New Project wizard enter the project name `northwind` and click **Next**. The project name will also become the name of your app when deployed.

![Entering the SAP Web IDE project name](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_9.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Select data source)]

The next step is to select the data source for your app. On the **Data Connection** page, click on **Service URL** as service source.

![Selecting the SAP Web IDE data source](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_10.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Select Northwind OData Service)]

If you only have one **`WebIDEEnabled`** destination in SAP Cloud Platform, it will be selected automatically. If you don't see the **Northwind OData Service** selected (the destination you created in the previous tutorial), click on the pull down menu and select it.

> Note: If you don't see the the **Northwind OData Service** in the pull down menu, go back to your destination in the SAP Cloud Platform cockpit and double-check the entries from the previous tutorial. If you make changes, reload Web IDE, then restart the procedure in this tutorial.

![Data connection](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_11.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Add OData path)]

After selecting the Northwind OData Service entry, enter the relative path (see below) to the OData service you will use in the field under the drop-down list-box (where it says "Paste URL here"). Be sure not to include any trailing space characters.

The relative path to enter is:

```
/V2/Northwind/Northwind.svc
```

The URL you entered for your destination plus the relative path you enter here points to the OData Service you will use for your app.

![Using the SAP Web IDE service pulldown menu](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_12.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Test your connection)]

Click the **Test** button to test the connection. If the connection is successful, the Service and its Collections of the Northwind OData Service will appear. This demonstrates that your destination is working properly. Click **Next** to advance to the **Template Customization** page.

![Entering the remaining portion of an OData URL](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_13.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Specify settings)]

On the **Template Customization** page you will specify the displayed values on the **Project Settings**, **Master Section** (Products List), **Main Data Fields**, the **Detail Section** (Selected Product Details) and the **Information Section** area.

![SAP Web IDE template customization page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_14.png)


Fill out the **Project Settings** and **Master Section** as displayed in the screenshot.

Field Name                  |  Value
:---------------------------| :-------------
Project Namespace           | `com.test.northwind`
Title                       | `Products`
OData Collection*           | `Products`
Search Placeholder          | `Search `
Search Tooltip              | `Search for product name`
Search Field                | `ProductName`

![SAP Web IDE template customization page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_15.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Add main data values)]

Scroll down to the **Main Data Fields Section**. Fill out the Main Data Fields as displayed in the screenshot.

Field Name          |  Value
:-------------------| :-------------
Item title          | `ProductName`
Numeric Attribute   | `UnitPrice`
Units Attribute     | `QuantityPerUnit`

![SAP Web IDE template customization page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_16.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Edit detail section)]

Scroll down to the **Detail Section**. Fill out the Detail Section as displayed in the screenshot.

Field Name              |  Value
:-----------------------| :-------------
Title                   | `Product Inventory Details`
Additional Attribute 1  | `UnitsInStock`
Additional Attribute 2  | `UnitsOnOrder`

![SAP Web IDE template customization page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_17.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Edit information section)]

Scroll down to the **Information Section**. Fill out the Information Section as displayed in the screenshot.

Field Name              |  Value
:-----------------------| :-------------
OData Navigations       | `Supplier`
Navigation Attribute 1  | `CompanyName`
Navigation Attribute 2  | `Phone`
Navigation Attribute 3  | `Address`

![SAP Web IDE template customization page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_18.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Click Finish)]

Click **Finish** to create the new Northwind application. When the generation finishes, click the **Northwind project folder icon** to see the project structure.

![SAP Web IDE project file view](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_19.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Run your app)]

To run your application, select the `index.html` file, and click the **Run** button. Your Northwind application will open in a Web IDE preview pane.

![How to run an SAP Web IDE project in preview mode](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_20.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 18: ](View completed app)]

Congratulations! You've developed your application that shows the products and supplier data!


![Finished mobile web app running in preview mode](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_21.png)


[ACCORDION-END]



## Next Steps
- [Deploy an app to SAP Cloud Platform](http://www.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html)
