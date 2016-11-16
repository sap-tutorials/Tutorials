---
title: Build an app from an SAP Web IDE template
description: Build an app using the SAP Web IDE template wizard
tags: [ products>sap-hana-cloud-platform, products>sap-web-ide, topic>cloud, topic>html5, topic>mobile, topic>odata, topic>sapui5, tutorial>beginner ]
---
## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Create an SAP HANA Cloud Platform destination pointing to an SAP Gateway](http://go.sap.com/developer/tutorials/teched-2016-3.html)

## Next Steps
 - [Deploy an app to SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-gw2.html)

## Details

### You will learn
The SAP Web IDE (Integrated Development Environment) has a project template wizard, which makes it easy to create new applications quickly. In this tutorial you will build an app that reads product and supplier information from an OData source and displays it in a responsive mobile web app.

You will be setting a few configurations, then filling out a few forms in this step, but the end result is an application which can be run on a mobile device.  Ready?  Let's get started...

### Time to Complete
**10 min**


1. Go to <https://account.hanatrial.ondemand.com> and log in to your HCP cockpit.

2. To open SAP Web IDE, click on the **Services** tab in the navigation bar, scroll down and then click the **SAP Web IDE** tile to open the SAP Web IDE service page.

    ![HCP Subscriptions page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_2.png)

3. On the service page, click on the **Open SAP Web IDE** link to open Web IDE in a new browser tab.

    ![SAP Web IDE status page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_3.png)

4. The first configuration step is to enable the Hybrid App Toolkit plugin (this is required for Web IDE to show the hybrid template you will use later). In the Web IDE tab, click on the **Tools** menu, then **Preferences**.

    ![SAP Web IDE Tools menu](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_4.png)

5. In the Preferences page, click on **Plugins** on the left, scroll down to find the **Hybrid App Toolkit** plugin then enable it by clicking on the slider.

    ![SAP Web IDE plugins options](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_5.png)

6. Click **Save**, and you will see a dialog box explaining that Web IDE will refresh. The purpose of the refresh is that after selecting the Hybrid App Toolkit plugin, Web IDE will download with the hybrid app configured templates you will use in this tutorial.

    ![mSAP Web IDE plugins reload page](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_6.png)

7. Once SAP Web IDE reloads, close the **Tips and Tricks** dialog box, then click on **File > New > Project from Template** to open the new project creation wizard.

    ![SAP Web IDE creating a new project from template](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_7.png)

8. On the **Template Selection** page, click on the **Category** pulldown menu (where you see **Featured**) and select **SAPUI5 Mobile Application**. When the mobile templates are displayed, select the **SAPUI5 Master Detail Kapsel Application** template, then click **Next**.

    ![SAP Web IDE template selection filtering](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_8a.png)

    ![Selecting a template in Web IDE](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_8b.png)

9. On the **Basic Information** page of the New Project wizard enter the project name `orders` and click **Next**. The project name will also become the name of your app when deployed.


10. The next step is to select the data source for your app. On the **Data Connection** page, click on **Service URL** as service source.


11.  If you only have one *`WebIDEEnabled`* destination in HCP, it will be selected automatically. If you don't see the **Gateway ES4 Service** selected (the destination your created in the previous tutorial), click on the pull down menu and select it.

    > Note: If you don't see the the **Gateway ES4 Service** in the pull down menu, go back to your destination in the HCP cockpit and double-check the entries from the previous tutorial. If you make changes, reload Web IDE, then restart the procedure in this tutorial.

12. After selecting the Northwind OData Service entry, enter the relative path (see below) to the OData service you will use in the field under the drop-down list-box (where it says **`Paste URL here`**). Be sure not to include any trailing space characters. 

    The relative path to enter is: 
    
    ```
    /sap/opu/odata/IWBEP/GWDEMO
    ```
    
    The URL you entered for your destination plus the relative path you enter here points to the OData Service you will use for your app.


13. Click the **Test** button to test the connection. If the connection is successful, the Service and its Collections of the Gateway OData Service will appear. This demonstrates that your destination is working properly. Click **Next** to advance to the **Template Customization** page.


14. On the **Template Customization** page you will specify the displayed values on the **Project Settings**, **Master Section** (Products List), **Main Data Fields**, the **Detail Section** (Selected Product Details) and the **Information Section** area.


15. Fill out the **Project Settings** and **Master Section** as displayed in the screenshot.

    Field Name                  |  Value
    :---------------------------| :-------------
    Project Namespace           | `com.test.northwind`
    Title                       | `Sales Orders`
    OData Collection*           | `SalesOrderCollection`
    Search Placeholder          | `Search `
    Search Tooltip              | `Search for customer name`
    Search Field                | `CustomerName`


16. Scroll down to the **Main Data Fields Section**. Fill out the Main Data Fields as displayed in the screenshot.

    Field Name          |  Value
    :-------------------| :-------------
    Item title          | `CustomerName`
    Numeric Attribute   | `TotalSum`
    Units Attribute     | `Currency`


17. Scroll down to the **Detail Section**. Fill out the Detail Section as displayed in the screenshot.

    Field Name              |  Value
    :-----------------------| :-------------
    Title                   | `Sales Order Details`
    Additional Attribute 1  | `DeliveryStatusDescription`
    Additional Attribute 2  | `Note`


18. Scroll down to the **Information Section**. Fill out the Information Section as displayed in the screenshot.

    Field Name              |  Value
    :-----------------------| :-------------
    OData Navigations       | `BusinessPartner`
    Navigation Attribute 1  | `Address/City`
    Navigation Attribute 2  | `Address/CountryText`


19. Click **Finish** to create the new application. When the generation finishes, click the **order** project folder icon to see the project structure.


20. To run your application, select the `index.html` file, and click the **Run** button. Your Northwind application will open in a Web IDE preview pane.


21. Congratulations! You’ve developed your application that shows sales orders and supplier data!


## Next Steps
 - [Deploy an app to SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-gw2.html)
