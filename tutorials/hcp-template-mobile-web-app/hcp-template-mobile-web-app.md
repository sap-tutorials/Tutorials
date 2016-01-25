---
title: Build an app from an SAP Web IDE template
description: Build an app using the SAP Web IDE template wizard
tags: [tutorial:product/hcp, tutorial:product/mobile, tutorial:interest/gettingstarted]
---
## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Create a Destination on HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-create-destination.html)

## Next Steps
[Deploy an app to SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html)

## Details

### You will learn
The SAP Web IDE (Integrated Development Environment) has a project template wizard, which makes it easy to create new applications quickly. In this tutorial you will build an app that reads product and supplier information from an OData source and displays it in a responsive mobile web app.

You will be setting a few configurations, then filling out a few forms in this step, but the end result is an application which can be run on a mobile device.  Ready?  Let's get started...

### Time to Complete
**10 min**


1. Go to <https://account.hanatrial.ondemand.com> and log in to your HCP cockpit.

    ![mob1-2_1.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_1.png)

2. To open SAP Web IDE, click on the **Subscriptions** tab in the navigation bar, then click the ```webide``` link in the **Application** column to open the Web IDE status page.

    ![mob1-2_1.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_1.png)

3. On the status page, click on the **Application URL** to open Web IDE in a new browser tab.

    ![mob1-2_3.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_3.png)

4. The first configuration step is to enable the Hybrid App Toolkit plugin (this is required for Web IDE to show the hybrid template you will use later). In the Web IDE tab, click on the **Tools** menu, then **Preferences**.

    ![mob1-2_4.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_4.png)

5. In the Preferences page, click on **Optional Plugins**, check the **Hybrid App Toolkit** checkbox and click **Save**.

    ![mob1-2_5.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_5.png)

6. Refresh the Web IDE browser tab to reload Web IDE. When you reload after selecting the Hybrid App Toolkit plugin, Web IDE will include the hybrid app configured templates you will use in this tutorial.

7. In the SAP Web IDE select the Local root, place your cursor over the **Local** folder icon and open the context menu by right-clicking. Choose **New > Project from Template** to open the new project creation wizard.

    ![mob1-2_7.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_7.png)

8. On the *Template Selection* page, click on the **SAPUI5 Master Detail Kapsel Application**  and click **Next**.

    ![mob1-2_8.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_8.png)

9. On the *Basic Information* page of the New Project wizard enter the project name **Northwind** and click **Next**.

    ![mob1-2_9.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_9.png)

10. On the *Data Connection* page, click on **Service URL** as service source.

    ![mob1-2_10.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_10.png)

11. From the drop-down list-box select the **Northwind OData Service** entry (which is the name of the destination you created).

    ![mob1-2_11.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_11.png)

12. After selecting the Northwind OData Service entry, enter the relative path (see below) to the OData service you will use in the field under the drop-down list-box (where it says “Paste URL here”). Be sure not to include any trailing space characters. The relative path to enter is: `/V2/Northwind/Northwind.svc`

    ![mob1-2_12.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_12.png)

13. Click the **run** (right arrow) button to test the connection. If the connection is successful, the `CollectionSets` (`Categories`, `CustomerDemographics`, etc.) of the Northwind OData Service will be displayed on the right side. This demonstrates that your destination is working properly. Click **Next** to advance to the *Template Customization* page.

    ![mob1-2_13.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_13.png)

14. On the *Template Customization* page you can customize the displayed values on the Project Settings, Master Section (Products List), Main Data Fields, the Detail Section (Selected Product Details) and the Information Section area.

    ![mob1-2_14.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_14.png)

15. Fill out the *Project Settings* and *Master Section* as displayed in the screenshot.

    **Project Namespace:** `com.test.northwind`
    **Title:** `Products`
    **OData Collection:**  `Products`
    **Search Placeholder:** `Search `
    **Search Tooltip:** `Search for product name`
    **Search Field:** `ProductName`

    ![mob1-2_15.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_15.png)

16. Scroll down to the *Main Data Fields Section*. Fill out the Main Data Fields as displayed in the screenshot.

    **Item title:** `ProductName`
    **Numeric Attribute:** `UnitPrice`
    **Units Attribute:** `QuantityPerUnit`

    ![mob1-2_16.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_16.png)

17. Scroll down to the *Detail Section*. Fill out the Detail Section as displayed in the screenshot.

    **Title:** `Product Inventory Details`
    **Additional Attribute 1:** `UnitsInStock`
    **Additional Attribute 2:** `UnitsOnOrder`

    ![mob1-2_17.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_17.png)

18. Scroll down to the *Information Section*. Fill out the Information Section as displayed in the screenshot.

    **OData Navigations:** `Supplier`
    **Navigation Attribute 1:** `CompanyName`
    **Navigation Attribute 2:** `Phone`
    **Navigation Attribute 3:** `Address`

    ![mob1-2_18.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_18.png)

19. Click **Next**, and then click **Finish** to create the new Northwind application. When the generation finishes, click the Northwind project folder icon to see the project structure.

    ![mob1-2_19.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_19.png)

20. To run your application, select the ```index.html``` file, and click the **Run** button. Your Northwind application will open in a Web IDE preview pane.

    ![mob1-2_20.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_20.png)

21. Congratulations! You’ve developed your application that shows the products and supplier data!


    ![mob1-2_21.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-template-mobile-web-app/mob1-2_21.png)

## Next Steps
[Deploy an app to SAP HANA Cloud Platform](http://go.sap.com/developer/tutorials/hcp-deploy-mobile-web-app.html)
