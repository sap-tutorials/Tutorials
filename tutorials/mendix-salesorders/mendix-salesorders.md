---
title: Create Sales Order App with SAP RAD by Mendix
description: Create a sales order app that consumes a service from the SAP Gateway Demo System (ES5).
auto_validation: true
primary_tag: products>sap-rad-by-mendix
author_name: Paola Laufer
tags: [  tutorial>beginner, topic>cloud, topic>odata, products>sap-cloud-platform  ]
time: 20
---

## Prerequisites  
 - You are using a Windows desktop (or a Windows VM on a Mac).
 - You have access to an SAP BTP account. If not, you can open a trial account. See the [Get a Free Account on SAP BTP Trial](hcp-create-trial-account) tutorial or the [documentation](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/65d74d39cb3a4bf8910cd36ec54d2b99.html).
 - You have completed the [Get Started with SAP Rapid Application Development by Mendix](mendix-onboarding) tutorial. Name your app **SAP Sales Orders**.
 - You have completed the [Create an Account on the SAP Gateway Demo System](gateway-demo-signup) tutorial.


## Details
### You will learn  
- How to create an SAP Blank application in SAP BTP Rapid Application Development.
- How to create the application data model using the SAP OData Model Creator.
- How to consume an OData service using the SAP OData Connector.
- How to deploy the application to SAP Business Technology Platform (BTP).

You can build business applications for the SAP BTP, Cloud Foundry environment using SAP BTP Rapid Application Development by Mendix, without needing to write code.

This tutorial takes you through the basics of development in the Mendix Desktop Modeler and teaches you how to build a simple sales order application consuming the [`GWSAMPLE_BASIC service`](https://help.sap.com/viewer/68bf513362174d54b58cddec28794093/7.51.4/en-US/59283fc4528f486b83b1a58a4f1063c0.html) from the SAP Gateway Demo System (ES5).

This tutorial also showcases the [SAP OData Model Creator](https://appstore.home.mendix.com/link/app/105622/) and [SAP OData Connector](https://appstore.home.mendix.com/link/app/74525/), available in the [Mendix App Store](https://appstore.home.mendix.com/index3.html).

**Before starting this tutorial, make sure you have followed the prerequisites**.

>This tutorial assumes that you are using a trial Cloud Foundry environment, but it is applicable also to productive CF environment with minor changes.  
&nbsp;
>The Mendix Desktop Modeler, for building your application, is available for Windows platforms only.



---

[ACCORDION-BEGIN [Step 1: ](Get SAP OData Connector)]

The [SAP OData Connector](https://appstore.home.mendix.com/link/app/74525/Mendix/SAP-OData-Connector) is available for download from the Mendix App Store.

The **Fiori Blank** template you are using already contains this connector, locate it in your project's App Store modules.
For more information, see [How to Use App Store Content in the Modeler](https://docs.mendix.com/community/app-store/use-app-store-content-in-the-modeler).

![Mendix App Store](mendix-salesorders1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create GWSAMPLE_BASIC service module)]

The **SAP OData Model Creator** is a web site where your OData service metadata is transformed into a Mendix domain model that can be imported into your project.

You can generate this domain model by providing your service metadata as a file, URL, or select a service from SAP API Business Hub.

![OData Model Creator](mendix-salesorders1a.png)

We will do it by providing a file.
This file can be downloaded from the OData service URL directly using the `$metadata` suffix or retrieved from SAP Gateway. Since we're using the SAP Demo Gateway System (ES5) for this tutorial, the metadata file can be found at:

```
https://sapes5.sapdevcenter.com/sap/opu/odata/iwbep/GWSAMPLE_BASIC/$metadata
```

To generate the service module, follow these steps:

1. Save the metadata file to your hard drive.

2. Open the SAP [OData Model Creator](https://sapodatamodelcreator.mendixcloud.com/).

3. Select the **Manual** option.

4. Upload the OData metadata XML file and click **Continue**:

    ![Domain Model Creator](mendix-salesorders2.png)

5. Select the Schema and click **Continue**.

    ![Domain Model Creator Schema](mendix-salesorders2a.png)

6. Press **`Generate`** **`.mpk`**. A progress bar will be shown during the parsing and generation of the module.

7. Once the generation is done, the **Download** button appears. Notice that the file name of your module is extracted from the metadata file itself.

    Press **Download** and save the `.mpk` file locally.

    ![Domain Model Creator](mendix-salesorders3.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Import module in Mendix app)]

Now you have a Mendix module ready to be imported into your project.

1. Open the project that you created, right-click the project root folder, select **Import module package…** and select the `.mpk` file.

    ![Import module package](mendix-salesorders4.png) <br>

    You now have your `GWSAMPLE_BASIC` module available in your project ready to use in combination with the SAP OData Connector.

    ![Import module package](mendix-salesorders5.png)

2. Open the generated domain model and explore the entities and associations. Imagine that you had to create that manually -- **a lot of work!**

    ![Generated Domain Model](mendix-salesorders6.png)

3. In addition to the domain model, the OData Model Creator also created two other items:
    * A constant with the name of the service, containing the service root URL
    * An enumeration (`EntitySetNames`) containing a list of all the entity sets in the model

    ![Generated Domain Model](mendix-salesorders33.png)

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create constants for ES5 credentials)]

Now that we have our domain model in place, it's time to implement the connectivity to the SAP Gateway Demo System (ES5).

Our tutorial uses basic authentication to connect to the ES5 system, so let's store the username and password in constants.

1. Right-click **`MyFirstModule`** and add a new folder called **`Constants`**.

2. Right-click the **`Constants`** folder and add a new constant called **`ES5Username`**.

    ![Add Constant](mendix-salesorders7.png)

3. Enter your ES5 username as **Default value**.

    ![Add Constant](mendix-salesorders8.png)

4. Following the same steps, add a new constant and name it **`ES5Password`**.

5. Enter your ES5 password as **`Default value`**.

    ![Add Constant](mendix-salesorders9.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add logic to get sales orders – authentication)]

Follow these steps to create the logic to get the sales orders.

1. Right-click **`MyFirstModule`** and add a new microflow, name it **`ACT_GetSalesOrders`**.

2. Right-click the line between the green and red dots in the microflow editor and select **Insert** | **Activity** (or drag and drop an activity from the upper toolbar).

3. Double-click the new activity and scroll down to locate the SAP OData Connector actions.

4. Select **`Create`** **`Request`** **`Params`**, and then click **Select**.

5. Name the variable **`RequestParams`**. This variable (as its name suggests) will hold the request parameters, and it's required for the **Add basic authentication** activity.

    ![Create Request Params](mendix-salesorders10.png)

6. Following the same steps, add an **Add basic authentication** activity.

7. Select the **`$RequestParams`** variable in the **Request parameters** dropdown.

8. Click **Edit...** for the **Username** and select the **`ES5Username`** constant by entering the following argument: `@MyFirstModule.ES5Username`

    ![Add Basic Authentication](mendix-salesorders12.png)

9. Follow the same steps for the **Password**.

10. Change the output **Variable** name to **Authentication**.

11. Click **OK** to close the dialog.

![Add Basic Authentication](mendix-salesorders13.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add logic to get sales orders – request)]

1. Add another activity to the microflow and select the **Get List** action from the SAP OData Connector.
The **Get List** action retrieves a list of entities described in the domain model. In our case we will retrieve a list of sales orders.

2. Fill in the required fields of the **Get List** action. For this tutorial, use the following settings:

    | Field | Value |
    |:-------|:-------|
    | Query  | The URL to which you want to execute your request. In our case:<br><br> `https://sapes5.sapdevcenter.com/sap/opu/odata/iwbep/GWSAMPLE_BASIC/SalesOrderSet` <br><br> And it's constructed by entering the following code:<br><br> `@GWSAMPLE_BASIC.GWSAMPLE_BASIC + '/' + toString(GWSAMPLE_BASIC.EntitySetNames.SalesOrderSet)` |
    | Response type | The type you want to query from the OData service. Select the `SalesOrder` entity. |
    | Request&nbsp;parameters | `$RequestParams` variable |
    | Parent |empty|
    | Result info |empty|
    | Use&nbsp;Cloud&nbsp;Connector | `false` |
    | Output Variable | `SalesOrders` |

    > In our case, the `Use cloud connector` is set to `false` because ES5 is a publicly accessible system. <br>
    > If you would like to consume a service from your on-premise back-end system, you need to setup and configure the SAP Cloud Connector and then mark this field as `true`. <br>
    > When running the Mendix application on SAP BTP, the SAP Cloud Connector will automatically be utilized to gain access to your on-premise system. <br>
    > For more information, see the [SAP Cloud Connector](https://help.sap.com/viewer/cca91383641e40ffbe03bdc78f00f681/Cloud/en-US/e6c7616abb5710148cfcf3e75d96d596.html) documentation.


3. Verify the **Get List** dialog matches the following:

    ![Get List](mendix-salesorders14.png)

4. Click **OK** to close the dialog.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add logic to get sales orders – return value)]

In the microflow, make the return value of the microflow a **`List`** of **`SalesOrders`** so you can call the microflow as a data source in a page.

  1. Double-click the **`End-Event`** (red dot).

  2. Select **`List`** for the **Type**.

  3. Select **`SalesOrder`** for the **Entity**.

  4. Click on **Generate…** and select the **`SalesOrders`** variable.

      ![Expression value](mendix-salesorders15.png)

  5. Verify the **End Event** dialog matches the following:

      ![End Event](mendix-salesorders16.png)

  6. Click **OK** to close the dialog.

Your microflow should look like the following:
    ![Microflow](mendix-salesorders17.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create a new master detail page)]

1. Right-click **`MyFirstModule`**, and choose to add a Page.

2. Enter a name for the page (e.g. `MyFirstPage`).

3. Select the **`SAP_MasterDetail (SAP_UI_Resources)`** Navigation layout.

4. Select the **Fiori Master Lists** category on the left-side menu and select the **Master List** template.

    ![Add New Page](mendix-salesorders17a.png)

5. Click **OK**.

6. Notice the **Errors** tab located at the bottom pane is showing an error **`No entity configured for the data source of this list view`**.

    ![Add New Page](mendix-salesorders17d.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Show sales orders in page – master)]

Now we will bind the Master section to the Sales Orders.

1. Double-click the error from the **Errors** tab to get the **List View** of the **Master** section selected.

    ![Edit List View](mendix-salesorders17b.png)

2. Double-click the **List View**, and change its **Data Source** to your **`ACT_GetSalesOrders`** microflow.

    ![Edit List View](mendix-salesorders18.png)

3. When prompted to automatically fill the contents of the list view, choose **No**.

4. Select the content of the **List View**, right-click it and choose **Delete**.

    ![List View Content](mendix-salesorders18a.png)

5. Open the **Connector** tab on the right-side pane to view the properties of the `Sales Order` entity.

    ![Open Connector Tab](mendix-salesorders19.png)

6. Double-click (or drag and drop) the **`CustomerID`** property from the **Connector** tab.

7. In the page, select the content below the **`Customer ID`** you just added and add the **`CustomerName`** property.

8. Select the content below it and add the **`CreatedAt`** property.

9. Verify it looks like the following:

    ![List View Content Final](mendix-salesorders20.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Show sales orders in page – details)]

Now we're going to make some modifications in the Detail section of the page.

1. Notice the **Errors** tab is showing several errors regarding fields that aren't bound to any property in the Detail section. Scroll down, select the **Tab Container**, and delete it. This should resolve the errors.

    ![Tab Container](mendix-salesorders21.png)

2. Scroll down, select the footer container, and delete it as well.

    ![Footer](mendix-salesorders22.png)

3. Scroll back up, double-click the **Title** text and change it to **`Sales Order`**.

4. Double-click the **Subtitle** text and change it to **`Details`**.

5. Select the container with the `New` and `Delete` buttons and delete it.

    ![Delete Container](mendix-salesorders22a.png)

Now let's bind the Detail section and present some more Sales Order properties:

6. You can see 3 containers with `Category` and `Value` texts.

    ![3 Containers](mendix-salesorders22b.png)

7. Select the first container and delete both `Category` and `Value` texts.

8. Double-click the **`SalesOrderID`** property from the Connector tab.

    ![Container for SalesOrderID](mendix-salesorders23.png)

9. Select the second container and delete both texts again.

10. Double-click the **`CurrencyCode`** and the **`GrossAmount`** properties.

    ![Container for CurrencyCode](mendix-salesorders24.png)

11. Select the third container and delete both texts again.

12. Double-click the **`Note`**, **`LifecycleStatusDescripion`** and **`BillingStatusDescription`** properties.

13. Verify the UI looks like the following:

    ![Container for Note](mendix-salesorders25.png)

14. Scroll up in the page, double-click the **Page Title** text and change it to **Sales Orders Application**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Configure Home Page)]

Mendix apps work by showing pages to the user. You can define which page should be the Home page, i.e. the first page the user sees.

1. Right-click **Project** | **Navigation** and click **Open**.

    ![Open Navigation](mendix-salesorders26.png)

2. Click **`Select…`** next to **Default home page**.

3. Select the new page you created in the previous step as the new home page.

4. Click **Select**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Run app)]

Now that you've created the UI and business logic, you can run the app and have it connect with the SAP Gateway Demo System (ES5).

To run the app for the first time, follow these steps.

1. Click **Run** | **Run Locally**.

    ![Run Locally](mendix-salesorders28.png) <br>

    If you see the pop-up window asking if you want to create a database, select **Yes**. <br>
    If you see the pop-up window asking if you want to save your changed, select **Save and continue**.

2. Wait until the startup of the app has finished and the app is running.

3. Click **View** to view the app in your browser.

    ![View App](mendix-salesorders29.png)

You will now see your Sales Orders Application in the browser, with live data coming from ES5.

  ![View App](mendix-salesorders30.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Run app in SAP BTP)]

The final step is to deploy the application to SAP BTP, Cloud Foundry environment, and run it from your space!
The application will automatically bind to the Connectivity, XSUAA and PostgreSQL service instances.

1. Click **Run**.

    ![Run](mendix-salesorders31.png) <br>

    The deployment process will start and you will be notified when completed.

    ![Deploy](mendix-salesorders32.png)

2. Once the application is deployed successfully, click on **View** to run it from SAP BTP.

[DONE]
[ACCORDION-END]

### Additional Information
- Read more about using Mendix at [https://docs.mendix.com/howto/](https://docs.mendix.com/howto/).
