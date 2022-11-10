---
parser: v2
author_name: David Stepanov
author_profile: https://github.com/divadvo/
auto_validation: true
time: 45
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-intelligent-robotic-process-automation
---

# Capture Orders Management SAP UI5 application using Recorder
<!-- description --> Use the recorder for SAPUI5 to capture  Order Management application(Orders and Product details)

## Prerequisites
- You have subscribed to the RPA service: [Subscribe to SAP Intelligent RPA Service in SAP BTP](irpa-setup-1-booster-subscription)
- You have installed the on-premise components: [SAP Intelligent RPA On-premise Installation](irpa-setup-2-onpremise-installation)

## You will learn
  - How to use the recorder for SAPUI5 to prepare screens for automation
  - How to use **Multi Dimensional Collection**

---

### Create Project


Create a new project for this specific automation.

1.  In the Cloud Factory, select **Projects**.

2.  Select **New Project**.

    <!-- border -->![007_CreateProject](images/007_CreateProject.png)

3.  Type **Order Management** in the **Project name** field and click **Create**.

    <!-- border -->![008_NameProject](images/008_NameProject.png)

4.  The project will automatically open in the Cloud Studio.

    <!-- border -->![009_NewProjectHome](images/009_NewProjectHome.png)


### Screen Recording


Next, you define the application which will be used in your automation.

In this case you use a screen recorder because the application is a SAPUI5 application. You can use a screen recorder for all types of SAP GUIs (SAP GUI for Windows, SAP GUI for HTML, SAP UI5/Fiori). You will capture various screens and the elements on the web page.

You will use these screens and elements in the automation later.

### Create Application

1.  Make sure the [SAPUI5 application](https://openui5.hana.ondemand.com/test-resources/sap/m/demokit/orderbrowser/webapp/test/mockServer.html) is opened in a separate window, not just in a new tab. The Desktop Agent must be running and connected to the right tenant.

2.  In the Cloud Studio, click the **Create** button and then select the **Application** artifact from the artifact menu.

    <!-- border -->![010_CreateApplication](images/010_CreateApplication.png)

3.  Select the screen named **Browse Orders**. This is your SAPUI5 application.

    <!-- border -->![011_SelectApplication](images/011_SelectApplication.png)

    > **NOTE**: If the SAPUI5 application (step 1 - Create application) is not opened in a separate window, open it and click on the refresh icon to get the currently opened windows.

    <!-- border -->![012_SelectApplication](images/012_SelectApplication.png)

4.  Click the **Record** button.

    <!-- border -->![013_UI5_record](images/013_UI5_record.png)

5. The **Browse Orders** application opens and the recording controls appear.

    <!-- border -->![014_UI5_start](images/014_UI5_start.png)

    > **NOTE**: If the window with recording controls is not fully visible, click and hold on the drag section (refer to the following screenshot) and move the window. Release the left mouse button to finish.

    <!-- border -->![015_UI5_recorder_window](images/015_UI5_recorder_window.png)

### Record Application

Now, you record your first step.

1.  Click the **Record** button. Recording starts and your first screen is captured as shown in the following screenshot.

    <!-- border -->![016_UI5_first](images/016_UI5_first.png)

2.  Click on the search box in the SAPUI5 application window, type **Bottom-** and click the **Search** button. **Order 2686** appears and the new step is displayed in the recorder window as shown in the following screenshot.

    <!-- border -->![017_UI5_second](images/017_UI5_second.png)

3.  Now, click the **New screen capture** button.

    <!-- border -->![018_UI5_recorder_window_capture_button](images/018_UI5_recorder_window_capture_button.png)

    You will see a second line is added to the recording steps.

    <!-- border -->![019_UI5_recorder](images/019_UI5_recorder.png)

4.  Choose **Order 2686**.

    <!-- border -->![020_UI5_step](images/020_UI5_step.png)

    After the click, your screen must look like the following screenshot:

    <!-- border -->![021_UI5_result](images/021_UI5_result.png)

5. Click again the **New screen capture** button.

    <!-- border -->![022_UI5_recorder_window_capture_button](images/022_UI5_recorder_window_capture_button.png)

    Your screen must look like the following screenshot:

    <!-- border -->![023_UI5_result](images/023_UI5_result.png)

6.  Click the **Stop** button.

    <!-- border -->![024_UI5_step](images/024_UI5_step.png)

7.  Click the **Export** button in the screen recorder window to finish the capturing activity.

    <!-- border -->![025_UI5_step](images/025_UI5_step.png)

    Wait until the recording is saved. The Cloud Studio opens with the new automation.

    <!-- border -->![026_UI5_result](images/026_UI5_result.png)

### Test Application

Now, you can test the automation. This step is optional however it is highly recommended to check whether the automation runs smoothly.

1.  Close the SAPUI5 application (in a separate window) and click the **Test** button.

    <!-- border -->![027_UI5_test](images/027_UI5_test.png)

2.  Choose your environment and click the **Test** button.

    <!-- border -->![028_UI5_start_test](images/028_UI5_start_test.png)

3.  Wait until the package is generated and all **SDK**s are downloaded. You will see that the SAPUI5 application opens in a separate window, all steps are executed and the window gets closed. Test results are shown on the screen.

    <!-- border -->![029_UI5_test_result](images/029_UI5_test_result.png)

4.  Click the **Timeline** button to hide the test timeline panel.

    <!-- border -->![030_UI5_hide_test](images/030_UI5_hide_test.png)


### Adjust First Screen


By using the recorder, you already captured three screens that you will use in your automation. Now, you define various elements on those screens. You will use these screens and elements in the automation later.

You do not have anything to declare for your first screen because the screen recording has already captured and defined everything. You only need to rename the screen to make it more meaningful.

1.  Open the **Browse Orders** application tab.

2.  Select **Capture 1** element and change the name to **Order List Screen** then press enter.

    <!-- border -->![031_capture_1](images/031_capture_1.png)


### Adjust Second Screen


You rename the second screen from **Capture 2** to **Search Result Screen**.

<!-- border -->![032_capture_2](images/032_capture_2.png)

Next, you rename elements on the second screen.

2.  Make sure the second screen you just renamed to **Search Result Screen** is selected.

3.  Choose **Order 2686**.

4.  Rename the element to **Order Found**.

    <!-- border -->![033_SelectResultItem](images/033_SelectResultItem.png)


### Adjust Third Screen


Rename the last screen from **Capture 3** to **Order Details Screen**.

<!-- border -->![034_capture_3](images/034_capture_3.png)

Now, you need to define some new element of the third screen that you will use in your automation to get the data from the screen.

### Order Number

1.  Select the third screen you just renamed to **Order Details Screen**.

2.  Choose the order number in the preview.

3.  Rename the element to **Order Number**.

4.  Don't choose **Declare Element** yet.

    <!-- border -->![035_SelectOrder](images/035_SelectOrder.png)

    As you previously did, you are going to adjust the criteria.

5.  Choose the **Text** criteria.

6.  A popup opens. Change the **Operator** to **contains**.

7.  Change **Value** to **Order** with a space at the end.

8.  Click **Apply**.

    <!-- border -->![036_AdjustOrderCriteria](images/036_AdjustOrderCriteria.png)

9.  Click **Declare Element**.

    <!-- border -->![037_AdjustOrderCriteria-Declare](images/037_AdjustOrderCriteria-Declare.png)

### Price

1.  Make sure that the third screen, **Order Details Screen** is selected.

2.  Choose **Price** in the preview.

3.  Remove the **Text** criteria.

    <!-- border -->![038_PriceDeleteTextCriteria](images/038_PriceDeleteTextCriteria.png)

4.  Add the **ID** of the element as criteria instead. This will uniquely identify the price.

    <!-- border -->![039_SelectIdPrice](images/039_SelectIdPrice.png)

5.  Rename the element to **Price**.

6.  Make sure the correct **id** criterion is selected and click **Declare Element**.

    <!-- border -->![040_RenamePrice](images/040_RenamePrice.png)



### Define Shipping Address on Third Screen


Now, you define the Shipping Address in a different way, as a collection of multiple elements.

1.  Choose the **Name** under shipping address in the preview.

    <!-- border -->![041_SelectAddress](images/041_SelectAddress.png)

2.  Remove the **Text** criteria.

    <!-- border -->![042_RemoveAddressCriteria](images/042_RemoveAddressCriteria.png)

3.  Select **class** as element.

    <!-- border -->![043_AddressPickClass](images/043_AddressPickClass.png)

    With the SAP Intelligent RPA you can also see the underlying technical document structure of the page. You use this to select more precisely the element you would like to define.

4.  Click the **Both** tab on the top to show both the preview, as well as the technical DOM structure. Adjust the zoom (for example, to 50%) to be able to see the HTML elements and the screen.

5.  Choose the **Name** element of the Shipping Address. Select the **DIV** (the container), instead of the **SPAN**. This way, you will be able to connect it to the other elements of the address.

    <!-- border -->![044_OpenTree](images/044_OpenTree.png)

6.  **DIV** is selected. Now, remove the **Text** criteria as it is too specific.

    <!-- border -->![045_PickDivAbove](images/045_PickDivAbove.png)

7.  Add the **class** criteria instead.

    <!-- border -->![046_AddClassToRestrict](images/046_AddClassToRestrict.png)

8.  Class is selected. Set the name to **Shipping Address**.

9.  Click **Declare Element**.

    <!-- border -->![047_RenameShipping](images/047_RenameShipping.png)

    The element cannot be uniquely identified because multiple elements fulfil the same criteria. The SAP Intelligent RPA shows this in the warning status message in the **Declared Elements** section.

10. Create a collection of these multiple elements, by clicking the **Is a collection** button under **Declared Elements**, while **Shipping Address** is selected.

    <!-- border -->![048_ShippingAsCollection](images/048_ShippingAsCollection.png)

    As a result, a collection is defined and the element is uniquely identified. The technical page structure shows that multiple elements are part of this collection (0, 1, 2, 3, 4).

    <!-- border -->![049_ShippingResult](images/049_ShippingResult.png)

11. Don't forget to save the progress by choosing the **Save** button on the top right.


### Define Product Details(Line items) on Third Screen


>The automation displays product details, that is product name, unit price, quantity, and total amount of purchase order. You use **multiple collection** feature and retrieve the data.

1. Navigate to the **Order Details** screen.

2. Declare **TR** as an element and make sure you only have class as a recognition criterion.

3. Rename the declared element to **Table Row**.

    <!-- border -->![050](images/050bis.png)

4.  Since **TR** is a collection, make it as a collection by clicking on the three dots.

5.  Select the **Set as collection** button.

6.  Click **Declare Element**.

    <!-- border -->![051](images/051bis.png)

7.  Now, you see the **Table Row** element is added to the list of declared elements.

    <!-- border -->![052](images/052bis.png)

    <!-- border -->![053](images/053bis.png)    

    You need to retrieve that table data now, which is a cell.

9.  Select **TD**.

10. Remove **Text** and add class as a recognition criterion.

    <!-- border -->![054](images/054bis.png)

    Since **TD** is also a collection, it must be set as a collection.

10. Rename the **TD** to **Table Data**.

11. Select **Set as collection**.

12. Click **Declare Element**.

    <!-- border -->![055](images/055bis.png)

    You can see the one-dimension array added as of now. Since it is a collection, the **Table Data** is also a collection.

    <!-- border -->![056](images/056bis.png)

13. Click on the three dots on **Table Row**, to recognize the Table Data as two-dimensional.

14. Click **Add to criteria**.

    <!-- border -->![057](images/057bis.png)

    In the Element Information section, you can see that everything is added in the **Table Row**, and the **TD** is also marked as a two-dimensional array.


    <!-- border -->![Multidimension](images019_MultiDimension.png)

15. Click **Save** to save your automation.



Nice! You have completed the capturing of the application and can now start creating the actual automation. You can also continue with the second part of this tutorial: [Support multi-level collection in an Order Management automation](irpa-order-management-int-2-automation)




---
