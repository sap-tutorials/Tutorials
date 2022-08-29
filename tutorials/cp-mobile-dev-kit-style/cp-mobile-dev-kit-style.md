---
title: Add Styling to an MDK App
description: Customize an MDK app to display styling to its controls.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services, software-product>sap-business-application-studio]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial group:** [Set Up for the Mobile Development Kit (MDK)](group.mobile-dev-kit-setup)
- **Download and install:** **SAP Mobile Services Client** on your [iOS](https://apps.apple.com/us/app/sap-mobile-services-client/id1413653544) or [Android](https://play.google.com/store/apps/details?id=com.sap.mobileservices.client) device (If you are connecting to `AliCloud` accounts then you will need to brand your [custom MDK client](cp-mobile-dev-kit-build-client) by allowing custom domains.)

## Details
### You will learn
  - How to change color of action bar and tool bar
  - How to change font color and background color of MDK control properties in a section page

You may clone an existing project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/4-Level-Up-with-the-Mobile-Development-Kit/3-Add-Styling-to-an-MDK-App) and start directly with step 4 in this tutorial.

---

![MDK](img-1.gif)


[ACCORDION-BEGIN [Step 1: ](Create a new MDK project in SAP Business Application Studio)]

This step includes creating the mobile development kit project in the editor.

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Click **Start from template** on Welcome page.

    !![MDK](img-1.2.png)

    >If you do not see the Welcome page, you can access it via **Help** menu or via **View** menu > Find Command > Welcome.

3. Select **MDK Project** and click **Start**.

    !![MDK](img-1.3.png)

    >If you do not see the **MDK Project** option check if your Dev Space has finished loading or reload the page in your browser and try again.

4. In *Type* step, select or provide the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `MDK Template Type`| Select `CRUD` from the dropdown |
    | `Your Project Name` | Provide a name of your choice. `MDK_Styling` is used for this tutorial |
    | `Your Application Name` | <default name is same as project name, you can provide any name of your choice> |
    | `Target MDK Client Version` | Leave the default selection as `MDK 6.0+ (For use with MDK 6.0 or later clients)` |
    | `Choose a target folder` | By default, the target folder uses project root path. However, you can choose a different folder path |

    !![MDK](img-1.4.png)

    >The `CRUD` template creates the offline or online actions, rules, messages, List Detail Pages with editable options. More details on _MDK template_ is available in [help documentation](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mdk/bas.html#creating-a-new-project-cloud-foundry).  

    >This screen will only show up when your CF login session has expired. Enter your login credentials, click Login icon and select the org & space where you have set up the initial configuration for your MDK app.

    >!![MDK](img-1.4.1.png)

7. In *Service configuration* step, provide or select the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `Data Source` | Select `Mobile Services` from the dropdown |
    | `Mobile Services Landscape` | Select `standard` from the dropdown |
    | `Application Id` | Select `com.sap.mdk.demo` from the dropdown |
    | `Destination` | Select `SampleServiceV2` from the dropdown |
    | `Enter a path to the OData service` | Leave it as it is |
    | `Enable Offline` | It's enabled by default |

    !![MDK](img-1.7.png)

    Regardless of whether you are creating an online or offline application, this step is needed app to connect to an OData service. When building an Mobile Development Kit application, it assumes the OData service created and the destination that points to this service is set up in Mobile Services. Since we have Enable Offline set to Yes, the generated application will be offline enabled in the MDK Mobile client.

8. In *Data Collections* step, select `Customers` and `Products`. Click **Finish** to complete the project creation.

    !![MDK](img-1.8.png)

9. After clicking **Finish**, the wizard will generate your MDK Application based on your selections. You should now see the `MDK_Styling` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add style metadata in LESS file)]

The `LESS` stylesheet provides the ability to define styling styles that can be used to style the UI in the MDK app.

>You can find more details about [styling in MDK](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Styles.less.html).

1. In `MDK_Styling` project, expand the **Styles** folder and open the `Styles.less` file.

    !![MDK](img_2.1.png)

2. Copy and paste the following code.

    ```LESS
    @mdkYellow1: #ffbb33;
    @mdkRed1: #ff0000;

    //// This style applies to all the ActionBars in the application
    ActionBar {
        color: white;
        background-color: red;
    }

    //// This style applies to all the ToolBars in the application
    ToolBar {
        color: white;
        background-color: gray; /* Android */
        bartintcolor: gray;     /* iOS */
    }

    //// LogoutToolbarItem is tool bar item for Logout in Main.page
    #LogoutToolbarItem  {
        color: brown;
    }

    //// UploadToolbarItem is tool bar item for Sync in Main.page
    #UploadToolbarItem  {
        color: blue;
    }

    //// By-Class style: These style classes can be referenced from rules and set using ClientAPI setStyle function
    //// below snippet is to style Customers button on Main.page
    .MyCustomerButton{
      font-color: @mdkRed1;
      background-color: cyan;
    }

    //// below snippet is to style Title property of an Object Table control in Customers_List.page
    .ObjectTableTitle {
     color: @mdkYellow1;
    }

    //// below snippet is to style Object Header control in Customers_Detail.page

    /* Object Header - BodyText */
    /* iOS Only */
    .objectHeaderBodyText {
      color: red;
    }

    /* Object Header - Description */
    /* iOS Only */
    .objectHeaderDescription {
      color: blue;
    }

    /* Object Header - Footnote */
    /* iOS Only */
    .objectHeaderFootNote {
      color: green;
    }

    /* Object Header - Headline */
    /* iOS Only */
    .objectHeaderHeadline {
      color: #ff00ff;
    }

    /* Object Header - Background */
    .objectHeaderBackground {
    background-color: #DC143C;
    }

    /* Object Header - StatusText */
    /* iOS Only */
    .objectHeaderStatus {
      color: red;
      font-style: italic;
      font-size: 18;
    }

    /* Object Header - Subhead */
    /* iOS Only */
    .objectHeaderSubhead {
      color: yellow;
    }

    /* Object Header - SubstatusText */
    /* iOS Only */
    .objectHeaderSubStatus {
      color: blue;
      font-style: italic;
      font-size: 18;
    }
    ```

    >To learn more on styling, find more details in [help documentation](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/Styles.less.html).

3. Save your changes to the `Styles.less` file.

    >`Styles.less` is already bound to _Styles_ properties in `Application.app` file.

    >!![MDK](img-2.3.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Set the styling for SDK controls)]

In this step, you will bind style classes:

* `MyCustomerButton` to `Customers` section button control on `Main.page`
* `ObjectTableTitle` to Title property of Object Table in `Customers_List.page`
* `objectHeaderBodyText` to `BodyText` property of Object Header in `Customers_Detail.page`
* `objectHeaderDescription` to `Description` property of Object Header in `Customers_Detail.page`
* `objectHeaderFootNote` to `Footnote` property of Object Header in `Customers_Detail.page`
* `objectHeaderHeadline` to `HeadlineText` property of Object Header in `Customers_Detail.page`
* `objectHeaderBackground` to `ObjectHeader` property of Object Header in `Customers_Detail.page`
* `objectHeaderStatus` to `StatusText` property of Object Header in `Customers_Detail.page`
* `objectHeaderSubhead` to `Subhead` property of Object Header in `Customers_Detail.page`
* `objectHeaderSubStatus` to `SubstatusText` property of Object Header in `Customers_Detail.page`

1.  In the `Main.page`, select `Customers` section button, click **link** icon next to **Style** property.

    In Object browser, double click `MyCustomerButton` class to bind style property and click **OK**.

    !![MDK](img-3.1.gif)

2. Navigate to **Pages** | **Customers**, click `Customers_List.page`, select **Object Table** control, scroll-down to **Style** section.

    Click **link** icon next to **Title** property.

    In Object browser, double-click `ObjectTableTitle` class to bind style property and click **OK**.

    !![MDK](img-3.2.png)

3. Navigate to **Pages** | **Customers**, click `Customers_Detail.page`, select **Object Header** control, scroll-down to **Style** section and bind control properties to style properties.

    !![MDK](img-3.3.png)
    !![MDK](img-3.4.png)


[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy the application)]

So far, you have learned how to build an MDK application in the SAP Business Application Studio editor. Now, you will deploy the application definitions to Mobile Services and Cloud Foundry to use it in the Mobile client and Web application respectively.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-4.1.png)

2. Select deploy target as **Mobile & Cloud**.

    MDK editor will deploy the metadata to Mobile Services (for Mobile client) followed by to Cloud Foundry (for Web application).

    !![MDK](img-4.2.png)

    You should see successful messages for both deployments.

    !![MDK](img-4.3.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Run the app)]

[OPTION BEGIN [Android]]

>Make sure you are choosing the right device platform tab above. Once you have scanned and on-boarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

SAP Business Application Studio has a feature to display the QR code for onboarding in the Mobile client.

1. Click the **Application.app** to open it in MDK Application Editor and then click the **Application QR Code** icon.

    !![MDK](img-5.1.png)

    The On-boarding QR code is now displayed.

    !![MDK](img-5.2.png)

3. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-Android-client/Onboarding-Android-client.md) to on-board the MDK client.

    After you accept the app update, you will see the list of entities on the **Main** page, **Logout** and **Sync** options at bottom of the page. Tap any entity, it navigates to a list page.  If you select one of the items, the detail page will be displayed. In Main page, you will notice styling on action bar, tool bar, items (Logout & Sync) available on tool bar and `Customers` button.

    ![MDK](img-6.4.png)

4. Tap **Customers** to navigate to Customer List. You will see that Title property has been styled.

    ![MDK](img_6.5.png)

5. Tap any record to navigate to Customer Detail page. You will see that Object Header control has been styled.

    ![MDK](img-6.6.png)       

[OPTION END]

[OPTION BEGIN [iOS]]

>Make sure you are choosing the right device platform tab above. Once you have scanned and on-boarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

SAP Business Application Studio has a feature to generate QR code for app onboarding.

Click the `Application.app` to open it in MDK Application Editor and click **Application QR Code** icon.

!![MDK](img-5.1.png)

The On-boarding QR code is now displayed.

!![MDK](img-5.2.png)

3. Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-iOS-client/Onboarding-iOS-client.md) to on-board the MDK client.

    After you accept the app update, you will see the list of entities on the **Main** page, **Logout** and **Sync** options at bottom of the page. Tap any entity, it navigates to a list page.  If you select one of the items, the detail page will be displayed. In Main page, you will notice styling on action bar, tool bar, items (Logout & Sync) available on tool bar and `Customers` button.

    ![MDK](img-6.1.png)

4. Tap **Customers** to navigate to Customer List. You will see that Title property has been styled.

    ![MDK](img-6.2.png)

5. Tap any record to navigate to Customer Detail page. You will see that Object Header control has been styled.

    ![MDK](img-6.3.png)   


[OPTION END]

[OPTION BEGIN [Web]]

1. Click the highlighted button to open the MDK Web application in a browser. Enter your SAP BTP credentials if asked.

    !![MDK](img-6.5.1.png)

    >You can also open the MDK web application by accessing its URL from `.project.json` file.
    !![MDK](img-6.5.2.png)

    You will see the list of entities on the **Main** page, **Logout** and **Sync** options at bottom of the page. Click any entity, it navigates to a list page.  If you select one of the items, the detail page will be displayed. In Main page, you will notice styling on action bar, tool bar, items (Logout & Sync) available on tool bar and `Customers` button.

    !![MDK](img-6.7.png)

2. Click **Customers** to navigate to Customer List. You will see that Title property has been styled.

    !![MDK](img-6.8.png)

4. Click any record to navigate to Customer Detail page. You will see that Object Header control has been styled.

    !![MDK](img-6.9.png)   


[OPTION END]

[VALIDATE_4]
[ACCORDION-END]

---
