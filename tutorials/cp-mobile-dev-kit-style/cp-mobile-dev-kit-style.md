---
title: Add Styling to an MDK App
description: Customize an MDK app to display styling to its controls.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 25
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial:** [Set Up Initial Configuration for an MDK App](cp-mobile-dev-kit-ms-setup)
- **Tutorial:** [Enable SAP Web IDE Full-Stack](webide-multi-cloud)
- **Tutorial:** [Enable Mobile Services App Development Tools](cp-mobile-dev-tools-setup)
- **Download and install** **SAP Mobile Services Client** on your [iOS](https://itunes.apple.com/us/app/sap-mobile-services-client/id1413653544?mt=8) or [Android](https://play.google.com/store/apps/details?id=com.sap.mobileservices.client) device
- **Download and install** [Barcode Scanner](https://play.google.com/store/apps/details?id=com.google.zxing.client.android&hl=en) (required only for Android device)

## Details
### You will learn
  - How to change background color of a page
  - How to change color of action bar and tool bar
  - How to change font color of SDK control properties in a section page

---

MDK Template supports Style **LESS** file for styling for `NativeScript`, Android SDK & iOS SDK controls.

![MDK](img_1.gif)

[ACCORDION-BEGIN [Step 1: ](Create a new MDK project in SAP Web IDE)]

Make sure that you have already created a new destination `mobileservices_cf` as per [this tutorial](fiori-ios-hcpms-setup). This is required to connect SAP Web IDE to Mobile Services on Cloud Foundry environment.

This step includes creating the Mobile Development Kit project in the Editor.

Launch the SAP Web IDE and select the **MDK perspective** by clicking on the icon in the left panel.

Right click on Workspace folder and select **New** | **MDK CRUD Project**.

![MDK](img_001.1.png)

>_The MDK CRUD Project_ template creates the offline or online actions, rules, messages and list detail pages along with editable capability in respective pages. You can use such template to handle error archive situation.

>More details on _MDK template_ is available in
[help documentation](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/cfd84e66bde44d8da09f250f1b8ecee6.html).

Enter the Project Name as `MDK_Styling` and click **Next**.

![MDK](img_002.png)

Leave the default values in _Application Creation_ step as it is, click **Next**.

In _Service Creation_ step, provide and select the below information:

| Field | Value |
|----|----|
| `Name`| `SampleServiceV2` |
| `Service URL` | `/destinations/mobileservices_cf` |
| `Application ID` | `com.sap.mdk.demo` |
| `Service URL` | `com.sap.edm.sampleservice.v2` |
| `Enable Offline Store` | `Should be checked` |

>For Offline OData capability only OData V2 is supported. OData V2 and V4 are supported for Online OData.

![MDK](img_004.1.png)

Regardless of whether you are creating an online or offline application, this step is needed app to connect to an OData service. When building an Mobile Development Kit application, it assumes the OData service created and the destination that points to this service is setup in Mobile Services and SAP Cloud Platform.

Since you will create an offline based app, hence _Enable Offline Store_ option is selected.

Click **Check Service** to validate the service properties. If all the details are fine, you will see a success message. Click **Next**.

![MDK](img_005.png)

>More details on _Sample Back End_ is available in [help documentation](https://help.sap.com/viewer/468990a67780424a9e66eb096d4345bb/Cloud/en-US/1c2e51a24361487f8b0649702d59dd0f.html).

In **Metadata Source** step, select checkbox before **Entity Type** to select all available entities‚ and click **Next**.

![MDK](img_006.png)

In following steps go with default selections and **Finish** the project creation.

![MDK](img_006.gif)

After clicking Finish, the wizard will generate your MDK Application based on your selections.  You should now see the `MDK_Styling` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add style metadata in LESS file)]

The `LESS` stylesheet provides the ability to define styling styles that can be used to style the UI in the MDK app.
                           
>You can find more details about [styling in MDK](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/fb52430105254f9b8869cad9039c1529.html).

In the SAP Web IDE project, expand the **Styles** folder and open the `Styles.less` file.

![MDK](img_001.png)

Copy and paste the following code.

```LESS

@mdkYellow1: #ffbb33;
@mdkRed1: #ff0000;

//// By-Type style: All Pages in the application will now have a yellow background
Page
{background-color: @mdkYellow1;	}

//// This style applies to all the ActionBars in the application
ActionBar {
    color: white;
    background-color: red;
}

//// This style applies to all the ToolBars in the application
ToolBar {
    color: white;
    background-color: blue;
}

//// LogoutToolbarItem is tool bar item for Logout in Main.page
#LogoutToolbarItem  {
    color: brown;
}

//// UploadToolbarItem is tool bar item for Upload in Main.page
#UploadToolbarItem  {
    color: green;
}

//// By-Class style: These style classes can be referenced from rules and set using ClientAPI setStyle function
//// below snippet is to style SalesOrder button on Main.page
.MySalesOrderButton {
  color: @mdkRed1;
  background-color: black;
}

//// below snippet is to style Title property of an Object Table control in Customers_List.page
.ObjectTableTitle {
  font-color: @mdkYellow1;
  background-color: @mdkRed1;
}
```

![MDK](img_007.png)

Save your changes to the `Styles.less` file.

`Styles.less` is already bound to Styles properties in Application.app file.

![MDK](img_008.png)

>

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Set the styling for SDK controls)]

In this step, you will bind style classes:

* `MySalesOrderButton` to `SalesOrderHeaders` section button control on `Main.page`
* `ObjectTableTitle` to Title property of Object Table in `Customers_List.page`

Double-click on `Main.page`, select `SalesOrderHeaders` section button, click on **link** icon next to **Style** property.

![MDK](img_009.png)

In Object browser, select **SDK Style Classes** from dropdown, double-click on `MySalesOrderButton` class to bind style property and click **OK**.

![MDK](img_010.png)

Save the changes to `Main.page`.

Navigate to **Pages** | **Customers**, double-click on `Customers_List.page`, select **Object Table** control, scroll-down to **Style** section.

Click on **link** icon next to **Title** property.

![MDK](img_011.png)

In Object browser, select **SDK Style Classes** from dropdown, double-click on `ObjectTableTitle` class to bind style property and click **OK**.

![MDK](img_012.png)

Save the changes to `Customers_List.page`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy and activate the application)]

So far, you have learned how to build an MDK application in the SAP Web IDE editor. Now, we deploy this application definition to Mobile Services.

Right click on the `MDK_Styling` MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

![MDK](img_013.png)

Let the default configuration as it is and click **Next**.

![MDK](img_014.png)

>_Filter Files_ will be filtered and ignored in web packing process.

>_Externals_ is the list of NPM modules that are part of the MDK Client application and should not be validated in the bundle.

>By default, automatically deploy option is selected, In other words, the application is automatically deployed from Mobile Services to your MDK client.

Click the drop down for Destination Name and select the `mobileservices_cf` destination , you will find list of existing application IDs , select the one you have chosen while creating the project in step 1.

![MDK](img_015.png)

Click **Next** to finish the deployment from SAP Web IDE.

You should see **Application deployed successfully** message in console log.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Populate the QR code for app on-boarding)]

SAP Web IDE has a feature to generate QR code for app on-boarding.

Right click on the `MDK_Styling` MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

![MDK](img_013.png)

Let the default configuration as it is and click **Next**.

![MDK](img_014.png)

Click on QR code icon to populate QR code for app on-boarding.

![MDK](img_016.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the app in MDK client)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

On Android, camera app does not support scanning the QR code. You can use [Barcode Scanner](hhttps://play.google.com/store/apps/details?id=com.google.zxing.client.android&hl=en) app to scan it.

Open the Barcode scanner app and start scanning the QR code showing in SAP Web IDE.

Tap **Open browser**. It will open SAP Mobile Services Client app.

![MDK](img_017.jpg)

Tap **GET STARTED** to connect MDK client to SAP Cloud Platform.

![MDK](img_018.jpg)

Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

![MDK](img_019.png)

**AGREE** on `End User License Agreement`.

![MDK](img_020.png)

Choose a passcode with at least 8 characters for unlocking the app and tap **NEXT**.

![MDK](img_021.png)

Confirm the passcode and tap **DONE**.

![MDK](img_022.png)

Optionally, you can enable fingerprint to get faster access to the app data.

![MDK](img_023.png)

Tap **OK**.

![MDK](img_024.png)

The MDK client receives deployed metadata definitions as a bundle.

Now, you will see the **Main** page (with **LOGOUT** and **SYNC** options at bottom of the page) and list of entity sets to navigate to List-Detail pages. In Main page, you will notice styling on action bar, tool bar, items (Logout & Sync) available on tool bar, `SALESORDERHEADERS` button.

![MDK](img_025.png)

Tap on **CUSTOMERS** to navigate to Customer List. You will see that Title property has been styled.

![MDK](img_026.png)

[OPTION END]

[OPTION BEGIN [iOS]]

On iPhone, open your camera app and start scanning the QR code, as shown below.

![MDK](img_027.png)

Tap the toast message to launch **SAP Mobile Services Client**. It will open SAP Mobile Services Client app.

Tap **Start** to connect MDK client to SAP Cloud Platform.

![MDK](img_028.png)

Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

![MDK](img_029.png)

**Agree** on `End User License Agreement`.

![MDK](img_030.png)

Choose a passcode with at least 8 characters for unlocking the app and click **Next**.

![MDK](img_031.png)

Confirm the passcode and click **Done**.

![MDK](img_032.png)

Optionally, you can enable Touch ID to get faster access to the app data, click **Enable**.

![MDK](img_033.png)

Tap **OK**.

![MDK](img_034.png)

The MDK client receives deployed metadata definitions as a bundle.

Now, you will see the **Main** page (with **Logout** and **Sync** options at bottom of the page) and list of entity sets to navigate to List-Detail pages. In Main page, you will notice styling on action bar, tool bar, items (Logout & Sync) available on tool bar, `SalesOrderHeaders` button.

![MDK](img_035.png)

Tap on **Customers** to navigate to Customer List. You will see that Title property has been styled.

![MDK](img_036.png)

[OPTION END]

[DONE]
[ACCORDION-END]


---
