---
title: Create an MDK Offline App
description: Use the mobile development kit editor to create a mobile app.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 15
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial group:** [Set Up for the Mobile Development Kit (MDK)](group.mobile-dev-kit-setup)
- **Download and install:** **SAP Mobile Services Client** on your [iOS](https://apps.apple.com/us/app/sap-mobile-services-client/id1413653544) or [Android](https://play.google.com/store/apps/details?id=com.sap.mobileservices.client) device
- **Download and install** [Barcode Scanner](https://play.google.com/store/apps/details?id=com.google.zxing.client.android&hl=en) (required only for Android device)

## Details
### You will learn
  - How to create an MDK app using an existing template in SAP Web IDE
  - How to deploy an MDK app to Mobile Services and run it in a client

---

[ACCORDION-BEGIN [Step 1: ](Set up the application foundation)]

This step includes creating the Mobile Development Kit project in the Editor.

1. Launch the SAP Web IDE and select the **MDK perspective** by clicking on the icon in the left panel.

2. Right click Workspace folder and select **New** | **MDK Base Project**.

    ![MDK](img_001.png)

    >The _MDK Base Project_ template creates the offline or online actions, rules, messages and an empty page (`Main.page`). After using this template, you can focus on creating your pages, other actions, and rules needed for your application.

    >More details on _MDK template_ is available in [help documentation](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/cfd84e66bde44d8da09f250f1b8ecee6.html).    

3. Enter the Project Name as `DemoSampleApp` and click **Next**.

    ![MDK](img_002.png)

4. Leave the default values in _Application Creation_ step as it is, click **Next**.

5. In _Service Configuration_ step, provide and select the below information:

    | Field | Value |
    |----|----|
    | `Name`| `SampleServiceV2` |
    | `Service URL` | select `/destinations/mobileservices_cf` destination |
    | `Application ID` | `com.sap.mdk.demo` |
    | `Destination Name` | `com.sap.edm.sampleservice.v2` |
    | `Enable Offline Store` | `Should be checked` |

    > If you do not find `mobileservices_cf` destination, please ensure that you have followed [this tutorial](fiori-ios-hcpms-setup) to setup this destination in SAP Cloud Platform cockpit.

    >If you see a _Authentication Required_ pop-up, then enter your cloud platform User Name and password to authenticate.

    >For Offline OData capability only OData V2 is supported. OData V2 and V4 are supported for Online OData.

    ![MDK](img_004.png)

    >In [previous tutorial](cp-mobile-dev-kit-ms-setup), server-side configuration for this MDK app were already done.

    Regardless of whether you are creating an online or offline application, this step is needed app to connect to an OData service. When building an Mobile Development Kit application, it assumes the OData service created and the destination that points to this service is setup in Mobile Services.

    Since you will create an offline based app, hence _Enable Offline Store_ option is selected.

6. Click **Check Service** to validate the service properties. If all the details are fine, you will see a success message. Click **Next**.

    ![MDK](img_005.png)

    >Wondering how _Service URL_, _Application ID_ & _Destination Name_ were populated? As part of [enabled Mobile Services](fiori-ios-hcpms-setup) tutorial, you created a destination `mobileservices_cf` in Cloud Platform Cockpit, SAP Web IDE fetches all these details based on this destination.
    ![MDK](img_0031.1.png)

    >You can look in SAP Cloud Platform Mobile Services Cockpit for the destination belongs to the MDK app by clicking on **Mobile Connectivity** feature.
    ![MDK](img_0032.png)

    >More details on _Sample Back End_ is available in [help documentation](https://help.sap.com/viewer/468990a67780424a9e66eb096d4345bb/Cloud/en-US/1c2e51a24361487f8b0649702d59dd0f.html).

7. In the **Features** step, click the Queries drop down and select `Customers`, `Products`, `SalesOrderHeaders` and  `SalesOrderItems` and click **Finish**.

    ![MDK](img_006.png)

    After clicking Finish, the wizard will generate your MDK Application based on your selections. You should now see the `DemoSampleApp` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get familiar with generated project structure)]

This is how the project structure looks like within the workspace.

![MDK](img_007.png)

These are the metadata definitions available in the editor and the format in which these metadata definitions are stored in the editor. Just to brief on some of these -

**`InitializeOffline.action`**: For offline applications, this action binds the application to the Offline OData server and downloads the required data to the offline store on the mobile device.

**`DownloadOffline.action`** and **`UploadOffline.action`**: Using app initialization, data is downloaded to the offline store. If you want to have the application download any updated data from the backend server or upload changed data to the backend server, these actions will be needed.

**`Success & Failure Message action`**: Here are some messages showing up in the app on a successful or failure of data initialization, sync etc.

**`Main.page`**: This is the first page of your Mobile Development Kit application that is shown. For this application we will use this as a launching page to get to application functionality. We will add the logout action to this page.

**`OnWillUpdate.js`**: MDK applications automatically download updates and apply them to the client without the end-user needing to take any action. The `OnWillUpdate` rule empowers the user to run business logic before the new definitions are applied. This allows the application designer to include logic to prompt the user to accept or defer applying the new definitions based on their current activity. For example, if the end-user is currently adding new customer details or in the middle of a transaction, they will be able to defer the update. The application will prompt again the next time it checks for updates.

**`Application.app`**: this is the main configuration file for your application from within SAP Web IDE. Here you set your start page (here in this tutorial, it is main.page), action settings for different stages of the application session lifecycle, push notifications, and more.

>You can find more details about [metadata definitions](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/78e21fae61044df8ae8a78a43157fe8f.html).

Open the application settings in the application editor by double clicking on the Application.app in the project explorer pane.

![MDK](img_008.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Deploy and activate the application)]

So far, you have learned how to build an MDK application in the SAP Web IDE editor. Now, we deploy this application definition to Mobile Services.

1. Right click the `DemoSampleApp` MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

    ![MDK](img_009.1.1.png)

2. Let the default configuration as it is and click **Next**.

    ![MDK](img_010.png)

    >_Filter Files_ will be filtered and ignored in web packing process.

    >_Externals_ are the list of NPM modules that are part of the MDK Client application and should not be validated in the bundle.

3. Click the drop down for Destination Name and select the `mobileservices_cf` destination, you will find list of existing application IDs, select the one you have chosen while creating the project.

    ![MDK](img_014.1.png)

    >By default, automatically deploy option is selected, In other words, the application is automatically deployed from Mobile Services to your MDK client.

4. Click **Next** to finish the deployment from SAP Web IDE.

    You should see **Application deployed successfully** message in console log.

    ![MDK](img_015.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Populate the QR code for app on-boarding)]

SAP Web IDE has a feature to generate QR code for app on-boarding.

Right click the `DemoSampleApp` MDK Application in the project explorer pane and select **MDK Show QR Code**.

![MDK](img_009.png)

>**MDK Show QR Code** option is greyed out if MDK project is not yet deployed and activated as per step 3.

![MDK](img_012.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the app in MDK client)]

>Make sure you are choosing the right device platform tab above. Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, same onboarding URL settings will be reused without the need to scan. You will need to use 3rd party QR scanner app in Android or device Camera in iOS, if you would like to scan a different onboarding URL.

[OPTION BEGIN [Android]]

1. Launch **`Mobile Svcs`** app on your Android device. Tap **GET STARTED** to connect MDK client to SAP Cloud Platform.

    ![MDK](img_016.1.jpg)

2. Tap **QR CODE SCAN** to start the device camera for scanning the on-boarding QR code.

    ![MDK](img_013.2.png)

3. Once scan is succeeded, tap **CONTINUE**.

    ![MDK](img_013.3.png)

4. Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

    ![MDK](img_017.1.png)

5. Tap **AGREE** on `End User License Agreement`.

    ![MDK](img_018.1.jpg)

6. Choose a passcode with at least 8 characters for unlocking the app and tap **NEXT**.

    ![MDK](img_019.1.jpg)

7.  Confirm the passcode and tap **DONE**.

    ![MDK](img_021.1.png)

    Optionally, you can enable fingerprint to get faster access to the app data.

    ![MDK](img_022.1.png)

8. Tap **OK**.

    ![MDK](img_023.1.png)

    The MDK client receives deployed metadata definitions as a bundle.

    Now, you will see the **Main** page (with **Logout** and **Sync** options at bottom of the page) being displayed and Offline store is being initialized.

    ![MDK](img_023.1.jpg)

    >Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, same onboarding URL settings will be reused without the need to scan. You will need to use 3rd party QR scanner app in Android or device Camera in iOS, if you would like to scan a different onboarding URL.

[OPTION END]

[OPTION BEGIN [iOS]]

1. Launch **`Mobile Svcs`** app on your iOS device. Tap **Scan** to start the device camera for scanning the on-boarding QR code.

    ![MDK](img_013.png)

2. Once scan is succeeded, tap **Continue**.

    ![MDK](img_013.1.png)

3. Enter Email address and password to login to SAP Cloud Platform and tap **Log On** to authenticate.

    ![MDK](img_029.png)

4. Tap **Agree** on `End User License Agreement`.

    ![MDK](img_018.png)

5. Choose a passcode with at least 8 characters for unlocking the app and tap **Next**.

    ![MDK](img_019.png)

6. Confirm the passcode and tap **Done**.

    ![MDK](img_020.png)

    Optionally, you can enable Touch ID to get faster access to the app data, tap **Enable**.

    ![MDK](img_021.png)

7. Tap **OK**.

    ![MDK](img_022.png)

    The MDK client receives deployed metadata definitions as a bundle.

    Now, you will see the **Main** page (with **Logout** and **Sync** options at bottom of the page) being displayed and Offline store is being initialized.

    ![MDK](img_023.gif)

    >Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, same onboarding URL settings will be reused without the need to scan. You will need to use 3rd party QR scanner app in Android or device Camera in iOS, if you would like to scan a different onboarding URL.

[OPTION END]

You have successfully created an MDK offline app and you are now all set to [Create a Customer List Page](cp-mobile-dev-kit-list-page).

[VALIDATE_1]
[ACCORDION-END]

---
