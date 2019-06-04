---
title: Create an MDK Online App
description: Use the mobile development kit editor to create a mobile app for online use case.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>mobile-development-kit-client, software-product-function>sap-cloud-platform-mobile-services ]
time: 20
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- [Set Up Initial Configuration for an MDK App](cp-mobile-dev-kit-ms-setup)
- [Enable SAP Web IDE Full-Stack](webide-multi-cloud)
- [Enable Mobile Services App Development Tools](cp-mobile-dev-tools-setup)
- Download and install **SAP Mobile Services Client** on your [iPhone](https://itunes.apple.com/us/app/sap-mobile-services-client/id1413653544?mt=8)

## Details
### You will learn
  - How to create an MDK Online app using an existing template in SAP Web IDE
  - How to deploy an MDK app to Mobile Services and run it in a client

---

[ACCORDION-BEGIN [Step 1: ](Get familiar with use case)]

With mobile development kit, you can also create applications that are online or always connected and make calls to the backend servers for each action you take in the application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up the application foundation)]

This includes creating the Mobile Development Kit project in the Editor.

Launch the SAP Web IDE and select the **MDK perspective** by clicking on the icon in the left panel.

Right-click on `Workspace` folder and select **New** | **MDK Base Project**.

![MDK](img_001.png)

Enter the **Project Name** as `MDKOnlineApp` and click **Next**.

![MDK](img_002.png)

Leave the default values in _Application Creation_ step as it is, click **Next**.

In _Service Creation_ step, provide and select the below information:

| Field | Value |
|----|----|
| `Name`| `SampleServiceV2` |
| `Service URL` | `/destinations/mobileservices` |
| `Application ID` | `com.sap.mdk.demo` |
| `Service URL` | `com.sap.edm.sampleservice.v2` |

>For Offline OData capability only OData V2 is supported. OData V2 and V4 are supported for Online OData.

![MDK](img_004.png)

>Server-side configuration for this MDK app were already done in [this tutorial](cp-mobile-dev-kit-ms-setup).

Regardless of whether you are creating an online or offline application, this step is needed app to connect to an OData service. When building an Mobile Development Kit application, it assumes the OData service created and the destination that points to this service is setup in Mobile Services and SAP Cloud Platform.

>Since you will create an online only based app, hence _Enable Offline Store_ option is unchecked.

Click **Check Service** to validate the service properties. If all the details are fine, you will see a success message. Click **Next**.

![MDK](img_005.png)

>Wondering how _Service URL_, _Application ID_ & _Destination Name_ were populated? Well, when you [enabled Mobile Services](fiori-ios-hcpms-setup), a destination `mobileservices` was created under **Destinations** in Cloud Platform Cockpit.
![MDK](img_0031.png)

>With help of `mobileservices` destination, SAP Web IDE fetches and populates all the MDK apps (of configuration template type as Mobile Development Kit) and their respective destinations.

>You can look in Mobile Services Cockpit for the destination belongs to the MDK app by clicking on **Connectivity** feature.
![MDK](img_0032.png)

>More details on _Sample Back End_ is available in
[help documentation](https://help.sap.com/viewer/38dbd9fbb49240f3b4d954e92335e670/Cloud/en-US/1c2e51a24361487f8b0649702d59dd0f.html).

Leave the default configuration as it is and click **Next** and then click **Finish**.

![MDK](img_006.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Get familar with generated project structure)]

This is how the project structure looks like within the workspace.

![MDK](img_007.png)

These are the metadata definitions available in the editor and the format in which these metadata definitions are stored in the editor. Just to brief on some of these:

  - **`CreateService.action`**: This action binds the OData service to your application.

  - **`OpenService.action`**: This action allows your application to see the data within the OData service.

  - **`CreateOpenService Success & Failure Message action`**: Here are some messages showing up in the app on a successful or failure of application data service initialization.

  - **`Main.page`**: This is the first page of your Mobile Development Kit application that is shown. For this application we will use this as a launching page to get to application functionality. We will add the logout action to this page.

  - **`OnWillUpdate.js`**: MDK applications automatically download updates and apply them to the client without the end-user needing to take any action. The `OnWillUpdate` rule empowers the user to run business logic before the new definitions are applied. This allows the application designer to include logic to prompt the user to accept or defer applying the new definitions based on their current activity. For example, if the end-user is currently adding new customer details or in the middle of a transaction, they will be able to defer the update. The application will prompt again the next time it checks for updates.

  - **`Application.app`**: this is the main configuration file for your application from within SAP Web IDE. Here you set your start page (here in this tutorial, it is main.page), action settings for different stages of the application session lifecycle, push notifications, and more.

>You can find more details about [metadata definitions](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/78e21fae61044df8ae8a78a43157fe8f.html).

Open the application settings in the application editor by double clicking on the Application.app in the project explorer pane.

![MDK](img_008.png)

>In MDK online use case, you need **create** and **open** service actions chained together for your online provider.

>`CreateService.action` should be set at `OnLaunch` and success of this action should call `OpenService.action`.
![MDK](img_008.1.png)

>You can find more details about [OData actions](https://help.sap.com/viewer/977416d43cd74bdc958289038749100e/Latest/en-US/3f462c086df846439d59fa88a2b4536b.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy and activate the application)]

So far, you have learnt how to build an MDK application in the SAP Web IDE editor. Now, we deploy this application definition to Mobile Services.

Right-click on the MDK Application in the project explorer pane and select **MDK Deploy and Activate**.

![MDK](img_009.png)

Let the default configuration as it is and click **Next**.

![MDK](img_010.png)

>_Filter Files_ will be filtered and ignored in web packing process.

>_Externals_ is a list of NPM modules to be excluded from the bundle.

Based on the `mobileservices` destination, you will find list of existing MDK application IDs , select the one you have chosen while creating the project in step 1

![MDK](img_011.png)

>By default, automatically deploy option is selected, In other words, the application is automatically deployed from Mobile Services to your MDK client.

Click on QR code icon to populate QR code for app on-boarding.

![MDK](img_012.png)

Open your phone camera app and start scanning the QR code, as shown below.

![MDK](img_013.png)

Click the toast message to launch **SAP Mobile Services Client**.

>At this moment, **SAP Mobile Services Client** is available only for iOS device.

Before you click on **Start** in client app, first finish the deployment from SAP Web IDE. Click **Next**.

![MDK](img_014.png)

You should see **Application deployed successfully** message in console log.

![MDK](img_015.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the app in MDK client)]

The MDK client receives deployed metadata definitions as a bundle.

Click **Start** to connect MDK client to SAP Cloud Platform.

![MDK](img_016.png)

Enter your SAP Cloud Platform credentials and click **Log On** to authenticate.

![MDK](img_017.png)

**Agree** on `End User License Agreement`.

![MDK](img_018.png)

Choose a passcode with at least 8 characters for unlocking the app and click **Next**.

![MDK](img_019.png)

Confirm the passcode and click **Done**.

![MDK](img_020.png)

Optionally, you can enable Touch ID to get faster access to the app data, click **Enable**.

![MDK](img_021.png)

Click **OK**.

![MDK](img_022.png)

Now, you will see Main page being displayed and application data service is initialized.

![MDK](img_023.png)

You can now continue creating your first page to show [customers list](https://developers.sap.com/tutorials/cp-mobile-dev-kit-list-page.html).

[VALIDATE_1]
[ACCORDION-END]

---
