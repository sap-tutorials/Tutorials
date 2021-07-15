---
title: Quick Start with the Mobile Development Kit (MDK)
description: Create and examine your first mobile (offline) and web application using the MDK template connecting against a sample service.
auto_validation: true
primary_tag: products>mobile-development-kit-client
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>mobile-development-kit-client, products>sap-mobile-services, products>sap-business-application-studio]
time: 15
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial group:** [Set Up for the Mobile Development Kit (MDK)](group.mobile-dev-kit-setup)
- **Download and install:** **SAP Mobile Services Client** on your [iOS](https://apps.apple.com/us/app/sap-mobile-services-client/id1413653544) or [Android](https://play.google.com/store/apps/details?id=com.sap.mobileservices.client) device (If you are connecting to `AliCloud` accounts then you will need to brand your [custom MDK client](cp-mobile-dev-kit-build-client) by allowing custom domains.)

## Details
### You will learn
  - How to create an MDK sample app using a template in SAP Business Application Studio
  - How to deploy an MDK app to Mobile Services and run it in mobile client
  - How to deploy an MDK app to Cloud Foundry and run it as a Web application

---

[ACCORDION-BEGIN [Step 1: ](Create a new MDK project in SAP Business Application Studio)]

This step includes creating the mobile development kit project in the editor.

1. Launch the [Dev space](cp-mobile-bas-setup) in SAP Business Application Studio.

2. Click **Start from template** on Welcome page.

    !![MDK](img-1.2.png)

    >If you do not see Welcome page, you can access it via **Help** menu.

3. Select **MDK Project** and click **Start**.

    !![MDK](img-1.3.png)

4. In *Type* step, provide the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `MDK template type`| Select `CRUD` from the dropdown |
    | `Your project name` | `MDKApp` |
    | `Your application name` | <default name is same as project name, you can provide any name of your choice> |

    !![MDK](img-1.4.png)

    >If you see *Cloud foundry token expired, continue without mobile services connection?* message, then set up the Cloud Foundry environment again by navigating to **View** menu > **Find Command**> **CF: Login to Cloud foundry** to initiate a valid session and click Start Over.

5. In *Service Name* step, provide or select the below information and click **Next**:

    | Field | Value |
    |----|----|
    | `Service File Name`| `<Provide any name of your choice>` |
    | `OData Source` | Select `Mobile Services` from the dropdown |
    | `Mobile Services Landscape` | Select `standard` from the dropdown |
    | `Application Id` | Select `com.sap.mdk.demo` from the dropdown (this app was configured as per [this](cp-mobile-dev-kit-ms-setup) tutorial) |
    | `Destination` | Select `SampleServiceV2` from the dropdown |
    | `Enter a path to the OData service` | Leave it as it is |
    | `Language URL` | Leave it with the default value |
    | `Enable Offline` | It's enabled by default |

    !![MDK](img-1.7.png)

    Regardless of whether you are creating an online or offline application, this step is needed for app to connect to an OData service. When building an MDK Mobile application, it assumes the OData service created and the destination that points to this service is set up in Mobile Services. For MDK Web application, destination is set up in SAP BTP cockpit.

    The generated application will be offline enabled in the MDK Mobile client and will run as online in Web environment.

8. In *Collection* step, select `Customers`, `Products`, `SalesOrderHeaders` and `SalesOrderItems`. Click **Finish** to complete the project creation.

    !![MDK](img-1.8.png)

9. After clicking **Next**, the wizard will generate your MDK Application based on your selections. You should now see the `MDKApp` project in the project explorer.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get familiar with generated project structure)]

This is how the project structure looks like within the workspace.

!![MDK](img-2.png)

These are the [metadata definitions](https://help.sap.com/doc/69c2ce3e50454264acf9cafe6c6e442c/Latest/en-US/docs-en/reference/schemadoc/App.schema.html) available in the editor and the format in which these metadata definitions are stored in the editor. Just to brief on some of these:

- **`InitializeOffline.action`**: For Mobile applications, this action binds the application to the Mobile Services Offline OData server and downloads the required data to the offline store on the mobile device. For Web applications, it will initialize the service to be consumed in online mode.

- **`DownloadOffline.action`** and **`UploadOffline.action`**: These actions are applicable to Mobile client only. Using app initialization, data is downloaded to the offline store. If you want to have the application download any updated data from the backend server or upload changed data to the backend server, these actions will be needed.

- **`Success & Failure Message action`**: Here are some messages showing up in the app on a successful or failure of data initialization, sync etc.

- **`Main.page`**: This is the first page of your MDK application that is shown. For this application you will use this as a launching page to get to application functionality.

- **`OnWillUpdate.js`**: This rule is applicable to Mobile client only. MDK applications automatically download updates and apply them to the client without the end-user needing to take any action. The `OnWillUpdate` rule empowers the user to run business logic before the new definitions are applied. This allows the app designer to include logic to prompt the user to accept or defer applying the new definitions based on their current activity. For example, if the end-user is currently adding new customer details or in the middle of a transaction, they will be able to defer the update. The app will prompt again the next time it checks for updates.

- **`Web`**: In this folder, you can provide web specific app resource files and configurations.

- **`Application.app`**: this is the main configuration file for your application from within SAP Business Application Studio. Here you define your start page (here in this tutorial, it is main.page), action settings for different stages of the application session lifecycle, push notifications, and more.

>Open the application settings in the application editor by clicking the `Application.app`.

>!![MDK](img-2.1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Deploy the application)]

So far, you have learned how to build an MDK application in the SAP Business Application Studio editor. Now, you will deploy this application definition to Mobile Services and Cloud Foundry to consume it as Mobile and Web application respectively.

1. Right-click `Application.app` and select **MDK: Deploy**.

    !![MDK](img-3.1.png)

2. Select deploy target as **Mobile & Cloud**.

   MDK editor will deploy the metadata to Mobile Services (for Mobile application) followed by to Cloud Foundry (for Web application).

   !![MDK](img-3.2.gif)

>First web deployment takes 2-3 minutes as it creates five service instances for the application, you can find these details in space cockpit.

>-	XSUAA

>- destination

>- connectivity

>- HTML Repo host

>- HTML repo runtime


You should see successful messages for both deployments.

!![MDK](img-3.3.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Display the QR code for onboarding the Mobile app)]

SAP Business Application Studio has a feature to generate QR code for onboarding the mobile app.

Double-click the `Application.app` to open it in MDK Application Editor and click **Application QR Code** icon to display the QR code.

!![MDK](img-4.1.png)

!![MDK](img-4.2.png)

>Leave the Onboarding dialog box open for step 5.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Run the app)]

[OPTION BEGIN [Android]]

>Make sure you are choosing the right device platform tab above. Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-Android-client/Onboarding-Android-client.md) to on-board the MDK client.

Once you accept app update, you will see the list of entities on the **Main** page, **LOGOUT** and **SYNC** options at bottom of the page and Offline store is being initialized. click either entity, it navigates to detail page, you can create, update, delete a record. This record gets saved to offline request queue database, navigate back to main page and press **SYNC** to upload local changes to the backend. Once the upload is successful, it will also download the data from the backend to the offline store to have same dataset on both ends.

![MDK](img_5.1.gif)

Additionally, you can search over all properties of the objects displayed in the section by entering manually or via barcode scanner. For example, in Products list, you can scan the barcode to search the products belong to _MP3 Players_ category.

!![MDK](img_5.3.gif)

>Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

[OPTION END]

[OPTION BEGIN [iOS]]

>Make sure you are choosing the right device platform tab above. Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

Follow [these steps](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/blob/master/Onboarding-iOS-client/Onboarding-iOS-client.md) to on-board the MDK client.

Once you accept app update, you will see the list of entities on the **Main** page, **Logout** and **Sync** options at bottom of the page and Offline store is being initialized. click either entity, it navigates to detail page, you can create, update, delete a record. This record gets saved to offline request queue database, navigate back to main page and press **Sync** to upload local changes to the backend. Once the upload is successful, it will also download the data from the backend to the offline store to have same dataset on both ends.

![MDK](img_5.2.gif)

Additionally, you can search over all properties of the objects displayed in the section by entering manually or via barcode scanner. For example, in Products list, you can scan the barcode to search the products belong to _MP3 Players_ category.

!![MDK](img_5.4.gif)

>Once you have scanned and onboarded using the onboarding URL, it will be remembered. When you Log out and onboard again, you will be asked either to continue to use current application or to scan new QR code.

[OPTION END]

[OPTION BEGIN [Web]]

1. Click the highlighted button to open the MDK Web application in a browser. Enter your SAP BTP credentials if asked.

    !![MDK](img-5.5.png)

    >You can also open the MDK web application by accessing its URL from `.project.json` file.
    !![MDK](img-5.6.png)

    You will see the list of entities on the **Main** page, **Logout** option at bottom of the page and application data service is being initialized. click either entity, it navigates to detail page, you can create, update, delete a record.

    !![MDK](img_5.7.gif)

[OPTION END]

[VALIDATE_1]
[ACCORDION-END]

---

Congratulations! You have created a Multi-Channel (Mobile & Web) application with just couple of clicks. You may now follow [these tutorials](mission.mobile-dev-kit-get-started) to create an MDK app from scratch.

---
