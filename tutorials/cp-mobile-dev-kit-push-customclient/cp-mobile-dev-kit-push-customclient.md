---
title: Enable Push Notifications in Your Branded MDK Client
description: Use the SAP Mobile Services to enable push notifications in your branded MDK client.
auto_validation: true
primary_tag: software-product>mobile-development-kit-client
tags: [ tutorial>intermediate, operating-system>ios, operating-system>android, topic>mobile, software-product>sap-business-technology-platform, software-product>mobile-development-kit-client, software-product>sap-mobile-services ]
time: 35
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Prerequisites
- **Tutorial**: [Set Up Initial Configuration for an MDK App](cp-mobile-dev-kit-ms-setup)
- **Tutorial**: Follow steps 2-5 in [Enable Push Notifications in the MDK Public Store Client](cp-mobile-dev-kit-push) to create an MDK metadata project
- **Tutorial**: Follow steps 1-3 in [Build Your Mobile Development Kit Client Using MDK SDK](cp-mobile-dev-kit-build-client) to create your local MDK project
- **Apple ID**: A paid Apple developer account is required.

## Details
### You will learn
  - How to set up push notifications on an Apple Developer and Google Firebase account
  - How to configure push settings on SAP Mobile Services
  - How to send push notification in your branded MDK client

You may clone an existing metadata project from [GitHub repository](https://github.com/SAP-samples/cloud-mdk-tutorial-samples/tree/master/5-Brand-Your-Customized-App-with-Mobile-Development-Kit-SDK/1-Enable-Push-Notifications-in-your-branded-MDK-client).

---


[ACCORDION-BEGIN [Step 1: ](Generate push configuration for Android/iOS device)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. Open the [Firebase console](https://console.firebase.google.com/u/0/?pli=1), login with your Google account and click **Create Project** or **Add Project** (you will see this option if you already have any existing projects).

    !![MDK](img_1.1.png)

2. Provide a Project Name, click **Continue**.

    !![MDK](img_1.2.png)

3. Uncheck **Enable Google Analytics for this project** option and click **Create Project**.

    !![MDK](img_1.3.png)

4. Once the project is ready, click **Continue**.

    !![MDK](img_1.4.png)

5. Click **Android** icon to add Firebase to your Android app.

    !![MDK](img_1.5.png)

6. Provide a unique name to Android package name, click **Register app**.

    !![MDK](img_1.6.png)

7. `Download goole-services.json` file, click **Next**.

    !![MDK](img_1.7.png)

8. In **Add Firebase SDK** step, click **Next**.

9. In the following step, click **Next** and then click **Continue to console**.

    !![MDK](img_1.9.png)    

[OPTION END]

[OPTION BEGIN [iOS]]

In order to implement Push Notifications, a paid Apple developer account is required. Students or other developers with a personal Apple ID for their team will not be able to use push notifications, because they will not have access to the Developer Portal to generate the required certificate.

To enable your app for push notifications, you need to carry out the following tasks:

* Obtain a certificate signing request
* Register an iOS App ID
* Create a new development certificate `.cer` file
* Install the `.cer` file and create the .p12 file
* Register your device

1. Obtain a certificate signing request

    In order to use the **Apple Push Notification service**, you need to create a **CSR file**.

    On your Mac, open the **Keychain Access** application, and navigate to **Keychain Access > Certificate Assistant > Request a Certificate From a Certificate Authority...**

    !![MDK](img_1.10.png)

    In the dialog, enter the email address which is associated with your Apple Developer account. Also, make sure you check the **Request is saved to disk** option.

    !![MDK](img_1.11.png)

    Click **Continue**.

    Choose a folder to store the certificate -- it is good practice to store generated files in a separate folder for each project -- and click **Save**.

    Once you see a dialog saying the certificate is saved successfully, click **Done** to finish.

    !![MDK](img_1.12.png)

2. Register an iOS App ID

    Go to your [Apple Developer Account](https://developer.apple.com) and click **Certificates, Identifiers & Profiles**.

    !![MDK](img_1.12.1.png)

    Click **+** icon to register a unique **Identifiers** for your application.

    !![MDK](img_1.12.2.png)

    Select **App IDs** and click **Continue**.

    !![MDK](img_1.12.3.png)

    Provide a unique **Bundle ID** name and **Description**.

    !![MDK](img_1.12.4.png)

    Scroll down and select the **Push Notifications** capability from the list, click **Continue**.

    !![MDK](img_1.12.5.png)

    In the following screen, select option for **Deployment Details** and then click **Continue**.

    Confirm your App ID by clicking on **Register**.

3. Create a new development certificate `.cer` file

    Under **Identifiers**, search for the App ID that you registered in previous step.

    !![MDK](img_1.12.6.png)

    Scroll down and select the **Push Notifications** capability, click **Configure**.

    !![MDK](img_1.12.7.png)

    To configure push notifications for the App ID `com.sap.mdk.demo`, a Client SSL Certificate is required that will allow the notification server to connect to the Apple Push Notification Service. Each App ID requires its own Client SSL Certificate.

    Click **Create Certificate** to start the process for creating the needed `.cer` file.

    !![MDK](img_1.12.8.png)

    Click **Choose File** and browse to the downloaded Signing Request `CSR` file, click **Continue**.

    Apple will now create a `.cer` file for you which is issued by the **Apple Worldwide Developer Relations Certification Authority**.

    !![MDK](img_1.12.9.png)

    Click **Download** to download your certificate.

    !![MDK](img_1.12.10.png)

4. Install the `.cer` file and create the .p12 file

    In order to configure the `APNS` on **SAP Mobile Services**, you need to install the `.cer` file and create the needed `.p12` file.

    >A `.p12` file is a encrypted container for the certificate and private key. This file is needed by Mobile Services for the `APNS` configuration.

    Locate your downloaded `.cer` file and double-click it in order to install the certificate.

    >In case the **Add Certificate** dialog pops up make sure to choose **Login** from the dropdown and click **Add**.

    If the certificate is added correctly to the Keychain you should see it in the `MyCertificates` section, make sure you selected **login** as keychain.

    !![MDK](img_1.12.11.png)

    Select the certificate as well as the private key and right-click to export those two items.

    !![MDK](img_1.12.12.png)

    Make sure that in the dropdown **Personal Information Exchange (.p12)** is selected and click **Save**. You will be prompted to enter a password, click **OK** to export the files.

    !![MDK](img_1.12.13.png)

5. Register your device

    Click **+** icon to register your iOS device.

    !![MDK](img_1.12.14.png)

    Provide **Device Name** & **Device ID (UDID)** and then click **Continue**.

    !![MDK](img_1.12.15.png)

[OPTION END]

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Provide information to Mobile Services)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. In Firebase console page, click gear icon and then click **Project Settings**.

    !![MDK](img_2.1.png)

2. Navigate to **Cloud Messaging** tab, copy the **Server key**.

    !![MDK](img_2.2.png)

3. In Mobile Services cockpit, navigate to `Mobile Applications` | `Native/Hybrid` | `com.sap.mdk.demo` | `Mobile Push Notification`. Paste the Server Key value in **Android** push settings in Mobile Services cockpit.

    !![MDK](img-2.3.png)

4. Repeat the above step for **Sender ID** and then click **Save**.

[OPTION END]

[OPTION BEGIN [iOS]]

1. In Mobile Services cockpit, navigate to the **Mobile Push Notification** feature for app id `com.sap.mdk.demo`.

    !![MDK](img_2.4.png)

2. Provide the following details to the **Apple** panel and click **Save**.


    |  Field Name     | Value |
    |  :------------- | :------------- |
    |  APNS Endpoint | Select `Sandbox` from the dropdown |
    |  Authenticate | Choose `Certificate` |
    |  Certificate | `Browse to the `.p12`certificate you just exported` |
    |  Password | Enter the password you provided during the export |

    !![MDK](img-2.5.png)

You have now successfully configured the APNS Endpoint on the server side in Mobile Services cockpit.



[OPTION END]

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Place push files in local MDK project)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

Paste the downloaded `google-services.json` file to `/DemoSampleApp.mdkproject/App_Resources/Android/` path.

!![MDK](img-3.1.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Create a new file named as `app.entitlements` and place it under `/DemoSampleApp.mdkproject/App_Resources/iOS/` path.

    !![MDK](img-3.2.png)

2. Open this file and copy & paste the below information:

    ```XML
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
    <plist version="1.0">
    <dict>
    	<key>aps-environment</key>
    	<string>development</string>
    </dict>
    </plist>
    ```

    >Assign a value of development or production to aps-environment key, depending only on which activity you are creating the provisioning profile for.

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create MDK client)]

Follow step 4 from [this](cp-mobile-dev-kit-build-client) tutorial to create your branded MDK client.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Run the MDK client)]

>Make sure you are choosing the right device platform tab above.

[OPTION BEGIN [Android]]

1. Follow step 5 from [this](cp-mobile-dev-kit-build-client) tutorial to create your branded MDK client.

2. Once you accept the App update, a message should show **Push Notification registered**. Click **OK**.

    ![MDK](img_6.1.png)

3. It is time now to send the first push notification from the **SAP Mobile Services push notification feature**.

    Navigate to Mobile Services cockpit. In **Mobile Push Notification** feature, switch to **Push Registrations** tab.

    There you will find information about user registered for push notification and also details about Push providers. Identify your Device ID and click **Send Notification**.

    !![MDK](img_6.2.png)

4. In notification dialog, type a notification message and click **Send**.

    !![MDK](img_6.3.png)

    You will see a success toast message.

    !![MDK](img_6.3.1.png)

    After sending notification, mobile device should receive the message.

    ![MDK](img_6.3.2.png)

[OPTION END]

[OPTION BEGIN [iOS]]

1. Follow step 5 from [this](cp-mobile-dev-kit-build-client) tutorial to create your branded MDK client.

2. Once you accept the App update, you will also notice that it will first ask permission to display notifications.

    !![MDK](img_6.4.png)

3. If push registration is successful, a message should show **Push Notification registered**. Click **OK**.

    !![MDK](img_6.5.png)

4. It is time now to send the first push notification from the **SAP Mobile Services push notification feature**.

    Navigate to Mobile Services cockpit. In **Mobile Push Notification** feature, switch to **Push Registrations** tab.

    There you will find information about user registered for push notification and also details about Push providers. Identify your Device ID and click **Send Notification**.

    !![MDK](img_6.6.png)

5. In notification dialog, type a notification message and click **Send**.

    !![MDK](img_6.3.png)

    You will see a success toast message.

    !![MDK](img_6.3.1.png)

    After sending notification, mobile device should receive the message.

    ![MDK](img_6.9.png)

    If you have Apple watch connected to the iPhone device, you can also see same push notification on the Apple Watch.

    ![MDK](img_6.10.png)

    >MDK supports rich push notification. MDK does not run on smart watches or as an Apple watch application.

[OPTION END]

Congratulations, you have successfully enabled push notification in your branded client and you can continue with the remaining tutorials in this mission.

[VALIDATE_4]
[ACCORDION-END]

---
