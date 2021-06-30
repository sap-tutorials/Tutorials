---
title: Set Up SAP Mobile Cards
description: Activate SAP Mobile Services and connect the mobile application to your trial account.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-business-technology-platform, products>sap-mobile-cards, products>sap-mobile-services ]
time: 10
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---

## Prerequisites

- [Get a Free Trial Account on SAP BTP](hcp-create-trial-account)
- [Enable SAP Mobile Services](fiori-ios-hcpms-setup)
- **Install SAP Mobile Cards Application:**
   <table><tr><td align="center">!![Play Store QR Code](pre_qr_android.png)<br>Android</td><td align="center">!![App Store QR Code](pre_qr_ios.png)<br>iOS</td></tr></table>

## Details

### You will learn

- How to connect the SAP Mobile Cards application to your SAP Mobile Services

---

[ACCORDION-BEGIN [Step 1: ](Understand SAP Mobile Cards)]

SAP Mobile Cards is a feature within SAP Mobile Services which provides customers access to a micro-application platform to publish data into a consumer-grade wallet or passbook-style app.

It allows companies to quickly create simple, yet highly valuable quick-win apps. These apps can give access to useful organisational tools like to do lists, payslips, time sheets and workflows like Leave request approvals, or Purchase Order approvals.

You can read more about the features [here](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-overview.html).

You can see the list of templates shipped out of the box [here](https://github.com/SAP-samples/mobile-cards-templates).

!![MobileCards](img_1_1.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Go to SAP Mobile Services cockpit)]

1. In your web browser, open the [SAP BTP trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Provide the login details and click **Log On**.

    !![SAP BTP Log On Screen](img_2_2.png)

3. Navigate to the trial global account by clicking **Enter Your Trial Account**.

    !![Enter Global Account](img_2_3.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region (select the region closest to you). Your user profile will be set up for you automatically.  

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    >!![Enable SAP BTP CF Trial](img_2_3_note.png)

4. Navigate to your subaccount by clicking on the tile named **trial**.

    !![Trial Global Account](img_2_4.png)

5. Under **Spaces**, choose the available space as highlighted below.

    !![Enter Space](img_2_5.png)

    >**Organization:** Organizations in Cloud Foundry enable collaboration among users and grouping of resources.

    >**Space:** Cloud Foundry has a standard working environment for individual applications: it is called a space. Spaces are individual working areas, which normally contain a single application.

6. In the left pane, choose **Services** > **Service Marketplace**.

    !![Enter Service Marketplace](img_2_6.png)

    >The **Service Marketplace** is where you can find services to attach to any of your applications. These services are provided by SAP BTP to create and produce applications quickly and easily.

7. Search for **Mobile**, and click on the **Mobile Services** tile.  

    !![Mobile Service](img_2_7.png)

8. Choose **Support** to open **SAP Mobile Services Cockpit**.

    !![Mobile Service Detailed View](img_2_8.png)

9. Provide your Cloud Platform *credentials* and Click **Next**.

    !![CPMS Login](img_2_9.png)

10. Choose the **Organization** and **Space** from the dropdown list, and then select **Open**.

    !![Select Org & Space for CPMS Cockpit](img_2_10.png)

    > Since you will access the mobile services cockpit throughout the mission, Bookmark the **Mobile Services cockpit URL** for quick access.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Get familiar with available configurations)]

1. Click **SAP Mobile Cards** in the side bar menu to open the Mobile Cards configuration.

    !![Mobile Cards Option](img_3_1.png)

    >If the SAP Mobile Cards Advisory screen pops up, choose **Close** to close it.

2. Click **APIs** tab.

    !![APIs Tab](img_3_2.png)

    > If you can't see the APIs tab, click **Initialize**. This will create a new service instance for Mobile Cards.

    > !![MobileCards](img_3_2_note.png)

In the next step, you will scan the respective QR code to connect your mobile application to SAP BTP Mobile Service.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Configure SAP Mobile Cards client)]

>Make sure you are choosing the right device platform tab ( **Android** or **iOS** ) **above**.

[OPTION BEGIN [Android]]

1. Launch Mobile Cards Application on your device.

    !![App Launch](img_4_and_1.png)

    > If you don't have the application installed your device, complete all the prerequisites of this tutorial.

2. Tap **Proceed** in the Welcome pop-up.

    !![Welcome Pop-up](img_4_and_2.png)

3. Tap **I AGREE** after reading the *End User License Agreement*.

    !![EULA Screen](img_4_and_3.png)

4. Tap **Scan QR CODE**.

    !![Home Screen](img_4_and_4.png)

5. Scan the Android QR Code present in the APIs tab of the Mobile Services cockpit.

    !![QR Code](img_4_and_5.png)

6. Enter your **SAP BTP login credentials** and tap **Log On**.

    !![SAP BTP Login screen](img_4_and_6.png)

7. Specify an app **Passcode** matching the criteria, and tap **NEXT**.

    !![Passcode Screen](img_4_and_7.png)

8. Re-enter the **Passcode** to verify, and tap **DONE**.

    !![Passcode Verify Screen](img_4_and_8.png)

9. Place your finger on the fingerprint scanner to enable biometric authentication.

    !![Biometric Screen](img_4_and_9.png)

    > This option is available only on devices that support biometric authentication. It allows you to use your biometric information, rather than the app passcode defined earlier.

10. Tap **Allow only while using the app** option for the location services request.

    !![Location Services Screen](img_4_and_10.png)

    > SAP Mobile Cards is capable of showing some cards based on your location. [Click here](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-android.html#location-specific-notifications) to read more about it.

    > This option is shown when you launch the application and log in to a server for the first time.

[OPTION END]

[OPTION BEGIN [iOS]]

1. Launch the **QR Code Scanner** from the Control Center on your iOS device.

    !![Control Center](img_4_ios_1.png)

2. Scan the iOS QR Code present in the APIs tab of the Mobile Services cockpit.

    !![QR Code](img_4_ios_2.png)

    > If the SAP Mobile Cards application doesn't open, you may not have the application installed on your mobile device. To install the application complete all the prerequisites of this tutorial.

3. Enter your **SAP BTP login credentials** and tap **Log On**.

    !![SAP BTP Login screen](img_4_ios_3.png)

4. Define a passcode to unlock the app and tap **Done**.

    !![Passcode Screen](img_4_ios_4.png)

5. Tap **Enable** to enable biometric authentication.

    !![Biometric Screen](img_4_ios_5.png)

    > This biometric option may be Face ID depending on the capabilities of your device. This feature allows you to use your biometric information, rather than the app passcode defined earlier.

6. Tap **Allow** to enable notification functionality for your application.

    !![Notification Request Screen](img_4_ios_6.png)

    > This option is shown when you launch the application and log in to a server for the first time.

[OPTION END]

You've now connected your SAP Mobile Cards client to your SAP BTP account.

You can automate the set up covered in this tutorial by using SAP BTP Boosters. You can find the details in the [starter mission of SAP Mobile Cards](cp-mobile-cards-welcome).

You can now proceed to the [next tutorial](cp-mobile-bas-setup), which will guide you to set up your Business Application Studio to build SAP Mobile Cards.

[DONE]
[ACCORDION-END]

---
