---
title: Create Xcode Project with SAP Cloud Platform SDK for iOS
description: Enable SAP Cloud Platform Mobile Services, create a connection in the SAP Cloud Platform SDK for iOS Assistant, and generate a master-detail view app with the iOS Assistant.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, topic>odata, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios ]
time: 15
---

## Prerequisites  
- **Tutorials:** [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account) and [Enable SAP Cloud Platform Mobile Services](fiori-ios-hcpms-setup)
- **Development environment:** Apple Mac running macOS High Sierra or higher with Xcode 10 or higher
- **SAP Cloud Platform SDK for iOS:** Version 3.0 SP02

## Details
### You will learn  
  - How to create a connection in the SAP Cloud Platform SDK for iOS Assistant
  - How to generate an app with the SAP Cloud Platform SDK for iOS Assistant

Before you start, make sure you:

  - Have downloaded [SAP Cloud Platform SDK for iOS](https://www.sap.com/developer/trials-downloads/additional-downloads/sap-cloud-platform-sdk-for-ios-14485.html) **version 3.0 SP02**.
  - Have a trial account on SAP Cloud Platform. See [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).
  - Enabled the SAP Cloud Platform Mobile Services. See [Enable SAP Cloud Platform Mobile Services](fiori-ios-hcpms-setup).

---

[ACCORDION-BEGIN [Step 1: ](Enable SAP Cloud Platform Mobile Services)]

The SAP Cloud Platform SDK for iOS is designed to work seamlessly with a set of services provided by the SAP Cloud Platform that are optimized for communication with mobile devices, known collectively as Mobile Services. These include not only data services, but also features like analytics, push notifications, and app configuration. Before creating your first app, you'll need to ensure that Mobile Services are enabled for your trial account.

To enable the service, do the tutorial [Enable SAP Cloud Platform Mobile Services](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html).

>Alternatively, you can go directly to: `https://hcpmsadmin-<your_user_id>trial.dispatcher.hanatrial.ondemand.com/`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up Mobile Services account in iOS Assistant)]

The SAP Cloud Platform SDK for iOS includes an Assistant app for generating and managing iOS apps that use the Mobile Services. To get started with the Assistant, you'll need to configure it for your account. Part of this configuration can be imported automatically.

Once you're logged into **SAP Cloud Platform Mobile Services**, click the **Important Links** tab in the lower-left corner. The **Important Links** section opens.

> When you open up Mobile Services for the first time, you will see a popup appear which contains a couple of QR codes. Please go ahead and close that.

Locate the tile **SAP Cloud Platform SDK Tools** and click the **Importing URLs directly** link.

![Setup Assistant](fiori-ios-scpms-teched19-00.png)

You should now see the following pop-up:

![Setup Assistant](fiori-ios-scpms-teched19-00a.png)

Click **Allow**. The SAP Cloud Platform SDK for iOS Assistant application will open. The **Add Account** settings dialog will open, and both **API URL** and **UI URL** parameters are pre-populated automatically:

![ Setup Assistant](fiori-ios-scpms-teched19-01.png)

Provide the following additional details:

| Field | Value |
|----|----|
| **Name** | A descriptive name for the configuration, for instance `SAP Cloud Platform Mobile Services` |
| **Authentication** | `SAML Authentication` |
| **User** | Your trial account user |
| **Password** | Password for your trial account user |

![Setup Assistant](fiori-ios-scpms-teched19-02.png)

Click **Save** when finished. The account is now added to the SDK Assistant:

![Setup Assistant](fiori-ios-scpms-teched19-03.png)

Click **Back** to return to the main screen for the SAP Cloud Platform SDK for iOS Assistant.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create iOS app using iOS Assistant)]
 
With your account created, you can now use the Assistant to create cloud application configurations on the Mobile Services back end, create destinations pointing to data service endpoints, and generate Xcode projects with a built-in data service layer to access the backend via Swift.

For the remainder of this tutorial, you will use the Assistant to generate an Xcode project that accesses a sample backend hosted on the SAP Cloud Platform, then replace the generated user interface with your own, and finally add the code necessary to integrate a Core ML machine learning model to classify product images.

Let's get started by creating a cloud application definition on Mobile Services connecting to a service named `com.sap.edm.sampleservice.v2`. This service is provided as a sample service on SAP Cloud Platform.

Open up the **SAP Cloud Platform SDK for iOS Assistant** and click **Create new**.

![Project creation](fiori-ios-scpms-teched19-04.png)

Next, click **Create New Application** to start the workflow. Choosing this workflow gives you the ability to customize many aspects of the back end connection and select various features for the generated Xcode project.

![Project creation](fiori-ios-scpms-teched19-04a.png)

In step 1 of this workflow, you should see listed the Mobile Services account you created earlier in this tutorial. The account should be pre-selected so you can simply click **Next** to continue.

![Project creation](fiori-ios-scpms-teched19-05.png)

In step 2 labeled **Cloud Application**, you will create the Mobile Services application configuration for your iOS app. This configuration will contain a destination and some sort of reverse proxy to connect to the sample service backend.

Enter these values for the following fields:

| Field | Value |
|----|----|
| **Name** | `SalesAssistant` |
| **Identifier** | `com.example` |
| **Authentication** | `OAuth2` |

If you are using a trial account to access Mobile Services, be sure that you haven't exceeded the 5 application limit before proceeding. If you already have 5 applications you have to delete one of them to be able to create a new one.

If you're using a new trail account or if you are sure that you have a free spot for a Mobile Service application, click **Next** to continue.

![Project creation](fiori-ios-scpms-teched19-06.png)

With the cloud application definition set in step 2 of the workflow, it's time to define the service destination the application should reference. In this tutorial series we're using a sample built into every Mobile Services account.

Select the service titled `com.sap.edm.sampleservice.v2` from the list.

![Project creation](fiori-ios-scpms-teched19-07.png)

Click **Next**.

![Project creation](fiori-ios-scpms-teched19-08.png)

The Features step allows you to select which features of the SDK you want to use. Selecting a feature will setup the corresponding configurations on the Mobile Services backend.

Accept the default settings and click **Next**.

![Project creation](fiori-ios-scpms-teched19-09.png)

For step 5, labeled **Xcode Project**, enter these values for the following fields:

| Field | Value |
|----|----|
| **Product Name** | `SalesAssistant` |
| **Organization Name** | Your organization name |
| **Organization Identifier** | `com.teched.appspace` |

> For your real apps, please select the organization name and identifier wisely. This will be your bundle identifier, which will be used to identify your app. No worries, you can change the bundle identifier in the Xcode project.

Click **Next**.

![Project creation](fiori-ios-scpms-teched19-10.png)

For step 5, labeled Proxy Classes, you should see the sample service name `com.sap.edm.sampleservice.v2` that we selected in step 3 of the workflow.

Click **Next**.

![Project creation](fiori-ios-scpms-teched19-11.png)

The last and final step is UI configuration. Here you can choose from a number of iOS app types the Assistant can generate. For this tutorial, we want a Master / Detail UI that includes the on-boarding flow, so make sure the **Master / Detail Screens with Onboarding for destination:** radio button is selected.

Click **Finish**.

![Project creation](fiori-ios-scpms-teched19-12.png)

The SAP Cloud Platform SDK for iOS Assistant will now create the cloud application on Mobile Services, pull the metadata document of the OData service and generate an Xcode project, including the on-boarding flow and all the necessary proxy classes and a convenience data service API.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run iOS app on simulator)]

After clicking **Finish**, the Assistant will generate an Xcode project, save it to disk, and open the project in Xcode.

To compile and open your app in the iOS Simulator, click the run button in the top-left corner of the Xcode toolbar or choose **Run** from the Xcode **Project** menu.

![App Onboarding](fiori-ios-scpms-teched19-13.png)

The iOS Simulator should launch, initialize iOS, then run your app. The first screen you'll see is the start of the generated on-boarding flow. It's important to know that the generated on-boarding flow is simply the default experience and can be easily modified to fit your requirements.

Click **Start**.

> If you're interested in the different on-boarding variants you can take a look at the design guidelines: [Fiori for iOS Design Guidelines](https://experience.sap.com/fiori-design-ios/article/onboarding/).

![App Onboarding](fiori-ios-scpms-teched19-14.png)

Next your app will connect to the SAP identity provider triggering an authentication prompt for your SAP Cloud Platform account id and password.

Please enter the needed information and click **Log on**.

> You could also define your own IDP on the cloud side if you don't want to use the SAP IDP.

![App Onboarding](fiori-ios-scpms-teched19-15.png)

The next screen shows an example Data Privacy disclosure prompt which allows you to include data privacy and consent text that complies with the EU General Data Protection Regulation (GDPR) for example.

Click **Allow** to continue with the flow. Clicking **Deny** will cause the app to go back to the initial screen of the on-boarding flow.

![App Onboarding](fiori-ios-scpms-teched19-16.png)

Next, you'll be present the passcode screen. This screen is optional when building your own app however, requiring a passcode can provide an additional layer of security for accessing sensitive data like customer information, sales numbers, etc.

Enter a passcode containing 8 characters. For this tutorial series it's sufficient to just enter a passcode of `12345678`.

Passcode policies are managed via Mobile Services and can contain configurations that define the password complexity, how many failed attempts are allowed before the app locks, etc.

Before you click **Next**, select on `FaceID > Enrolled from the Hardware menu in the iOS Simulator`.
This will configure the simulator to automatically use `FaceID` or `TouchID` to unlock the app instead of asking you for a passcode every time it's run.

After you've enrolled in `TouchID` or `FaceID` (depending on the simulator you're using) , click **Next**.

![App Onboarding](fiori-ios-scpms-teched19-17.png)

 Re-enter your passcode and click **Done**.

![App Onboarding](fiori-ios-scpms-teched19-18.png)

Now that you've enrolled in `TouchID` or `FaceID`, a screen will be added to the on-boarding flow asking you to authorize the app to use Apple's native security feature.

Click **Enable** to continue.

![App Onboarding](fiori-ios-scpms-teched19-19.png)

Congratulations, you've successfully on-boarded into your app! You should see a table view containing all the entities of our sample OData service allowing you to edit existing records, insert your own, or delete records altogether.

Click on an entity to see its records.

![App Onboarding](fiori-ios-scpms-teched19-20.png)

[VALIDATE_4]
[ACCORDION-END]
