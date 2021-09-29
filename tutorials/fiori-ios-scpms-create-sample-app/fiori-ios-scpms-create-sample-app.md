---
title: Create a Sample iOS App
description: Create and examine your first Fiori for iOS app using the SAP BTP SDK Assistant for iOS connecting against a sample service.
auto_validation: true
primary_tag: products>ios-sdk-for-sap-btp
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, topic>odata, products>sap-business-technology-platform, products>sap-mobile-services ]
time: 15
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---

## Prerequisites  
- Completed the previous tutorial(s) in this group.
- **Development environment:** Apple Mac running macOS Catalina or higher with Xcode 11 or higher
- **SAP BTP SDK for iOS:** Have downloaded Version 5.1.0 or higher from [Trials and Downloads](https://developers.sap.com/trials-downloads.html?search=sdk%20for%20ios)  

## Details
### You will learn  
  - How to create a Fiori for iOS app
  - What the SAP BTP SDK Assistant for iOS can generate.

---

[ACCORDION-BEGIN [Step 1: ](Use SAP Mobile Services to create an Mobile Application configuration)]

Using SAP Mobile Services (Mobile Services) allows you to manage your mobile apps. It also enables you to create mobile application configurations, not the real iOS app, to be later being consumed by the SAP BTP SDK Assistant for iOS (Assistant).

The Assistant will let you then consume that configuration and generate an Xcode project out of it, containing the data model classes and convenience data service. More in detail at a later point in the tutorial.

Open up your Mobile Services instance which should be running as service instance on your Cloud Foundry landscape.

Click on **Mobile Applications** to expand that navigation node.

![Mobile Services](fiori-ios-scpms-create-sample-app-01.png)

Click on **Native/Hybrid** to be able to create a new mobile application. Within the new screen on the right-hand side, you should see a button **New**. Click on it to start the creation flow.

![Mobile Services](fiori-ios-scpms-create-sample-app-02.png)

In the upcoming pop-up, fill in the fields as seen in the following table and click on **Save**:

| Field        | Value           |
| ------------- | ------------- |
| ID      |  `com.example.tutorialapp`
| Name      | `TutorialApp`      |
| Description | Describe the usage of that app configuration |

![Mobile Services](fiori-ios-scpms-create-sample-app-03.png)

In the next window, you can select the features for you application. Some features are pre-selected for you.

Select **Mobile Sample OData ESPM** and click Finish.

![Feature Selection Window](fiori-ios-scpms-create-sample-app-04.png)

> For most of the tutorials, you will use this cloud app configuration which is based on the **Mobile Sample OData ESPM** service. This service provides you with an OData backend containing a product, supplier and customer catalog.

In the confirmation dialog that appears, click **OK**.

![Mobile Services](fiori-ios-scpms-create-sample-app-05.png)

The mobile app configuration is now created.

![Mobile Services](fiori-ios-scpms-create-sample-app-06.png)

Your work is done in Mobile Services now. The following steps will describe how to use the Assistant to take that cloud app configuration to generate your first app.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Use the SAP BTP SDK Assistant for iOS to create your first Xcode iOS project)]

On your MacBook, open the Assistant and click **Create New** to start the creation workflow.

![Assistant](fiori-ios-scpms-create-sample-app-07.png)

The initial step of the workflow asks you to choose whether to create a sample app, use an existing cloud app configuration, or create a new one.

![Assistant](fiori-ios-scpms-create-sample-app-08.png)

Click the **Reuse Existing Application** tile in the middle of the **Create a new App** step. This will allow us to use the previously defined cloud app configuration.

The first step in the **Reuse Existing Application** workflow wants you to select the Mobile Services account you want to use. You could have multiple accounts here for different landscapes you might use.

Select your account and click on **Next**.

![Assistant](fiori-ios-scpms-create-sample-app-09.png)

> If you selected Authentication Type as Single Sign-On while configuring your account, you will be asked to enter your username and password.
  ![Assistant](fiori-ios-scpms-create-sample-app-sso.png)

Step 2 lets you select the **Cloud Application** you want to base your Xcode iOS project on.

Select the `com.example.tutorialapp` and click on **Next**.

![Assistant](fiori-ios-scpms-create-sample-app-10.png)

If you look closely at the UI you can see a clear distinction between the **Cloud Configuration** and the **Client Configuration**. So right now you're transitioning into the definition of your Xcode project and the way the Assistant generates your app.

Fill in the fields as seen in the following table:

| Field        | Value           |
| ------------- | ------------- |
| Product Name      |  `TutorialApp`
| Organization Name      | Fill in your org name |
| Organization Identifier | `com.sap.example` |

> **Note:** The name of the cloud application does not need to match your iOS app name, and often won't; your iOS app may access multiple cloud applications. The Application Identifier, however, must be unique across all of your configured cloud applications, and typically follows a reverse-DNS format.

Choose a path where to save the project to and click **Next** to advance to the **Proxy Classes** step.

![Assistant](fiori-ios-scpms-create-sample-app-11.png)

Here you can see what destination is going to be used to pull the Metadata of the OData Service to generate the OData Proxy Classes. If you hover over the displayed service identifier you can make changes to the way the Assistant generates these classes. You won't touch this feature in most of the tutorials.

![Assistant](fiori-ios-scpms-create-sample-app-12.png)

Click **Next** to go to the **UI Configuration** step.

The UI Configuration step will let you define what the Assistant should generate on top of the Proxy Classes and convenience data service. For most tutorials, you will replace the generated UI with your own but to get an understanding of the Assistant's power, make sure all the checkboxes are selected and click on **Finish**.

![Assistant](fiori-ios-scpms-create-sample-app-13.png)

The Assistant will now connect to Mobile Services and fetches the defined configuration and the Metadata document of the sample service. With that information, it will create an Xcode project for you including the selected UI, Proxy Classes which will represent the entities in the service, as well as a convenience data service for easy backend communication.

Your Xcode project should automatically open up and you can run the app.

![Assistant](fiori-ios-scpms-create-sample-app-14.png)

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the App Created by Assistant on an iOS Simulator )]

In the Xcode Tool Bar, click **Set the active scheme**.

![Assistant](fiori-ios-scpms-create-sample-app-15.png)

Select a simulator of your choice.

![Assistant](fiori-ios-scpms-create-sample-app-16.png)

> You can run the application on your physical device too.

In the Xcode Tool Bar, Click **Run**.

![Assistant](fiori-ios-scpms-create-sample-app-17.png)

Upon successful build, the simulator will be launched and the application will be started.

![Assistant](fiori-ios-scpms-create-sample-app-18.png)

Click **Start** and enter your SAP BTP credentials to login.

![Assistant](fiori-ios-scpms-create-sample-app-19.gif)

Finish the setup of your application.

![Assistant](fiori-ios-scpms-create-sample-app-20.gif)

Explore the application created.

![Assistant](fiori-ios-scpms-create-sample-app-21.gif)

> Since we used the *Mobile Sample OData ESPM* service. The assistant builds an application using the data returned by this service.

You have successfully built an iOS application using the Assistant that seamlessly connects to SAP Mobile Services.

[DONE]
[ACCORDION-END]
