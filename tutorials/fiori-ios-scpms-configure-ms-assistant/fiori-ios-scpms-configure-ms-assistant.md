---
title: Configure Mobile Services in iOS Assistant
description: Enable SAP Cloud Platform Mobile Services and create a connection in the SAP Cloud Platform SDK for iOS Assistant.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, topic>odata, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios, software-product-function>sap-cloud-platform-mobile-services]
time: 15
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---

## Prerequisites
- Completed the previous tutorial(s) in this group.
- **Development environment:** Apple Mac running macOS Catalina or higher with Xcode 11 or higher
- **SAP Cloud Platform SDK for iOS:** Have downloaded Version 5.0 or higher from [Trials and Downloads](https://developers.sap.com/trials-downloads.html?search=sdk%20for%20ios)  

## Details
### You will learn  
  - How to create a connection in the SAP Cloud Platform SDK for iOS Assistant

---

[ACCORDION-BEGIN [Step 1: ](Enable SAP Cloud Platform Mobile Services)]

The SAP Cloud Platform SDK for iOS is designed to work seamlessly with a set of services provided by the SAP Cloud Platform that are optimized for communication with mobile devices, known collectively as Mobile Services. These include not only data services, but also features like analytics, push notifications, and app configuration. Before creating your first app, you'll need to ensure that Mobile Services are enabled for your trial account.

> If you have already configured the SAP Cloud Platform SDK for iOS Assistant, you can **skip this step** and proceed with the "Create Your First Fiori for iOS App" tutorial.

To enable the service, do the tutorial [Enable SAP Cloud Platform Mobile Services](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up your Mobile Services account in the iOS Assistant)]

The SAP Cloud Platform SDK for iOS includes an Assistant app for generating and managing iOS apps that use the Mobile Services. To get started with the Assistant, you'll need to configure it for your account. Part of this configuration can be imported automatically.

Once you're logged in to **SAP Cloud Platform Mobile Services**, click the **Important Links** tab in the lower left bottom. The **Important Links** section opens.

Locate the tile **SAP Cloud Platform SDK for iOS Assistant** and click the **Importing URLs directly** link:

![Important Links](fiori-ios-scpms-configure-ms-assistant-01.png)

> NOTE: Please make sure that the SAP Cloud Platform SDK for iOS Assistant is on the main window.

You should now see the following pop-up:

![Import URLs](fiori-ios-scpms-configure-ms-assistant-02.png)

Click **Allow**. The SAP Cloud Platform SDK for iOS Assistant application will start. The **Add Mobile Services Account** settings dialog will open, and both **API URL** and **UI URL** parameters are pre-populated automatically:

![Import URLs](fiori-ios-scpms-configure-ms-assistant-03.png)

Provide the following additional details:

| Field | Value |
|----|----|
| Name | A descriptive name for the configuration, for instance `SAP Cloud Platform Mobile Services` |
| Authentication Type | `Basic Authentication` |
| User | Your trial account user |
| Password | Password for your trial account user |

Click **Save** when finished. The account is now added to the SDK Assistant:

![Import URLs](fiori-ios-scpms-configure-ms-assistant-04.png)

Click **Back** to return to the main screen for the **SAP Cloud Platform SDK for iOS Assistant**.

[VALIDATE_1]
[ACCORDION-END]
