---
title: Enable SAP Cloud Platform Mobile Services
description: Learn how to enable SAP Cloud Platform Mobile Services on your SAP Cloud Platform Trial Account.
auto_validation: true
primary_tag: products>sap-cloud-platform-sdk-for-ios
tags: [  tutorial>beginner, operating-system>ios, topic>mobile, topic>odata, products>sap-cloud-platform, products>sap-cloud-platform-sdk-for-ios  ]
time: 15
---

## Prerequisites  
- **Tutorials:** [Get a Free Trial Account on SAP Cloud Platform](https://developers.sap.com/tutorials/hcp-create-trial-account.html) and [Enable SAP Cloud Platform Mobile Services](https://developers.sap.com/tutorials/fiori-ios-hcpms-setup.html)
- **Development environment:** Apple Mac running macOS High Sierra or higher with Xcode 10 or higher
- **SAP Cloud Platform SDK for iOS:** Version 3.0

## Details
### You will learn  
  - How to enable SAP Cloud Platform Mobile Services and create a connection in the SAP Cloud Platform SDK for iOS Assistant.

Before you start, make sure you:

  - Have downloaded [SAP Cloud Platform SDK for iOS](https://www.sap.com/developer/trials-downloads/additional-downloads/sap-cloud-platform-sdk-for-ios-14485.html) **version 3.0**.
  - Have a trial account on SAP Cloud Platform. See [Get a Free Trial Account on SAP Cloud Platform](https://developers.sap.com/tutorials/hcp-create-trial-account.html).

---

[ACCORDION-BEGIN [Step 1: ](Enable SAP Cloud Platform Mobile Services)]

The SAP Cloud Platform SDK for iOS is designed to work seamlessly with a set of services provided by the SAP Cloud Platform that are optimized for communication with mobile devices, known collectively as "Mobile Services". These include not only data services, but also features like analytics, push notifications, and app configuration. Before creating your first app, you'll need to ensure that Mobile Services are enabled for your trial account.

> If you have already configured the SAP Cloud Platform SDK for iOS Assistant, you can **skip this step** and proceed with the "Create Your First Fiori for iOS App" tutorial.

Log on to your SAP Cloud Platform trial account at [https://account.hanatrial.ondemand.com/](https://account.hanatrial.ondemand.com/) and once logged in choose **Neo Trial** as your target landscape. Click on **Services** on the left side to see all available Services. Scroll down to **Mobile Services, std**, click on the tile. After the Service page loaded click the **Enable** button to enable this service. After the Service is enabled click the **Go to Service** link to open a new window to **SAP Cloud Platform Mobile Services**.

> Alternatively, you can go directly to `https://hcpmsadmin-<your_user_id>trial.dispatcher.hanatrial.ondemand.com/`

![SCPms landing page](fiori-ios-scpms-create-app-teched18-part1-01.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Setup your Mobile Services account in the iOS Assistant)]

The SAP Cloud Platform SDK for iOS includes an Assistant app for generating and managing iOS apps that use the Mobile Services. To get started with the Assistant, you'll need to configure it for your account. Part of this configuration can be imported automatically.

Once you're logged in to **SAP Cloud Platform Mobile Services**, click the **Important Links** tab in the lower left bottom. The **Important Links** section opens.

Locate the tile **SAP Cloud Platform SDK for iOS Assistant** and click the **Importing URLs directly into Assistant** link:

![Important Links](fiori-ios-scpms-create-app-teched18-part1-02.png)

You should now see the following pop-up:

![Import URLs](fiori-ios-scpms-create-app-teched18-part1-03.png)

Click **Allow**. The SAP Cloud Platform SDK for iOS Assistant application will start. The **Add Mobile Services Account** settings dialog will open, and both **API URL** and **UI URL** parameters are pre-populated automatically:

![Import URLs](fiori-ios-scpms-create-app-teched18-part1-04.png)

Provide the following additional details:

| Field | Value |
|----|----|
| Name | A descriptive name for the configuration, for instance `SAP Cloud Platform Mobile Services` |
| Authentication Type | `Basic Authentication` |
| User | Your trial account user |
| Password | Password for your trial account user |

![Import URLs](fiori-ios-scpms-create-app-teched18-part1-05.png)

Click **Save** when finished. The account is now added to the SDK Assistant:

![Import URLs](fiori-ios-scpms-create-app-teched18-part1-06.png)

Click **Back** to return to the main screen for the **SAP Cloud Platform SDK for iOS Assistant**.

[VALIDATE_1]
[ACCORDION-END]
