---
title: Enable SAP Cloud Platform Mobile Services
description: Learn how to enable SAP Cloud Platform Mobile Services within a SAP Cloud Platform trial account and how to open the Mobile Services cockpit.
auto_validation: true
primary_tag: software-product-function>sap-cloud-platform-mobile-services
tags: [  tutorial>beginner, topic>mobile, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment, software-product-function>sap-cloud-platform-mobile-services, products>sap-cloud-platform-sdk-for-ios, products>sap-cloud-platform-sdk-for-android, products>sap-mobile-cards, products>mobile-development-kit-client]
time: 5
author_name: Sandeep TDS
author_profile: https://github.com/sandeep-tds
---
## Prerequisites  
 - **Tutorial:** If you're new to SAP Cloud Platform, follow the tutorial [View the SAP Cloud Platform from 10,000 Meters](cp-explore-cloud-platform).
 - **Tutorial:** If you don't have an SAP Cloud Platform account, follow the tutorial [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).

## Details
### You will learn  
- How to enable SAP Cloud Platform Mobile Services in your Cloud Foundry or Neo trial account

---

>**This tutorial has been executed with Mobile Services in SAP Cloud Platform Cloud Foundry and Neo environment, please switch to either tab according to your environment.**

Once SAP Cloud Platform Mobile Services is available, you can use its features in your Mobile development kit, Mobile Cards, SAP Cloud Platform SDK for iOS & Android apps.

[ACCORDION-BEGIN [Step 1: ](Open the SAP Cloud Platform cockpit)]

>Make sure you are choosing the right environment platform tab above.

Go to your [SAP Cloud Platform cockpit landing page](https://cockpit.hanatrial.ondemand.com). Click on the **Enter Your Trial Account** to see your global account.

![enter trial account](enter-trial.png)

>If this is your first time accessing your trial account, you'll have to configure your account by choosing a region (select the region closest to you). Your user profile will be set up for you automatically.  

>Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

>Choose **Continue**.

>![Account setup](cp-initial-setup.png)

The global trial account contains **one** subaccount and space. Navigate to subaccount by clicking on the tile named **trial** (this name may vary if you created the subaccount manually).

![enter subaccount](global-account.png)

To get to the space, in which your applications and services live, click on the **dev** space.

![enter space](sub-account.png)

In the left pane, choose **Services** > **Service Marketplace**.

>The **Service Marketplace** is where you can find services to attach to any of your applications. These services are provided by SAP Cloud Platform to create, and produce applications quickly and easily. Once a service has been created, it is known as a `service instance`.

![service marketplace](img_5.png)

Search for **Mobile**, and click on the **SAP Mobile Services** tile.  

![mobile service tile](img_6.png)

Choose **Support** to open **SAP Cloud Platform Mobile Services Cockpit**.

![support button click](img_7.png)

>**Organization:** Organizations in CF enable collaboration among users and enable grouping of resources.

>**Space:** Cloud Foundry has a standard working environment for individual applications: it is called a space. Spaces are individual working areas, which normally contain a single application.

Choose the relevant **Organization** and **Space** from the dropdown list, and then select **Open**.

![cf Trial](img_8.png)

You have now logged in to the SAP Mobile Services cockpit.

![cf Trial](img_9.png)

Bookmark the **Mobile Services cockpit URL** for quick access.

[VALIDATE_3]
[ACCORDION-END]
