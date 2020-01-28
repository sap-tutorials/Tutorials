---
title: Enable SAP Cloud Platform Mobile Services
description: Learn how to enable SAP Cloud Platform Mobile Services within a SAP Cloud Platform trial account and how to open the Mobile Services cockpit.
auto_validation: true
primary_tag: software-product-function>sap-cloud-platform-mobile-services
tags: [  tutorial>beginner, topic>mobile, operating-system>ios, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment, software-product-function>sap-cloud-platform-mobile-services, products>sap-cloud-platform-sdk-for-ios, products>sap-cloud-platform-sdk-for-android, products>sap-mobile-cards, products>mobile-development-kit-client]
time: 5
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---
## Prerequisites  
 - **Tutorial:** If you're new to SAP Cloud Platform, follow the tutorial [View the SAP Cloud Platform from 10,000 Meters](cp-explore-cloud-platform).
 - **Tutorial:** If you don't have an SAP Cloud Platform account, follow the tutorial [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).

## Details
### You will learn  
- How to enable SAP Cloud Platform Mobile Services in your Cloud Foundry or Neo trial account

---

>**This tutorial has been executed with Mobile Services in SAP Cloud Platform Cloud Foundry and Neo environment, please switch to either tab according to your environment.**

Once SAP Cloud Platform Mobile Services is available, you can use its features in your SAP Cloud Platform SDK for iOS, Android, Mobile development kit and Mobile Cards apps.

[ACCORDION-BEGIN [Step 1: ](Open the SAP Cloud Platform cockpit)]

>Make sure you are choosing the right environment platform tab above.

[OPTION BEGIN [Cloud Foundry]]

Go to your [SAP Cloud Platform cockpit landing page](https://cockpit.hanatrial.ondemand.com). Click on the **Enter Your Trial Account** to see your global account.

![enter trial account](enter-trial.png)

The global trial account contains **one** subaccount and space. Navigate to subaccount by clicking on the tile named **trial** (this name may vary if you created the subaccount manually).

![enter subaccount](global-account.png)

To get to the space, in which your applications and services live, click on the **dev** space.

![enter space](sub-account.png)

In the left pane, choose **Services** > **Service Marketplace**.

>The **Service Marketplace** is where you can find services to attach to any of your applications. These services are provided by SAP Cloud Platform to create, and produce applications quickly and easily. Once a service has been created, it is known as a `service instance`.

![cf Trial](img_5.png)

Search the **Mobile Services** and click on it.

![cf Trial](img_6.png)

Choose **Support** to open **SAP Cloud Platform Mobile Services Cockpit**.

![cf Trial](img_7.png)

>**Organization:** Organizations in CF enable collaboration among users and enable grouping of resources.

>**Space:** Cloud Foundry has a standard working environment for individual applications: it is called a space. Spaces are individual working areas, which normally contain a single application.

Choose the relevant **Organization** and **Space** from the dropdown list, and then select **Open**.

![cf Trial](img_8.png)

You have now logged in to the SAP Mobile Services cockpit.

![cf Trial](img_9.png)

Bookmark the **Mobile Services cockpit URL** for quick access.

[OPTION END]

[OPTION BEGIN [Neo]]

Go to your [SAP Cloud Platform cockpit landing page](https://cockpit.hanatrial.ondemand.com). Scroll down and click **Access Neo Trial**.

![Neo Trial](neo-trial.png)

Click **Services** in the navigation bar, scroll down until you see the Mobile group then click on the **Mobile Services** tile.

![Mobile Group](mobile-group.png)

If **Mobile Services** is not enabled, then enable it.

Click on **Go to Service** to open the Mobile Services cockpit.

![Go to Service](go-to-service.png)

![Mobile Services Cockpit](management-cockpit.png)

Bookmark the **Mobile Services cockpit URL** for quick access.

[OPTION END]

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Optional: Setup a Connectivity between SAP Web IDE and Mobile Services)]

>Make sure you are choosing the right environment platform tab above.

[OPTION BEGIN [Cloud Foundry]]

If you want to use **SAP Web IDE Full-Stack** service to build **Mobile Cards** or **Mobile development kit** based apps connecting to Mobile Services running on Cloud Foundry environment, then you need to follow below step.

>Since SAP Web IDE Full-Stack is currently available only in SAP Cloud Platform Neo environment, in order to connect to SAP Cloud Platform Mobile Services on Cloud Foundry environment, you need to create a new destination.

On [SAP Cloud Platform cockpit landing page](https://cockpit.hanatrial.ondemand.com) scroll down and click **Access Neo Trial**.

![CF Trial](neo-trial.png)

Click **Destinations** on the left panel.

![CF Trial](destination.png)

Click **New Destination** to open a new destination configuration form and provide below information:

| Field | Value |
|----|----|
Name           | `mobileservices_cf`
Type           | `HTTP`
Description    | `Mobile Services CF`
URL            | please check note below
Proxy Type     | `Internet`
Authentication | `BasicAuthentication`
User | `SCP user id`
password | `SCP password`

>For URL field, in Mobile Services Cockpit, click **Important links** on bottom-left side and copy from the section of SAP Cloud Platform SDK Tools the link of Copy `AdminAPI` to your destination.
>![CF Trial](img_1.1.png)

Provide below information for **Additional Properties**:

| Field | Value |
|----|----|
`HandleRedirects`  | `false`
`SkipSSOTokenGenerationWhenNoUser`  | `true`
`WebIDEEnabled`  | `true`
`WebIDEUsage`  | `mobile`

>First two fields need to type manually.

Click **Save** and then click **Check Connection** on the newly created Destination to see if everything works.

![CF Trial](check_ms-cf-dest.png)

[OPTION END]

[OPTION BEGIN [Neo]]

Once **Mobile Services** is enabled, there is a destination named `mobileservices` gets created in **Connectivity** under Neo environment.

![Mobile Group](img_10.png)

[OPTION END]

[DONE]
[ACCORDION-END]
