---
title: Configure Initial Setup for SAP Mobile Cards
description: Define a destination in SAP Cloud Platform Mobile Services cockpit to establish connectivity to a SAP SuccessFactors sandbox API.
auto_validation: true
primary_tag: products>sap-mobile-cards
tags: [ tutorial>beginner, operating-system>ios, operating-system>android, topic>mobile, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services ]
time: 10
author_name: Jitendra Kansal
author_profile: https://github.com/jitendrakansal
---

## Details
### You will learn
  - How to access the SAP Cloud Platform Mobile Services cockpit
  - How to establish connectivity to a SAP SuccessFactors API

In this tutorial, you will create a destination to connect to the SAP SuccessFactors API data source from SAP Cloud Platform Mobile Services.


---

[ACCORDION-BEGIN [Step 1: ](Understand SAP Mobile Cards)]

SAP Mobile Cards is a feature within SAP Cloud Platform Mobile Services which provides our customers access to a micro-application platform to publish data into a consumer-grade wallet or passbook-style app. It allows companies to quickly create simple, yet highly valuable quick-win apps.

You can find more details on the [SAP Mobile Cards developer page](https://developers.sap.com/topics/mobile-cards.html).

![MobileCards](MobileCards.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Go to SAP Mobile Services cockpit)]

1. On the [SAP Cloud Platform Trial landing page](https://cockpit.hanatrial.ondemand.com), choose **Enter your Trial Account** to see your global account.

    ![MobileCards](img_0.png)

>The trial account should contain one `subaccount` and `space`.

2. The global trial account contains one subaccount and space. Navigate to subaccount by clicking on the tile named **trial** (this name may vary if you created the subaccount manually).

    ![cf Trial](img_2.png)

3. Under **Spaces**, choose the available space as highlighted below.

    ![cf Trial](img_4.png)

4. In the left pane, choose **Services** > **Service Marketplace**.

    >The **Service Marketplace** is where you can find services to attach to any of your applications. These services are provided by SAP Cloud Platform to create, and produce applications quickly and easily. Once a service has been created, it is known as a `service instance`.

    ![cf Trial](img_5.png)

5. Scroll down and then choose **Mobile Card Kit**.

    ![cf Trial](img_6.png)

6. Choose **Support** to open **SAP Cloud Platform Mobile Services Cockpit**.

    ![cf Trial](img_7.png)

    >**Organization:** Organizations in CF enable collaboration among users and enable grouping of resources.

    >**Space:** Cloud Foundry has a standard working environment for individual applications: it is called a space. Spaces are individual working areas, which normally contain a single application.

7. Choose the relevant **Organization** and **Space** from the dropdown list, and then select **Open**.

    ![cf Trial](img_8.png)

You have now logged in to the SAP Mobile Services cockpit.

![cf Trial](img_9.png)

Bookmark the **Mobile Services cockpit URL** for quick access.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create destination for SAP SuccessFactors sandbox)]

In this step, you will create a destination connecting to SuccessFactors sandbox system.

1. In the left pane, choose **Mobile Applications** | **SAP Mobile Cards** to view the Mobile Cards configuration.

    ![MobileCards](img_10.png)

    >If the SAP Mobile Cards Advisory screen pops up, choose **Close** to close it.

2. Click **Initialize**. This will create a new service instance for Mobile Cards.

    ![MobileCards](img_101.png)

3. Choose **Template Manager**.

    ![MobileCards](img_11.png)

4. Choose **SuccessFactors** from the **Select Category** drop-down list.

    ![MobileCards](img_12.png)

5. Scroll down and choose `SuccessFactors My TimeSheet` and select the  **Create Card Template** icon.

    ![MobileCards](img_13.png)

6. Click [here](https://api.sap.com/preferences) to get the `API Key`.  

    ![MobileCards](img_14.png)

    >For new users or first-time user of API Business Hub, generation of API Key may take a while.

7. Choose **Show API Key**.

    ![MobileCards](img_15.png)

8. Choose **Copy Key and Close**.

    ![MobileCards](img_16.png)

9. Switch back to **Mobile Services cockpit**, paste the copied API key in the open pop-up, and then choose **OK**.

    ![MobileCards](img_17.png)

    On successful creation, a **Destination Created** message is displayed.

    ![MobileCards](img_18.png)

[VALIDATE_1]
[ACCORDION-END]

In this tutorial, you have successfully created a destination connecting to a SAP SuccessFactors sandbox system. In the next tutorial, you will use this destination to create two cards displaying `TimeSheet` and `ToDos` data.


---
