---
title: Set Up SAP Business Application Studio for Mobile Development
description: Set up your SAP Business Application Studio to start developing mobile apps.
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>mobile, operating-system>ios, operating-system>android, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services, products>sap-business-application-studio, products>mobile-development-kit-client ]
primary_tag: software-product-function>sap-cloud-platform-mobile-services
---

## Prerequisites
 - You have [Set Up SAP Business Application Studio for Development](appstudio-onboarding).

## Details
### You will learn
  - How to create a development space in SAP Business Application Studio
  - How to connect to your Cloud Foundry target in SAP Business Application Studio

SAP Business Application Studio is the next-generation web-based IDE hosted on SAP Cloud Platform in the Cloud Foundry environment. In this tutorial, you will set up your SAP Business Application Studio for developing mobile apps.

---

[ACCORDION-BEGIN [Step 1: ](Create a development space)]

Log into your Business Application Studio and click **Create Dev Space**.

!![SAP Business Application Studio Dashboard](img_1_1.png)

Select SAP Cloud Platform Mobile Services, enter a name (`Mobile`) for your dev space and click **Create**.

!![New Development Space](img_1_2.png)

Your dev space will be created and the status will change to running.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up your workspace)]

Click your dev space's name to open it.

!![SAP Business Application Studio Dashboard with newly created Development Space](img_2_1.png)

Wait till your workspace loads completely.

There is no workspace opened yet, click **Open Workspace**.

!![SAP Business Application Studio Home](img_2_2.png)

Select projects, Click **Open**.

!![Select Workpace](img_2_4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure Cloud Foundry environment)]

In the bottom left corner of your status bar, click the message **`The organization and space in Cloud Foundry have not been set.`**.

!![CF View](img_3_1.png)

Verify the URL and **Click Enter** on your keyboard.

!![CF API End-Point](img_3_2.png)

> SAP Business Application Studio pre-populates the end-point of the environment it is running in. If you want to connect to a different environment, modify the API endpoint by copying it from your target SAP Cloud Platform account: *SAP Cloud Platform Cockpit &rarr; Sub-account &rarr; API Endpoint*

When prompted, **enter your e-mail address** you use to log in to the SAP Cloud Platform account.

!![Email ID prompt](img_3_3.png)

Next, **enter your password** you use to log in to the SAP Cloud Platform account.

!![Password prompt](img_3_4.png)

Upon successful login, you will see a toast message at the bottom right corner of your screen.

!![Sucess toast message](img_3_5.png)

Select the organisation in which you have enabled Mobile Services.

!![Org list](img_3_6.png)

 Select the space in which you have enabled Mobile Services.

!![Space list](img_3_7.png)

Upon successful setup, you will see a toast message at the bottom right corner of your screen, and the bottom status bar will highlight CF connection details.

!![Success toast message](img_3_8.png)

[DONE]
[ACCORDION-END]

**Congratulations!** You have successfully configured SAP Business Application Studio to build mobile apps.

---
