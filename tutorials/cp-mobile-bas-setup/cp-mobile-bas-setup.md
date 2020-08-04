---
title: Set Up Business Application Studio for Mobile Technologies
description: Set up your Business Application Studio to start developing mobile solutions.
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>mobile, operating-system>ios, operating-system>android, products>sap-cloud-platform, products>sap-mobile-cards, software-product-function>sap-cloud-platform-mobile-services, products>sap-business-application-studio, products>mobile-development-kit-client ]
primary_tag: software-product-function>sap-cloud-platform-mobile-services
---

## Prerequisites
 - [You have access to Business Application Studio](https://developers.sap.com/tutorials/appstudio-onboarding.html)

## Details
### You will learn
  - How to create a development space in Business Application Studio
  - How to connect to your Cloud Foundry Target in SAP Business Application Studio

SAP Business Application Studio is the next generation web based IDE hosted on SAP Cloud Platform in the Cloud Foundry environment. In this tutorial, you will learn how to create your first card in SAP Business Application Studio. [Click here to learn more about SAP Mobile Cards in SAP Business Application Studio.](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/getting-started/mck/mck-2-2-development-bas.html)

---

[ACCORDION-BEGIN [Step 1: ](Create a Development Space)]

Log into your Business Application Studio and click **Create Dev Space**.

!![SAP Business Application Studio Dashboard](img_1_1.png)

Select SAP Cloud Platform Mobile Services, enter a name (`mobileDevSpace`) for your dev space and click **Create**.

!![New Development Space](img_1_2.png)

Your dev space will be created and the status will change to running.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set Up Your Workspace)]

Click on your dev space's name to open it.

!![SAP Business Application Studio Dashboard with newly created Development Space](img_2_1.png)

Wait till your workspace loads completely.

!![SAP Business Application Studio Home](img_2_2.png)

In the menu bar, go to File &rarr; Open Workspace, click **Open Workspace**.

!![Open Workpace View](img_2_3.png)

Select projects, Click **Open**.

!![Select Workpace](img_2_4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Configure Cloud Foundry Target)]

In the bottom left corner of your status bar, click on the message **`The organization and space in Cloud Foundry have not been set.`**.

!![CF View](img_3_1.png)

Verify the URL and **Click Enter** on your keyboard.

!![CF API End-Point](img_3_2.png)

> SAP Business Application Studio pre-populates the end-point of the environment it is running in.

When prompted, **enter your e-mail address** you use to log in to the SAP Cloud Platform account.

!![Email ID Prompt](img_3_3.png)

Next, **enter your password** you use to log in to the SAP Cloud Platform account.

!![Password Prompt](img_3_4.png)

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

**Congratulations!** You have completed this tutorial.

You have configured SAP Business Application Studio to build mobile apps.

---
