---
title: Set Up SAP Business Application Studio for Development
description: Before you can start developing using SAP Business Application Studio, administrators must perform the required onboarding steps that are described in this tutorial.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>sapui5, products>sap-fiori, products>sap-cloud-platform, products>sap-cloud-platform-workflow, software-product-function>sap-cloud-application-programming-model, topic>mobile, products>sap-mobile-cards, products>mobile-development-kit-client]
primary_tag: products>sap-business-application-studio
---

## Prerequisites
 - You have an SAP Cloud Platform account: [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account)

## Details
### You will learn
  - How to set up SAP Business Application Studio

This tutorial is based on the procedure described in the [Getting Started](https://help.sap.com/viewer/9d1db9835307451daa8c930fbd9ab264/Cloud/en-US/19611ddbe82f4bf2b493283e0ed602e5.html) topic of the SAP Business Application Studio Administrator Guide.

---

[ACCORDION-BEGIN [Step: 1](Log into SAP Cloud Platform)]


1. Go to <https://account.hanatrial.ondemand.com> and log in to your SAP Cloud Platform cockpit.

2. Click **Enter Your Trial Account** to access the Cloud Foundry environment.

    !![Access SAP Cloud Platform Trial](2020-08 SCP Access Trial_.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: 2](Switch to subscriptions view)]

1. Select the tile of the subaccount in which you want to enable the SAP Business Application Studio subscription.

    >For the trial environment, SAP Business Application Studio is only available on:

    > - Amazon Web Services (AWS) - Europe (Frankfurt) or US East (VA) regions.

    >If you do not have a subaccount in one of these regions, you need to create a new subaccount, where:

    > - Provider = **Amazon Web Services (AWS)**
    > - Region = **Europe (Frankfurt)** or **US East (VA)**.

    !![subaccount](2020-08 Cockpit Select Subaccount_.jpg)

2. From the navigation area, click **Subscriptions**.

    !![opensubscriptions](2020-08 Cockpit Navigate to Subscriptions_.jpg)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step: 3](Add SAP Business Application Studio subscription to subaccount)]

In this step, you will add the SAP Business Application Studio subscription to a subaccount. Depending on when you created the SAP Cloud Platform account, this subscription might already be added in the subaccount.

1. In the **Subscriptions** page, search for **`studio`**.

2. Click the **SAP Business Application Studio** tile.

    !![findsubscription](2020-03 Cockpit Filter and Select AppStudio Subscription__.jpg)

3. Click **Subscribe** to add the subscription to the subaccount.

    !![subscribe](2020-03 Cockpit Subscribe__.jpg)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: 4](Launch SAP Business Application Studio)]

1. Click **Go to Application**.

    !![gotoapplication](2020-08 Cockpit Go to Application_.jpg)

2. You might be asked to accept the legal terms. Check the box and click **OK**.

    !![legalterms](2020-03 AppStudio Terms__.jpg)

3. An **Access Denied** page may appear. Log out from SAP Business Application Studio and then log in as depicted below.

    !![Logout](2020-05 AppStudio Access Denied Logout_.jpg)
    &nbsp;
    !![Login](2020-05 AppStudio Access Denied Login_.jpg)

4. Enter your credentials, and click **Log On**.

    !![authentication](2020-03 AppStudio Authentication__.jpg)

5. You might be asked to accept the legal terms. Check the box and click **OK**.

    !![legalterms](2020-03 AppStudio Terms__.jpg)

6. A new tab opens and SAP Business Application Studio loads.

    >**Bookmark this page!**

    >If you create a bookmark to this page, it is easy to get back to SAP Business Application Studio later.

    !![sapbusinessapplicationstudioloaded](2020-03 AppStudio Loaded_.jpg)

[VALIDATE_4]
[ACCORDION-END]

---

Congratulations!

With this, you have successfully completed the setup of SAP Business Application Studio.
