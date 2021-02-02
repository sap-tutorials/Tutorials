---
title: Set Up SAP Business Application Studio for Development
description: SAP Business Application Studio is a development environment available on SAP Business Technology Platform. Before you can start developing using SAP Business Application Studio, administrators must perform the required onboarding steps that are described in this tutorial.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>sapui5, products>sap-fiori, products>sap-cloud-platform, products>sap-cloud-platform-workflow, software-product-function>sap-cloud-application-programming-model, topic>mobile, products>sap-mobile-cards, products>mobile-development-kit-client]
primary_tag: products>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

## Prerequisites
 - You have an SAP BTP Trial account: [Get a Free Account on SAP BTP Trial](hcp-create-trial-account)

## Details
### You will learn
  - How to set up SAP Business Application Studio

This tutorial is based on the procedure described in the [Getting Started](https://help.sap.com/viewer/9d1db9835307451daa8c930fbd9ab264/Cloud/en-US/19611ddbe82f4bf2b493283e0ed602e5.html) topic of the SAP Business Application Studio Administrator Guide.

---

[ACCORDION-BEGIN [Step: 1](Log into SAP BTP Trial)]

Go to <https://account.hanatrial.ondemand.com> and log in to your SAP Cloud Platform cockpit.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: 2](Launch SAP Business Application Studio)]

1. Click **SAP Business Application Studio** to launch SAP Business Application Studio.

    !![launch sap business application studio](2020-10-SCP-Access-AppStudio-.png)

    >In recently created trial accounts, SAP Business Application Studio is subscribed to by default.

    >If you receive an error message and your recently created account was created with error, it is recommended that you re-create your account: delete the account and then [Get a Free Account on SAP BTP Trial](hcp-create-trial-account).

    >If you receive an error message and your account was not created recently, answer the validation question of the current step, go to the next step in this tutorial, and proceed from there.

2. You might be asked to accept the legal terms. Check the box and click **OK**.

    !![legalterms](2020-03-AppStudio-Terms--.png)

3. A new tab opens and the welcome page for SAP Business Application Studio loads.

!![sap business application studiowelcome](BAS_Welcome-.png)

4. Click **My Dev Spaces** to open the SAP Business Application Studio dev space manager page.
    <br>
    >**Bookmark this page!**

    >If you create a bookmark to this page, it is easy to get back to SAP Business Application Studio later.

    !![sap business application studio dev space manager](BAS-Dev-Space-Manager-Empty-.png)

    >If you reached this point in the tutorial, you have successfully on-boarded SAP Business Application Studio. Answer the validation question of the current step, and mark the rest of the steps as "Completed".

[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: 3](Add SAP Business Application Studio subscription to subaccount)]

In this step, you will add the SAP Business Application Studio subscription to a subaccount. Depending on when you created the SAP BTP Trial account, this subscription might already be added in the subaccount.

1. Click **Enter Your Trial Account** to access the Cloud Foundry environment.

    !![Access SAP BTP Trial](2020-08-SCP-Access-Trial-.png)

2. Select the tile of the subaccount in which you want to enable the SAP Business Application Studio subscription.

    >For the trial environment, SAP Business Application Studio is only available on:

    > - Amazon Web Services (AWS) - Europe (Frankfurt) or US East (VA) regions.

    > - Microsoft Azure - Singapore region.

    >When creating an SAP BTP Trial account a **trial** subaccount is generated.

    !![subaccount](2020-08-Cockpit-Select-Subaccount-.png)

3. From the navigation area, click **Subscriptions**.

    !![opensubscriptions](2020-08-Cockpit-Navigate-to-Subscriptions-.png)

4. In the **Subscriptions** page, search for **`studio`**.

5. Click the **SAP Business Application Studio** tile.

    !![findsubscription](2020-03-Cockpit-Filter-and-Select-AppStudio-Subscription--.png)

6. Click **Subscribe** to add the subscription to the subaccount.

    !![subscribe](2020-03-Cockpit-Subscribe--.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: 5](Launch SAP Business Application Studio)]

1. Click **Go to Application**.

    !![gotoapplication](2020-08-Cockpit-Go-to-Application-.png)

2. You might be asked to accept the legal terms. Check the box and click **OK**.

    !![legalterms](2020-03-AppStudio-Terms--.png)

3. An **Access Denied** page may appear. Log out from SAP Business Application Studio and then log in as depicted below.

    !![Logout](2020-05-AppStudio-Access-Denied-Logout-.png)
    &nbsp;
    !![Login](2020-05-AppStudio-Access-Denied-Login-.png)

4. Enter your credentials, and click **Log On**.

    !![authentication](2020-03-AppStudio-Authentication--.png)

5. You might be asked to accept the legal terms. Check the box and click **OK**.

    !![legalterms](2020-03-AppStudio-Terms--.png)

6. A new tab opens and the welcome page for SAP Business Application Studio loads.

    !![sap business application studio welcome](BAS_Welcome-.png)

7. Click **My Dev Spaces** to open the SAP Business Application Studio dev space manager page.
    <br>
    >**Bookmark this page!**

    >If you create a bookmark to this page, it is easy to get back to SAP Business Application Studio later.

    !![sap business application studio dev space manager](BAS-Dev-Space-Manager-Empty-.png)

[DONE]
[ACCORDION-END]

---

Congratulations!

With this, you have successfully completed the setup of SAP Business Application Studio.
