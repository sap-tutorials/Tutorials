---
title: Set Up SAP Business Application Studio for Development
description: Before you can start developing using SAP Business Application Studio, administrators must perform the required onboarding steps that are described in this tutorial.
auto_validation: true
time: 20
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

    !![Access SAP Cloud Platform Trial](2020-03 SCP Access Trial__.jpg)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: 2](Switch to subscriptions view)]

1. Select the tile of the subaccount in which you want to enable the SAP Business Application Studio subscription.

    >For the trial environment, SAP Business Application Studio is only available on Amazon Web Services (AWS) - Frankfurt region.

    >If you do not have a subaccount in this region, you need to create a new subaccount, where:

    > - Provider = **Amazon Web Services (AWS)**
    > - Region = **Europe (Frankfurt)**.

    !![subaccount](2020-03 Cockpit Select Subaccount__.jpg)

2. From the navigation area, click **Subscriptions** .

    !![opensubscriptions](2020-03 Cockpit Navigate to Subscriptions__.jpg)

[VALIDATE_2]
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

[ACCORDION-BEGIN [Step: 4](Assign permissions)]

In this step, you will add the relevant role collections to your user.

Role collections are user-related authorizations that restrict access to resources and services based on defined user permissions. They consist of individual roles. The roles are based on role templates.

For SAP Business Application Studio, there are 2 role templates available:

- **Developer** role, which allows developers to load and develop applications using SAP Business Application Studio
- **Administrator** role, which allows administrators to manage (export and delete) user data

The developer and administrator role collections, together with their corresponding templates, are created automatically when you subscribe to SAP Business Application Studio.

1. Navigate to your subaccount.

    !![subaccount](2020-03 Cockpit Subscription Navigate to Subaccount__.jpg)

2. From the navigation area, choose **Security > Trust Configuration**.

    !![trustconfiguration](2020-03 Cockpit Navigate to Trust Configuration__.jpg)

3. Select the default IdP by clicking on the `Name` attribute. The name might be `SAP ID Service` or `Default Identity Provider`.

    !![idp](2020-03 Cockpit Navigate to IdP__.jpg)

4. Enter the e-mail of the user to whom you want to give permissions, and click **Show Assignments**.

    !![rolecollections](2020-03 Cockpit Role Collections Show Assignments__.jpg)

5. Click **Assign Role Collection**.

    !![assignrolecollections](2020-03 Cockpit Role Collections Assign Role Collection__.jpg)

6. From the **Role Collection** dropdown list, select the **`Business_Application_Studio_Administrator`** role collection, and click **Assign Role Collection**.

    !![assignadministrator](2020-03 Cockpit Role Collections Assign Role Collection - Administrator__.jpg)

7. Repeat the role collection assignment for the **`Business_Application_Studio_Developer`** role collection.

    !![assigndeveloper](2020-03 Cockpit Role Collections Assign Role Collection - Developer__.jpg)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step: 5](Launch SAP Business Application Studio)]

1. Navigate to your subaccount.

    !![subaccount](2020-03 Cockpit Role Collections Navigate to Subaccount__.jpg)

2. From the navigation area, click **Subscriptions** .

    !![subscriptions](2020-03 Cockpit Trust Configuration Navigate to Subscriptions__.jpg)

3. In the **Subscriptions** page, search for **`studio`**.

4. Click **Go to Application**.

    !![gotoapplication](2020-03 Cockpit Go to Application__.jpg)

5. Enter your credentials, and click **Log On**.

    !![authentication](2020-03 AppStudio Authentication__.jpg)

6. You might be asked to accept the legal terms. Check the box and click **OK**.

    !![legalterms](2020-03 AppStudio Terms__.jpg)

7. A new tab opens and SAP Business Application Studio loads.

    >**Bookmark this page!**

    >If you create a bookmark to this page, it is easy to get back to SAP Business Application Studio later.

    !![sapbusinessapplicationstudioloaded](2020-03 AppStudio Loaded_.jpg)


[DONE]
[ACCORDION-END]



---
