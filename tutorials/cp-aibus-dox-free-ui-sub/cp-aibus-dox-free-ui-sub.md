---
title: Use Free Tier to Subscribe to Document Information Extraction UI
description: Use the free tier service plan to get access to the Document Information Extraction user interface application, using SAP Business Technology Platform (SAP BTP).
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>document-information-extraction, tutorial>free-tier]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

## Prerequisites
- You have created a service instance for Document Information Extraction: [Use Free Tier to Create a Service Instance for Document Information Extraction](cp-aibus-dox-free-service-instance)

## Details
### You will learn
  - How to check your subaccount entitlements and add Document Information Extraction UI
  - How to subscribe, assign role collection and access Document Information Extraction UI

---

[ACCORDION-BEGIN [Step 1: ](Access the SAP BTP cockpit)]

After completing the prerequisite tutorial [Use Free Tier to Create a Service Instance for Document Information Extraction](cp-aibus-dox-free-service-instance), to create a service instance for Document Information Extraction, you can start with the steps to subscribe to the Document Information Extraction UI.

1. Open the [SAP BTP cockpit](https://account.hana.ondemand.com/cockpit#/home/allaccounts).

2. Access your global account.

    !![Access Subaccount](global-account.png)

3. Click the tile to access your subaccount.

!![Access Subaccount](access-subaccount.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Check entitlements)]

To use Document Information Extraction UI, you need to make sure that your account is properly configured.

1. On the navigation side bar, click **Entitlements** to see a list of all eligible services. You are entitled to use every service in this list according to the assigned service plan.

2. Search for `Document Information Extraction UI`. ***If you find the service in the list, you are entitled to use it. Now you can set this step to **Done** and proceed with Step 3.***

    !![Check Entitlements](check-entitlements.png)

***ONLY if you DO NOT find the service in your list, proceed as follows:***

  1.  Click **Configure Entitlements**.

    !![Configure Entitlements](configure-entitlements.png)

  2.  Click **Add Service Plans**.

    !![Add Service Plans](add-service-plans.png)

  3.  In the dialog, select `Document Information Extraction UI` and choose the `default` service plan. Click **Add 1 Service Plan**.

    !![Add Service Plan](add-service-plan.png)

  4.  Click **Save** to save your entitlement changes.

    !![Save Entitlements](save-entitlements.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Get subscribed)]

1. On the navigation side bar, click **Service Marketplace**.

    !![DOX-UI](access-service-marketplace.png)

2. Search for **Document Information Extraction UI** and click the tile.

    !![DOX-UI](tile.png)

3. Click **Create**.

    !![DOX-UI](subscribe.png)

4. In the dialog, click **Create** once again.

  !![DOX-UI](dialog-create.png)

The subscription will now be created. Click on **View Subscription** to go to the list of your existing subscriptions.

!![DOX-UI](dialog-in-progress.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Assign role collection)]

1. Under **Security**, click **Users** and then the **Actions** arrow.

    !![DOX-UI](users.png)    

2. Click **Assign Role Collection**.

    !![DOX-UI](role-collection.png)

3. Choose **`Document_Information_Extraction_UI_Admin_User`** to access all the features available in the UI application and click **Assign Role Collection**.

    !![DOX-UI](assign-role-collection.png)

You're now assigned to the **`Document_Information_Extraction_UI_Admin_User`** role collection.

!![DOX-UI](roles.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Go to application)]

Go back to **Instances and Subscriptions**, click the dots to open the menu and select **Go to Application** to open the app.

!![DOX-UI](go-to-app.png)

The Document Information Extraction UI is displayed:

!![DOX-UI](app.png)

You have successfully subscribed to the Document Information Extraction UI.

[DONE]
[ACCORDION-END]
