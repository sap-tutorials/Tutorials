---
title: Set Up SAP Customer Order Sourcing Manually
description: Learn to set up SAP Customer Order Sourcing start to finish, how to add Users and give them appropriate permissions.
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-business-technology-platform, products>sap-btp--cloud-foundry-environment]
primary_tag: topic>cloud
---

## Prerequisites
- You have registered for a trial account on [SAP Business Technology Platform](https://cloudplatform.sap.com/index.html).
- You should be familiar with the SAP Business Technology Platform trial landscape (see [Get Started with SAP Business Technology Platform Trial](cp-trial-quick-onboarding)).
- You have a subaccount in the **Cloud Foundry** environment, with **Amazon Web Services (AWS)** as provider and the **Europe (Frankfurt)** region.

## Details
### You will learn
  - How to manually set up SAP Customer Order Sourcing

In this tutorial you learn how to set up SAP Customer Order Sourcing from start to finish.

This might help you in getting a better understanding on how the service works and shows you the options you have, when working with it.

With this you'll learn exactly what you have to do to add Users to your service and how to give them the appropriate permissions.

But if you want an easy and quick way to setup SAP Customer Order Sourcing, use this tutorial: [Create a Personalized Sourcing Strategy with SAP Customer Order Sourcing](cos-getting-started-trial).

---

[ACCORDION-BEGIN [Step 1: ](Create a service instance)]

1. Navigate to your subaccount.

2. In the navigation pane, open **Services > Service Marketplace**.

3. Search for **SAP Customer Order Sourcing** and click on the tile.

4. Choose **Create**, insert a name for the instance and finish the creating by clicking **Create**.
   >Keep the preselected entries.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Subscribe to SAP Customer Order Sourcing)]


1. In the [SAP Business Technology Platform trial space](https://account.hanatrial.ondemand.com), enter your trial account.

2. Navigate to your subaccount, probably named `trial`.

3. In the navigation pane, open **Service Marketplace**.

4. Search for **SAP Customer Order Sourcing** and click on it to open the **Overview** page.

5. Choose **Actions** ![Link text e.g., Destination screen](Actions_Button.png) and **Subscribe** to the service.

    **Go to Application** will be available once the subscription is activated. Later in this post, you can use this link to access the Strategy Builder.

    !![Subscribe to SAP Customer Order Sourcing](SubscribeToCustomerOrderSourcing.png)


>In case you are not able to subscribe to **SAP Customer Order Sourcing**, you need to [assign quota to a Subaccount](https://help.sap.com/viewer/cd03af1a94a440f1b5dbc0dc50a0989b/Cloud/en-US/bdf64a959a5249cf88b414a7c01391df.html).

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Assign read/write permissions to your user)]

You have successfully subscribed to SAP Customer Order Sourcing, but to be able to use it you have to assign read and write permissions to your user:

1. Navigate back to your subaccount.

2. In the navigation pane, open **Security > Trust Configuration**.

3. Open the default identity provider, named `sap.default`.

4. Enter your email address (the one you used to register for a SAP Business Technology Platform trial account) and search for already existing assignments via **Show Assignments**.

5. Choose **Assign Role Collection** and assign the role collection `StrategyBuilder_ReadWrite`.

!![Trust Configuration](TrustConfiguration.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create credentials)]

1. Open your newly created service instance via the **View Instance** button in the pop up or via **Services > Service Instances**.

2. Click on the **Actions** button and from the dropdown menu choose **Create Service Key**.

    !![Actions button](Actions_Button.png)

3. In the pop-up window, just enter a name for your service key. Click **Create**.

Your service key will look something like this.

If you navigate back to SAP Customer Order Sourcing under **Subscriptions** you can use the **Go to Application** link to enter your SAP Customer Order Sourcing trial account

!![Service Key](ServiceKey.png)

[DONE]
[ACCORDION-END]


---
