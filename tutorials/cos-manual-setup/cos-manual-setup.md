---
title: Set Up SAP Customer Order Sourcing Manually
description: Set up SAP Customer Order Sourcing manually.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud]
primary_tag: topic>cloud
---

## Prerequisites
- You have registered for a trial account on [SAP Cloud Platform](https://cloudplatform.sap.com/index.html).
- You should be familiar with the SAP Cloud Platform trial landscape (see [Get Started with SAP Cloud Platform Trial](cp-trial-quick-onboarding)).
- You have a subaccount in the **Cloud Foundry** environment, with **Amazon Web Services (AWS)** as provider and the **Europe (Frankfurt)** region.

## Details
### You will learn
  - How to manually set up SAP Customer Order Sourcing

In this tutorial you learn how to set up SAP Customer Order Sourcing from start to finish. This might help in giving you a better understanding how the service works and shows you the options you have, when working with it.

If you want an easy and quick way to setup SAP Customer Order Sourcing, use this tutorial: [Create a Personalized Sourcing Strategy with SAP Customer Order Sourcing](cos-getting-started-trial).
---

[ACCORDION-BEGIN [Step 1: ](Subscribe to SAP Customer Order Sourcing)]


1. In the [SAP Cloud Platform trial space](https://account.hanatrial.ondemand.com), enter your trial account.

2. Navigate to your subaccount, probably named `trial`.

3. In the navigation pane, open **Subscriptions**.

4. Search for **SAP Customer Order Sourcing** and click on it to open the **Overview** page.

5. **Subscribe** to the service.

    **Go to Application** will be available once the subscription is activated. Later in this post, you can use this link to access the Strategy Builder.

    !![Subscribe to SAP Customer Order Sourcing](SubscribeToCustomerOrderSourcing.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Assign read/write permissions to your user)]

You have successfully subscribed to SAP Customer Order Sourcing, but to be able to use it you have to assign read and write permissions to your user:

1. Navigate back to your subaccount.

2. In the navigation pane, open **Security > Trust Configuration**.

3. Open the **Default identity provider**.

4. Enter your email address (the one you used to register for a SAP Cloud Platform trial account) and search for already existing assignments via **Show Assignments**.

5. Choose **Assign Role Collection** and assign the role collection `StrategyBuilder_ReadWrite`.

![Trust Configuration](TrustConfiguration.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a service instance)]

1. Navigate to your subaccount.

2. In the navigation pane, open **Spaces**.

3. Open your already existing space, probably called `dev`.

4. In the navigation pane, open **Services > Service Marketplace**.

5. Search for **SAP Customer Order Souring** and click on the tile.

6. In the navigation pane, open **Instances**.

7. Create a new instance by clicking **New Instance** and then **Next** three times and inserting an instance name on the last page. Click **Finish**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create credentials)]

1. Open your newly created service instance.

2. In the navigation pane, open **Service Keys**.

3. Click **Create Service Key**. In the pop-up window, just enter a name for your service key. Click **Save**.

Your service key will look something like this.

If you navigate back to SAP Customer Order Sourcing under **Subscriptions** you can use the **Go to Application** link to enter your SAP Customer Order Sourcing trial account

!![Service Key](ServiceKey.png)

[DONE]
[ACCORDION-END]


---
