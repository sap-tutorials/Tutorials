---
title: Create a Custom Goal with SAP Customer Order Sourcing
description: Learn how to create a custom goal from start to finish with SAP Customer Order Sourcing.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>sap-api-business-hub]
primary_tag: topic>cloud
---

## Prerequisites
 - You have registered for a trial account on [SAP Business Technology Platform](https://cloudplatform.sap.com/index.html).
 - You should be familiar with the SAP Business Technology Platform trial landscape (see [Get Started with SAP Business Technology Platform Trial](cp-trial-quick-onboarding)).
 - You have a subaccount in the **Cloud Foundry** environment, with **Amazon Web Services (AWS)** as provider and the **Europe (Frankfurt)** region.
 - You have followed the tutorial [Set Up SAP Customer Order Sourcing Manually](cos-manual-setup) or steps 1 to 3 of tutorial [Create a Personalized Sourcing Strategy with SAP Customer Order Sourcing](cos-getting-started-trial)

## Details
### You will learn
  - How to create a custom attribute via the SAP Customer Order Sourcing API
  - How to create a custom goal using the custom attribute


  In this tutorial you learn create a custom goal in SAP Customer Order Sourcing from start to finish.

  Custom Goals allow you to extend the goal based sourcing according to your needs.


[ACCORDION-BEGIN [Step 1: ](Add custom attribute via SAP Customer Order Sourcing API)]

1. Navigate to [SAP Customer Order Sourcing on SAP API Business Hub](https://api.sap.com/api/Sourcing_API/resource), and login with your email address.

2. Select your trial environment, probably named `trial_test`.

3. On the left side, choose **Attributes**.

4. Open `POST/attributes` and choose `Try Out`.

5. Copy the following attribute information and paste it into the body:

    ```JSON
    {
      "extensionType": "SOURCE",
      "name": "sustainabilityrating",
      "description": "Sustainability rating of sources.",
      "type": "DOUBLE",
      "defaultValue": 50
    }

    ```
> To later use the attribute `sustainabilityrating`, you have to include it when uploading sources. See the following example:
    ```JSON
{"items": [
    {
      "sourceId": "dc1",
      "sourceName": "Berlin",
      "sourceType": "DC",
      "availableToSellDataProvider": "COS",
      "attributes": {
        "sustainabilityrating": 77
      },
      "sourceCoordinates": {
        "latitude": 52.5170365,
        "longitude": 13.3888599
      }
    }
]}
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a custom goal)]

1. In the [SAP Business Technology Platform trial space](https://account.hanatrial.ondemand.com), enter your trial account.

2. Navigate to your subaccount, probably named `trial`.

3. In the navigation pane, open **Service Marketplace**.

    Search for **SAP Customer Order Sourcing**.

4. Open the SAP Customer Order Service tile, choose **Go to Application**, and log in.

5. Open the Strategy Builder app and open an existing strategy or create a new one via **+**.

6. Under **Goals** click **Manage**.

7. In the **Manage Goals** view, create a new goal by clicking **+**.

    For the goal, set the following:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  **Name**           | **`Highest Sustainability Rating`**
    |  **Description**    | **`The focus of this goal is to deliver products only from the source with the highest (best) sustainability rating.`**
    |  **Numeric Attribute**    | **`sustainabilityrating`**
    |  **Comparator**          | **`Highest Value`**

    Save the goal.

You are now able to use the custom goal in the **Strategy Builder**.

For sourcing to properly work, you must upload sources that include the sustainability rating attribute.

For more information on how to upload source data, see **Step 4** of tutorial [Create Personalized Sourcing Strategies Depending on Location and Priorities](cos-advanced-sourcing).

[VALIDATE_1]
[ACCORDION-END]

---

### Additional Information

- [Official Product Documentation](https://help.sap.com/viewer/product/SAP_CUSTOMER_ORDER_SOURCING/Cloud/en-US?task=use_task)
- [Trial Documentation](https://help.sap.com/viewer/cd03af1a94a440f1b5dbc0dc50a0989b/Cloud/en-US)
- [Extensibility Documentation](https://help.sap.com/viewer/0e899d665ec840908ddb3eba9a6c25dc/Cloud/en-US)
- [API Reference Documentation](https://help.sap.com/viewer/59d653d22328437c9e0817340181b896/Cloud/en-US)
- [SAP Customer Order Sourcing on the SAP API Business Hub](https://api.sap.com/package/CustomerOrderSourcing?section=Artifacts)
- [**Video:** SAP Customer Order Sourcing in a nutshell](https://www.youtube.com/watch?v=novFLk35X2I)


---



---
