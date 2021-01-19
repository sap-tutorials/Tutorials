---
title: Create a Custom Rule with SAP Customer Order Sourcing
description: Learn how to create a custom rule from start to finish with SAP Customer Order Sourcing
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>sap-api-business-hub]
primary_tag: topic>cloud
---

## Prerequisites
 - You have registered for a trial account on [SAP Cloud Platform](https://cloudplatform.sap.com/index.html).
 - You should be familiar with the SAP Cloud Platform trial landscape (see [Get Started with SAP Cloud Platform Trial](cp-trial-quick-onboarding)).
 - You have a subaccount in the **Cloud Foundry** environment, with **Amazon Web Services (AWS)** as provider and the **Europe (Frankfurt)** region.
 - You have followed the tutorial [Set Up SAP Customer Order Sourcing Manually](cos-manual-setup) or steps 1 to 3 of tutorial [Create a Personalized Sourcing Strategy with SAP Customer Order Sourcing](cos-getting-started-trial)

## Details
### You will learn
  - How to create a custom attribute via the SAP Customer Order Sourcing API
  - How to create a custom rule using the custom attribute


  In this tutorial you learn create a custom rule in SAP Customer Order Sourcing from start to finish.

  Custom Rules allow you to extend the rule based sourcing according to your needs.


[ACCORDION-BEGIN [Step 1: ](Add a Custom Attribute via the SAP Customer Order Sourcing API)]

1. Navigate to [SAP Customer Order Sourcing on SAP API Business Hub](https://api.sap.com/api/Sourcing_API/resource), and login with your email address.

2. Select your trial environment, probably named `trial_test`.

3. On the left side choose **Attributes**.

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
> To later use the attribute `sustainabilityrating` you have to include it when uploading sources. See the following example:
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

[ACCORDION-BEGIN [Step 2: ](Create a Custom Rule)]

1. In the [SAP Cloud Platform trial space](https://account.hanatrial.ondemand.com), enter your trial account.

2. Navigate to your subaccount, probably named `trial`.

3. In the navigation pane, open **Subscriptions**.

4. Search for **SAP Customer Order Sourcing**.

5. In the SAP Customer Order Service Overview page, choose **Go to Application** and login.

6. Open the Strategy Builder app and open an existing strategy or create a new one via **+**.

7. Under **Rules** click **Manage**.

8. In the **Manage Rules** view, create a new rule by clicking **+**.

9. For the Name, enter **Highest Sustainability Rating**.

10. For the Description, enter "The focus of this rule is to deliver products only from the source with the highest (best) sustainability rating."

11. From the **Numeric Attribute** dropdown list choose `sustainabilityrating`.

12. As **Comparator** choose `Highest Value`.

13. Save the rule.

You are now able to use the custom rule in the **Strategy Builder**.

For sourcing to properly work you have upload sources which include the sustainability rating attribute.

For more information on how to upload source data see **Step 4** of tutorial [Create Personalized Sourcing Strategies Depending on Location and Priorities](cos-advanced-sourcing).

[VALIDATE_1]
[ACCORDION-END]

### Additional Information

- [Official Product Documentation](https://help.sap.com/viewer/product/SAP_CUSTOMER_ORDER_SOURCING/Cloud/en-US)
- [Trial Documentation](https://help.sap.com/viewer/DRAFT/cd03af1a94a440f1b5dbc0dc50a0989b/Cloud/en-US)
- [How does the Rule "Shortest Distance to Destination" work?](https://help.sap.com/viewer/a8094e21e0ed43b39ad79ade28eefabb/Cloud/en-US/a96a37284b5142ee968e9c9392304920.html)
- [API Reference Documentation](https://help.sap.com/viewer/59d653d22328437c9e0817340181b896/Cloud/en-US)
- [SAP Customer Order Sourcing on the SAP API Business Hub](https://api.sap.com/package/CustomerOrderSourcing?section=Artifacts)
- [Additional information on how to configure Environments on the API Business Hub](https://help.sap.com/viewer/84b35b9c39b247e3ba2a31f02beee46d/Cloud/en-US/f7796baaef6a48e9867298827f5028ff.html)
- [**Video:** SAP Customer Order Sourcing in a nutshell](https://www.youtube.com/watch?v=novFLk35X2I)


---



---
