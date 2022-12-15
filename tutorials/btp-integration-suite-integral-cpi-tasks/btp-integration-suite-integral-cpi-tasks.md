---
title: Save, Filter and Get your needed data in your Integration Flow
description: Boost your Integration Suite skills leveraging different tasks to store and filter messages travelling through your Integration Flow.
auto_validation: true
time: 10
tags: [ tutorial>beginner, tutorial>free-tier, software-product>sap-integration-suite, software-product>cloud-integration, software-product>sap-business-technology-platform  ]
primary_tag: software-product>sap-integration-suite
parser: v2
author_name: Mariajose Martinez
author_profile: https://github.com/mariajosesap
---

## Prerequisites

 - You have a SAP BTP account or trial account with access to the SAP Integration Suite.
 - You have completed the previous tutorial [Consume a Stripe service from SAP Open Connectors and SAP Cloud Integration to create payment transactions](btp-integration-suite-integral-openconnectors).


## You will learn

  - How to store, get and filter data to prepare your payload for specific request calls within the Integration Flow.

### Add Converters, Write and Filter Tasks

1. As SAP Cloud Integration works with XML and the expected payload from Open Connector/Stripe is in JSON, you need to add a JSON to XML converter in order to store the initial payload from SAP Build Apps in CPI and retrieve it when the payment transaction is successfully done.

2. Uncheck the "Add XML Root Element" box. You'll use the payload with the root as it comes.

    ![Adding the JSON to XML Converter](json_to_xml.png)

3. Add a Write task to store the initial payload you are going to send from SAP Build Apps. Select it from the cube "Persistence" and "Data Store Operations".

    ![Write Task](write_task.png)

4. Add the following:

    - Data Store Name: `AppGyverSavedPayload`
    - Visibility: Integration Flow
    - Entry ID: message1
    - Retention Threshold for Alerting (in days): reduce it to 1 day.
    - Expiration Period (in days): reduce it to 2 days.
    - Uncheck the "Encrypt Stored Message" box. For this exercise you are not going to encrypt the message.

        ![Write Task](write_task_setup.png)

5. Add a "Filter" task. This is needed to only send the payment data to stripe. 

6. Add this `Xpath` Expression in the Processing Tab: `/AppGyverSalesOrder/paymentData` and select `Nodelist` as the `Valuetype`. It is needed to select `Nodelist` as there is a list in the main payload: 

    <!-- cpes-file db/schema.cds -->
    ```JSON
    {
    "AppGyverSalesOrder": {
        "customer": {
        "ObjectID": "<Object ID from SAP C4C>",
        "BuyerPartyID": <BuyerPartyID from SAP C4C>
        },
        "paymentData": {
        "amount": <Payment transaction amount to be processed in Stripe>,
        "customer": "<stripe customer id>",
        "currency": "<currency selected in stripe account>",
        "source": "<card id>",
        "description": "Test payment via CPI"
        },
        "product": [
        {
            "ProductID": "<Product ID from SAP C4C>",
            "Quantity": <Product quantity to be purchased>
        }
        ]
    }
    }
    ```

    >Note: Bear in mind that the "customer" and "product" objects are included as part of the whole scenario. You'll need them for requesting SAP Sales and Service Core a sales order creation, which you'll see in the next tutorial.

    It should look like this:

    ![Filter Task](filter_task.png)

7. Add a XML to JSON converter. Stripe needs a JSON object to create the payment transaction. Make sure you select UTF-8 as the JSON Output Encoding, and check the box "Suppress JSON Root Element", this is important to be able to enter the data inside the `XPath` Expression without the "parent" root:

    ![XML to JSON Converter](xml_to_json.png)

### Test your Integration Flow

1. Test on Postman and check the payment transaction is still working, use the Payload structure as it is above, just change it to your data. You should be able to generate the payment transaction.

    ![Testing it with Postman](test_postman.png)

2. Go back to Monitor. Remember you need to enable first the Trace option. Check the trace:

    ![Checking the trace](check_trace.png)


### Add a Get Task

1. Add a Get task to retrieve the initial payload. You have to put the same name and entry ID as you did in the Write task. Check the "Delete On Completion" box, to delete the stored message when the Integration Flow transaction is completed.

    ![Get Task](get_task.png)

    You can test again if you want to, you should see the whole initial payload as it was saved with the Write task.

    Remember to activate the Trace log after deploying your IFlow. Go to the Monitor Message Processing and Trace.

    ![Log the trace](trace_log.png)

Now, you have successfully retrieve the initial payload using Write and Get task, plus learnt how to filter data using Filter task and `XPath` expressions.

Following the Tutorial Series, you now need to configure the SAP Sales and Service Core API consumption to create Sales Orders. For this step, check out the next tutorial: [Consume a SAP Sales and Service Core API to create Sales Orders using SAP Cloud Integration](btp-integration-suite-integral-sales-core).

### Check your knowledge

