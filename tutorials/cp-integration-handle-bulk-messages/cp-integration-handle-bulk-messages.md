---
title: Handling Bulk Messages
description: Learn how to handle messages individually, when receiving multiple messages as a part of one big message.
time: 10
auto_validation: true
tags: [ tutorial>intermediate, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform-integration-for-process-services
---

## Details
### You will learn
  - How to split a message containing multiple messages into individual messages
  - How mapping works with bulk messages

Assume that a customer orders multiple items. The input XML will then contain all the item information. In this exercise, we shall separate the items and process them individually.

---

[ACCORDION-BEGIN [Step 1: ](Process a bulk message)]

1. Let us change our input message and add 2 more items to it.

    * Add the following text to your input message:

    ```XML
    <item>
    <ProductID>RPD4044544</ProductID>
    <ProductDescription>Keyboard</ProductDescription>
    <Quantity>1</Quantity>
    <ItemValue>1057</ItemValue>
    </item>

    <item>
    <ProductID>RPD4044545</ProductID>
    <ProductDescription>Webcam</ProductDescription>
    <Quantity>1</Quantity>
    <ItemValue>1057</ItemValue>
    </item>  

    ```

    Once done, your message should look like:

    ![New Input](New Input.png)

2. Execute your flow with the new message. You will see that the output mail only contains 1 item information:

    ![Bulk items ignored](Bulk items ignored.png)

3. This is because the cardinality of the __Item__ field is set to 1..1.

    ![Incorrect Cardinality](Incorrect Cardinality.png)

4. Edit the XSD.

    * Go to the __Item__ field and add ```minOccurs="1" maxOccurs="unbounded"``` to it as attributes as follows:

    ![Change Cardinality](Change Cardinality.png)

    * Go to the integration flow's __Resources View__ and delete both Order.xsd and WarehouseOrder.xsd.

    ![Delete XSDs](Delete XSDs.png)

    * Go to the Message Mapping and reimport the edited XSD.

    ![Add New XSD](Add New XSD.png)

    > In spite of having deleted the XSD, SAP Cloud Platform Integration preserves the maps.  

    * Check if all the maps are configured.

    * Save, deploy and execute your flow. You should get one composite mail containing all product information:

    ![Combined Mail](Combined Mail.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Split message)]

1. Add a __Splitter__ step to the integration flow:

    * Click on the design Palette.
    * Choose __Message Routing__.
    * Go to __Splitter__.
    * Choose __General Splitter__.

    ![Choose General Splitter](Choose General Splitter.png)

    * Drag it on to the execution pipeline between the __Router__ and __XML to JSON Converter__ step on the branch for __KWA2MNWC2U__.
    Name it __Split Orders__.

    ![Add General Splitter](Add General Splitter.png)

    * Configure the __General Splitter__ as follows:

    | Field   | Value    |
    | :------------- | :------------- |
    | Expression Type       | XPATH       |    
    | `XPath` Expression       | /orders/order/items/item       |
    | Grouping      | 1    |
    | Streaming       | Checked       |
    | Parallel Processing       | Unchecked       |
    | Stop on Exception       | Checked       |


    * Save, deploy and execute your flow. You should get one 3 individual mails containing individual product information:

    ![Separate Mail](Separate Mail.png)

    > Play around with the different types of splitters  and different configuration, to understand how the results differ.

[VALIDATE_1]

[ACCORDION-END]


---
