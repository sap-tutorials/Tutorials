---
title: Create an SAP Cloud Platform destination pointing to an SAP Gateway
description: Create a new destination pointing to the SAP Gateway Demo System
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, topic>cloud, products>sap-gateway, products>sap-cloud-platform  ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  
You will create a destination (a server end-point where SAP Cloud Platform can send or receive data) that points to an SAP Gateway OData service.

### Time to Complete
**5 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Log into your SAP Cloud Platform account)]
Log into [SAP Cloud Platform](https://account.hanatrial.ondemand.com) by opening the following URL in a new tab: <https://account.hanatrial.ondemand.com>

Click on the ***Neo Trial*** tile to open the cockpit for the Neo environment.

![Click on Neo Trial](hcp-regions.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Go to Destinations)]

In the left-hand navigation bar, select **Connectivity > Destinations** then click on **New Destination**.

![open destinations](te-2016-3-02.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enter Destination details)]

Enter the field values as listed below:

Field Name     | Value
:------------- | :-------------
Name           | `SAP_Gateway`
Type           | `HTTP`
Description    | `SAP Gateway ES4`
URL            | `https://sapes4.sapdevcenter.com`
Proxy Type     | `Internet`
Authentication | `BasicAuthentication`
User           | `<Your ES4 Gateway user> (e.g. p123456789)`
Password       | `<Your ES4 Gateway password>`

Leave the **Use default JDK truststore** checked.

And add the following **Additional Properties** fields by clicking on the **New Property** button once for each property.

Field Name       | Value
:--------------- | :-------------
`WebIDEEnabled`  | `true`
`WebIDESystem`   | `gateway`
`WebIDEUsage`    | `odata_abap`

Then click **Save**.

![creating destination](te-2016-3-03.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Save your destination)]

Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Check the Additional Property settings)]

Select the value of the **`WebIDEUsage`** parameter in your destination from the list below and click **Validate**:

[VALIDATE_5]
[ACCORDION-END]
