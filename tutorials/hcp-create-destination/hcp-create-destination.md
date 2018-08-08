---
title: Create a Destination on SAP Cloud Platform
description: Create a Destination to allow SAP Cloud Platform to read/write data
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [ products>sap-cloud-platform, topic>cloud, topic>mobile, topic>odata, tutorial>beginner ]
---

## Prerequisites
- **Proficiency:** Beginner

## Details

### You will learn
Destinations are used for the outbound communication of your application to a remote system (which can be a cloud or on-premise system). You will create a destination by giving it a name, URL of the remote system or service, the authentication type, and some other configuration data.

The data source you will use in this tutorial series is called "Northwind", which is a publicly accessible OData source hosted by <http://www.odata.org>:

Northwind comes in several versions.  The tutorials currently use either V2 (<https://services.odata.org/V2/Northwind/Northwind.svc/>) or V3 (<https://services.odata.org/V3/Northwind/Northwind.svc/>).  To support both versions, and other versions that may be added later, you will create a generic connection to the Northwind service.  The exact path - and exact version - will be configured in later tutorials.

### Time to Complete
**5 Min.**

---


[ACCORDION-BEGIN [Step 1: ](Log into SAP Cloud Platform)]

Go to <https://account.hanatrial.ondemand.com> and log in to your SAP Cloud Platform cockpit.

![SAP Cloud Platform log in page](scp-trial-logon.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new destination)]

Select the **Destinations** tab on the left side, and then click on **New Destination** to open a new destination configuration form.

![Web IDE Destination tab](mob1-1_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Confirm data)]

Enter/confirm all fields in the Destination configuration section with the information below.

Field Name     | Value
:------------- | :-------------
Name           | `Northwind`
Type           | `HTTP`
Description    | `Northwind OData services`
URL            | `https://services.odata.org`
Proxy Type     | `Internet`
Authentication | `NoAuthentication`

> Do **not** use the entire path for the URL.  The URL should only be `https://services.odata.org`

When you specify a URL with the HTTPS scheme, a checkbox "Use default JDK truststore" will appear. Ensure that this is checked. 

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add properties)]


Add three **Additional Properties** by clicking on the **New Property** button once for each property.

Field Name       | Value
:--------------- | :-------------
`WebIDEEnabled`  | `true`
`WebIDESystem`   | `Northwind_Data`
`WebIDEUsage`    | `odata_gen`

![Completed SAP Cloud Platform destination](northwind-destination-details.png)


[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Save your destination)]

Click **Save**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check the Additional Property settings)]



[VALIDATE_6]
[ACCORDION-END]
