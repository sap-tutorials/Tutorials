---
title: Google Cloud Launcher – Connect to a Tenant database from SAP HANA Studio
description: This how-to explains the steps to connect to a tenant database in SAP HANA, express edition, created in the Google Cloud Platform created from the Launcher
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, tutorial>how-to, products>sap-hana\,-express-edition ]
---
## Prerequisites  
 - [Create a HANA Express instance using the Google Cloud Launcher](https://developers.sap.com/tutorials/hxe-gcp-getting-started-launcher.html)
 - [Download and install Eclipse](https://developers.sap.com/tutorials/hxe-howto-eclipse.html)


## How-To Details
Provides instructions to complete the necessary configuration to connect to a tenant database from SAP HANA Studio. The tenant database was created in an SAP HANA, express edition, in the Google Cloud Platform.

### Time to Complete
**15 Min**.

---

[ACCORDION-BEGIN [Step 1: ](Open the Administration cockpit)]

Using a user with the proper permissions, such as SYSTEM, open the Administration cockpit by selecting the connection to SYSTEMDB and clicking on the **Administration** button.

![Click on administration](1.png)

Click on the **Configuration** tab and use the value `use_default_route` as a filter:

![Look for default route](2.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add a new key-value pair)]

Right-click on `public hostname resolution` and on **Add parameter**. Use `map_hxehost` as the key and your external IP as shown in the Compute Engine as the value.

![Add key-value pair](3.png)

Click on **Finish**.

This is what `public_hostname_resolution` should look like:

![Hostname mapping](4.png)


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add the connection in SAP HANA Studio)]
You can now proceed to connect to your tenant DB as a **New System**:

![Connect to a tenant DB](5.png)


[ACCORDION-END]
