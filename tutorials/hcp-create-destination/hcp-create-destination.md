---
title: Create a Destination on HANA Cloud Platform
description: Create a Destination to allow HANA Cloud Platform to read/write data
tags: [tutorial:product/hcp, tutorial:product/mobile, tutorial:interest/gettingstarted]
---

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** [Sign up for an account on HANA Cloud Platform](http://go.sap.com/developer/tutorials/create-hcp-trial-account.html)

## Next Steps
[Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/build-mobile-web-app-from-template.html)

## Details

### You will learn
Destinations are used for the outbound communication of your application to a remote system (which can be a cloud or on-premise system). You will create a destination by giving it a name, URL of the remote system or service, the authentication type, and some other configuration data.

The data source you will use in this tutorial series is called "Northwind", which is a publicly accessible OData source hosted by ```www.odata.org```:

The data source URL is <http://services.odata.org/V2/Northwind/Northwind.svc/>

### Time to Complete
**< 5 min**

1. Go to <https://account.hanatrial.ondemand.com> and log in to your HCP cockpit.

 ![mob1-1_1.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-create-destination/mob1-1_1.png)

2. Select the **Destinations** tab on the left side, and then click on **New Destination…** to open a new destination configuration form.

 ![mob1-1_2.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-create-destination/mob1-1_2.png)

3. Enter/confirm all fields in the Destination configuration section with the information below.

 **Field Name: Value**

 - **Name:** `Northwind`
 - **Type:** `HTTP`
 - **Description:** `Northwind OData Service`
 - **URL:** `http://services.odata.org`
 - **Proxy Type:** `Internet`
 - **Authentication:** `NoAuthentication`

 Add three **Additional Properties** fields by clicking on the **New Property** button once for each property.


 **Field Name: Value**

 - **`WebIDEEnabled`:** `true`
 - **`WebIDESystem`:** `Northwind_Data`
 - **`WebIDEUsage`:** `odata_gen`

 ![mob1-1_3.png](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-create-destination/mob1-1_3.png)

4. Click **Save**, and that's it. Continue with the tutorial below to build your first app.

## Next Steps
[Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/build-mobile-web-app-from-template.html)
