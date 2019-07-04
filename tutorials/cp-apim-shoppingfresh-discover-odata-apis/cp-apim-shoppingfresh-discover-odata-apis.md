---
title: Discover and Manage OData APIs from SAP Gateway
description: Discover an OData API to expose all the business partner resources from SAP Gateway system and manage them.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-api-management, products>sap-cloud-platform]
primary_tag: products>sap-api-management
---

## Details
### You will learn
 - How to discover OData APIs from backend Gateway system  

---

[ACCORDION-BEGIN [Step 1: ](Discover OData API from Gateway system)]

1. Navigate to **Develop** and click on **Create** to launch the wizard for new API creation.

    ![Navigate API](01-navigate_apitab.png)

    >Note that by default the API Provider would have been set to NONE

2. Select `ES5` and click on **Discover** to discover the services in this system.

    ![Select ES5](02-select-es5.png)

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create API proxy)]

1. In the search field, start typing `gwsample`, this will list `GWSAMPLE_BASIC`, the sample service from Gateway, select this entry.

    ![Select Odata Service](03-gwservice.png)

2. Enter the Name and Title of the API as provided below.

    **Field** | **Value**
    ---- | ----
    API Provider |`ES5`
    Name |`salesTrackerAPI`
    Title |`salesTrackerAPI`
    API Base Path |`salesTrackerAPI`

    ![Provider Details](04-provider-details.png)

3. Click on **Create** and go to next page.

    ![API Details](05-api-details.png)

4. Click on **Deploy** to finally create and deploy the API proxy.

    ![Deploy Proxy](06-save-deploy.png)

    >Inspect the API Proxy and note the API Proxy URL assigned.

[DONE]
[ACCORDION-END]

---
