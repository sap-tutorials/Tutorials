---
title: Discover and Manage Third-Party HERE Maps API from SAP API Business Hub     
description: Discover and create  an API proxy that will initiate a connection with HERE Maps and get the result coordinates.
auto_validation: true
time: 20
tags: [ tutorial>beginner, products>sap-cloud-platform, products>sap-api-management]
primary_tag: products>sap-api-management
---

## Details
### You will learn
  - How to discover HERE Location Suite API from SAP API Business Hub

---

[ACCORDION-BEGIN [Step 1: ](Discover HERE APIs)]

1. Navigate to **Discover** on API Portal and select **ALL**.

    !![Navigate Discover](01-Nav-Discover.png)

2. Enter the text `HERE` in the search box to filter the list of APIs shown.

    From the result list, click **HERE Location Services** .

    !![Navigate Discover](02_Search_HERE.png)

3. Click on **Artifacts** tab from the Package.

    Select the **Copy** action on the `HERE Geocoder API - Forward` line item.

    !![Copy HERE APIs](03-copy-api.png)

4. In the resulting dialog, review the populated parameters and click **OK**.

    ![Copy HERE APIs OK](04-copy-api-click-ok.png)

5. Select the newly created API and click on **Deploy**.

    !![Deploy HERE APIs](05-deploy-api.png)

    >Make a note of the API Proxy URL and the base path in a separate notepad (`/<yourtrialuser>/6.2`). You will need this for a later step.



6. Open the proxy again and click on **Resource** tab.

    Review the listed Resource ``/geocode.{format}``

    !![Deploy HERE APIs](06-resources.png)

    >As you expand the resource, you will notice that in order to test the API, one needs a real `app_id` and `app_code` parameter. You can register with HERE.com and request your own `app_id` and `app_code`.
    It is suggested you write down your `app_id` / `app_code` to a notepad or other easily accessible location for future use.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 1: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]


---
