---
title: Add an API Proxy to a Product
description: In SAP Cloud Platform, API Management, API Proxies are grouped and exposed as so called Products. In this tutorial you will create a new product and assign the previously created API Proxy to it
primary_tag: products>sap-api-management
auto_validation: true
tags: [  tutorial>beginner, topic>cloud, products>sap-api-management, products>sap-cloud-platform ]
---
## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Create an API Proxy](https://developers.sap.com/tutorials/hcp-apim-create-api.html)

## Next Steps
- [Protect your API Proxy by adding an Application Key Verification](https://developers.sap.com/tutorials/hcp-apim-verify-api.html)


## Details
### You will learn  
SAP Cloud Platform, API Management uses three main components to expose APIs.
- The API Provider is used to abstract the connection to the backend / target system
- The API Proxy is the actual API which contains the logic to connect to the target system. Here you can model the flow, add security policies, transform the incoming message or look for content injections
- The API Product which bundles one or more API Proxies before they are exposed in the API Developer portal so they can be consumed by a developer

In SAP Cloud Platform, API Management, API Proxies are grouped and exposed as so called Products. In this tutorial you will create a new product and assign the previously created API Proxy to it

### Time to Complete
**15 Min**.

---


[ACCORDION-BEGIN [Step 1: ](Open the SAP API Management API Portal)]

Open the SAP API Management API Portal (you can get the URL from Enable the SAP Cloud Platform, API Management Service).

![Access API Portal](01-access_api_portal.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Quick start a product creation)]

From the Home screen,  click on **Product** under the Quick Actions tile.

![Click on Product](02-create_product.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](View current products)]

To access the list of Products, select the **Hamburger Menu** in the upper left corner and click on **Develop**

![Click on Develop](03-manage.png)

Select **Products** from the tab menu. This will bring up the list of previously created products.

![Click on Product](04-manage-product.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a new product)]

On the **Products** tab, click on **Create** to start the new product wizard.

![Click on Create](05-ProductCreate.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add name and title)]

On the Overview page, Enter the values for *Name* and *Title*.

**Field** | **Value**
---- | ----
Name |`ProductForFirstAPIProxy`
Title | Product For First API Proxy

![Product overview](05a.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Select the API Proxy)]

In the tab menu, select **API**. This is were you can add and remove APIs and API Proxies from the product.

Click the **Add** button.

![APIs for product](06-AddAPI.png)

The list of API Proxies is populated from the APIs you have created. Select the API Proxy `GWSAMPLE_BASIC` that you created in the previous tutorial. Click **OK**.

![Click on OK](07-SelectAPI-OK.png)

[VALIDATE_6]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Click Publish)]

Click on **Publish**.

![Click on Publish](08-Publish.png)

One Product is now available.

![One Product is available](09-ProductPublished.png)

[DONE]
[ACCORDION-END]


