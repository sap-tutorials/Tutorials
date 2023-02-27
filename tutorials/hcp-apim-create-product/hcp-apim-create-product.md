---
parser: v2
primary_tag: products>sap-integration-suite
auto_validation: true
tags: [  tutorial>beginner, topic>cloud, products>sap-api-management ]
---
# Add an API Proxy to a Product
<!-- description --> In SAP Integration Suite, API Management, API Proxies are grouped and exposed as so called Products. In this tutorial you will create a new product and assign the previously created API Proxy to it

## Prerequisites  
- **Proficiency:** Beginner
- **Tutorials:** [Create an API Proxy](hcp-apim-create-api)

## Next Steps
- **Tutorials:** [Protect your API Proxy by adding an Application Key Verification](hcp-apim-verify-api)


## You will learn  
SAP Integration Suite, API Management uses three main components to expose APIs.
- The API Provider is used to abstract the connection to the backend / target system
- The API Proxy is the actual API which contains the logic to connect to the target system. Here you can model the flow, add security policies, transform the incoming message or look for content injections
- The API Product which bundles one or more API Proxies before they are exposed in the API Developer portal so they can be consumed by a developer

## Intro
In SAP Integration Suite, API Management, API Proxies are grouped and exposed as so called Products. In this tutorial you will create a new product and assign the previously created API Proxy to it
## Time to Complete
**15 Min**.

---


### Open the SAP API Management API Portal


Open the **API Management, API Portal** (you can get the URL from Integration Suite Launchpad, click on Design Develop, and Manage APIs).

![Open SAP API Management API Portal](01-access_api_portal_cf.png)


### Quick start a product creation


From the Home screen,  click on **Product** under the Quick Actions tile.

![Click on Product](02-create_product-cf.png)


### View current products


To access the list of Products, select the **Hamburger Menu** in the upper left corner and click on **Develop**

![Click on Develop](03-manage-cf.png)

Select **Products** from the tab menu. This will bring up the list of previously created products.

![Click on Product](04-manage-product-cf.png)


### Create a new product


On the **Products** tab, click on **Create** to start the new product wizard.

![Click on Create](05-ProductCreate-cf.png)


### Add name and title


On the Overview page, Enter the values for *Name* and *Title*.

**Field** | **Value**
---- | ----
Name |`ProductForFirstAPIProxy`
Title | Product For First API Proxy

![Product overview](05a-cf.png)


### Select the API Proxy


In the tab menu, select **API**. This is were you can add and remove APIs and API Proxies from the product.

Click the **Add** button.

![APIs for product](06-AddAPI-cf.png)

The list of API Proxies is populated from the APIs you have created. Select the API Proxy `GWSAMPLE_BASIC` that you created in the previous tutorial. Click **OK**.

![Click on OK](07-SelectAPI-OK-cf.png)



### Click Publish


Click on **Publish**.

![Click on Publish](08-Publish-cf.png)

One Product is now available.

![One Product is available](09-ProductPublished-cf.png)

