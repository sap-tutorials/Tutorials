---
title: Create an API Proxy
description: Learn how SAP Cloud Platform, API Management can leverage the OData catalog service to retrieve relevant information from a SAP Gateway system and pre-populate required information for an API proxy.
auto_validation: true
primary_tag: products>sap-api-management
tags: [  tutorial>beginner, topic>cloud, topic>odata, products>sap-api-management, products>sap-cloud-platform, products>sap-gateway ]
time: 15
---
## Prerequisites  
- **Tutorials:**  [Create an API Provider System](https://developers.sap.com/tutorials/hcp-apim-create-provider.html)

## Details
### You will learn  
SAP Cloud Platform, API Management uses three main components to expose APIs.
- The API Provider is used to abstract the connection to the backend / target system
- The API Proxy is the actual API which contains the logic to connect to the target system. Here you can model the flow, add security policies, transform the incoming message or look for content injections
- The API Product which bundles one or more API Proxies before they are exposed in the API Developer portal so they can be consumed by a developer

In this tutorial you will learn how to create an API Proxy based on the API Provider created in the previous step. You will learn how SAP Cloud Platform, API Management can leverage the OData catalog service to retrieve relevant information from a SAP Gateway system and pre-populate required information for the API Proxy

---

[ACCORDION-BEGIN [Step 1: ](Learn about creating API proxy and Policy Editor)]

[Create an API proxy](https://blogs.sap.com/2016/06/22/part-6-overview-of-api-proxy-policies/) and take a look at the Policy Editor. You will start to look into the details of building an API Proxy that contains some real life functionality.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access the SAP API Management API Portal)]

Open the SAP API Management API Portal (you can get the URL from enabling the SAP Cloud Platform, API Management service).

![Access API Portal](01-access_api_portal.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Quick start an API creation)]

From the Home screen under the Quick Actions tile, click on **API**. This will bring up the Create API wizard.

![Click on API](02-API.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](View and create APIs)]
To view your APIs that you have previously created, select from the **Hamburger Menu** in the upper left corner and click **Develop**.

![Click on Develop](03-manage.png)

This will open the list of previously created APIs.

To create a new API from this page, click **Create**.

![Click on Create](04-CreateAPI.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Select API provider)]

From the drop-down select the `SAPDeveloperSystemES5` API Provider created in the previous tutorial.

![Select Provider System](05-API_Provider.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Discover the services)]

Click on **Discover**.

![Click on Discover](06-Discover.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Select the services)]

Select the `GWSAMPLE_BASIC` services by using the search bar to search for _sample_.

> You can use the Search to narrow down the list.

![Select the GWSAMPLE_BASIC service](07-Sample-OK.png)

Click on **OK**.

The remaining fields from the API Proxy creation screen are populated. Click **Create**.

> In the trial version, there is only 1 option for the Host Alias which is your default trial account. Leave this as is.

![Click on Create](08-Create.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Save and deploy your API)]

Click on **Save and Deploy**.

![Click on Save and Deploy](09-SaveAndDeploy.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](View APIs created)]

Click on the arrow next to **Create API** to go back to the overview page

![Go back to Overview](10-GoBackToOverview.png)

A new API Proxy has been created

![API Proxy has been created](11-Overview.png)

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Deploy Created API Proxy)]

Click on action button from right side and select **Deploy**

![Deploy Proxy](12-Deployproxy.png)

API proxy is deployed.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Test your API Proxy)]

To Test your API proxy, navigate to **Test** from navigation bar.
![Navigate Test](13-Navtest.png)

Select your API and provide the user name and password and click **OK**.

![Test Authentication](14-Testauth.png)

Click on Send.

![Get Request](15-Send.png)

You should get the response.

[DONE]

[ACCORDION-END]
