---
title: Discover Sales Order API from S/4HANA Cloud System
description: Discover the Sales Order API from S/4HANA Cloud system.
auto_validation: true
time: 20
tags: [ tutorial>intermediate,  products>sap-business-technology-platform]
primary_tag: products>sap-integration-suite
---

## Prerequisites

## Details
### You will learn
  - How to discover APIs from SAP API Business Hub
  - How to create API proxy
  - How to tryout APIs in API Management
  - How to get the API key

[ACCORDION-BEGIN [Step 1: ](Discover S/4HANA APIs in API portal)]

1. From trial cockpit, click **Access API Portal** link to launch API portal.

    ![Access API Portal](01-access-api-portal.png)

2. From API Portal expand menu, click **Discover** to connect to API catalog of SAP API Business Hub.

    ![Navigate Discover](02-navigate-discover.png)
    The API Discovery menu will open with many API packages available for consumption.

3. Click the **SAP S/4HANA Cloud** package to discover available APIs.

    ![Open Package](03-s4hanan-package.png)

4. Click the **Artifacts** tab to view the available APIs.

    ![Navigate Artifacts](04-artifacts-tab.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create API proxy)]

1. Click the Search Bar and type **sales order** to search for sales order API.

2. Find the **Sales Order – Create, Read, Update, Delete (A2X)** API and under **Actions** select **Copy**.

    ![Search Sales Order](05-search-sales-order.png)

3. From the copy wizard, select sandbox URL and click **OK**.

    ![Copy API](06-copy.png)

    You will be returned to the API Develop area.

4. For **`API_SALES_ORDER_SRV`** click **Deploy**.

    ![Deploy API](07-Deploy-proxy.png)

5. Open the created API proxy for example **`API_SALES_ORDER_SRV`**

    API Proxy has all the documentation  imported automatically.

    ![Open API](08-open-api.png)

7. Click **Resources** tab to see the Open API spec (Swagger) documentation that will be published to developers.

    ![Resources Tab](09-Resources.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Try out APIs)]

1. Expand the open API specification definition to see additional documentation and **Try out** feature which can help developers get started on the API more quickly.

    Click **Try out**

    ![Try out](10-try-out.png)

2. Give **$top** as 3.

    ![Top Records](11-top-records.png)

3. Click **Execute**.

    ![Execute API](12-execute.png)
    >The call will fail as it requires **Verify API Key** policy.

    >The Verify API Key policy lets you enforce verification of API keys at runtime, letting only apps with approved API keys access your APIs. This policy ensures that API keys are valid, have not been revoked, and are approved to consume the specific resources associated with your API products.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Get API Key)]

1. Go to [SAP API Business Hub](https://api.sap.com), to get your API Key.

    Click **Log On** button.

    ![API Hub](13-api-hub.png)

2. Click API package for example **SAP S/4HANA Cloud**.

    ![API Package](14-select-package.png)

3. Click **Sales Order** API to open the API details.

    ![API Details](15-select-api.png)

4. Click **Show API Key** button.

    ![API Key](16-show-api-key.png)

5. Click **Copy Key and Close button**.

    Save API Key value somewhere for later, e.g. Notepad or Notes, etc.

    ![Copy API Key](17-copy-api-key.png)

[VALIDATE_4]

[ACCORDION-END]


---
