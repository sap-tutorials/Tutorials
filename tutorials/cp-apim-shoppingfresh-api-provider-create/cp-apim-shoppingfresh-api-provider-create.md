---
title: Create an API Provider System to Access Gateway Service
description: Retrieve structured data set from SAP Gateway system.
auto_validation: true
time: 15
tags: [ tutorial>intermediate, products>sap-api-management, products>sap-web-ide]
primary_tag: products>sap-api-management
---

## Prerequisites
  - **Tutorial** [Create an Account on the Gateway Demo System](gateway-demo-signup)

## Details
### You will learn
  - How to create an API provider that points to SAP Gateway demo system named ES5

---

[ACCORDION-BEGIN [Step 1: ](Navigate to API Providers)]

From the API portal home page, navigate to **Configure**, by default **API Providers** tab will be selected.

![Create Provider](01-navigateapiprovider.png)

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Create API provider)]

1. Click on **Create**, to create a new API provider system.

    ![Create Provider](02-create-provider.png)

2. Give the name in **Overview** tab as "ES5" and description.

    ![Provider Overview](03-provider-name.png)

3. Navigate to **Connection** tab, enter the following values:

    **Field** | **Value**
    ---- | ----
    Type |`Internet`
    Host |`sapes5.sapdevcenter.com`
    Port |`443`
    Use SSL |`Checked`

    ![Provider Connection](05-connection.png)

4. Navigate to **Catalog Service Settings**, enter the following values:

    **Field** | **Value**
    ---- | ----
    Service Collection URL |`/sap/opu/odata/IWFND/CATALOGSERVICE/ServiceCollection`
    Authentication Type |`Basic`
    User Name |`<Your user name of gateway system for eg ES5>`
    Password |`<Your password of gateway system for eg ES5>`
    TrustAll |`true`

    ![Catalog Settings](04-catalog-settings.png)

5. Once the required fields are configured, click on **Save**.

    ![Provider Save](06-save.png)

[VALIDATE_2]

[ACCORDION-END]
