---
title: Connect to HubSpot
description: Connect to HubSpot (an example of a non-SAP third-party application) via CRM Hub.
auto_validation: true
time: 30
tags: [ tutorial>beginner, products>sap-cloud-platform]
primary_tag: products>sap-api-management
---

## Prerequisites
- You should have your test or sandbox tenant in `HubSpot(CRM)` or any other third-party application.

## Details
### You will learn
  - How to get the integration or API key to access `HubSpot`
  - How to connect to a third-party application from SAP Cloud Platform Open Connectors

[ACCORDION-BEGIN [Step 1: ](Get integration key to access HubSpot)]

1. Log on to your `HubSpot` [tenant](https://www.hubspot.com/) by providing your login credentials.
    - Click **Setting** and then click **Integrations**.

    ![Navigate Settings](01-settings.png)

2. Expand the **Integrations** from menu item.

3. Select **API key** menu item and then click **Show**.

    >You will get the API Key that can be used to connect securely from SAP Cloud Platform Open Connectors.

    ![Get API Key](02-get-API-key.png)

3. Click **Copy** to copy the API key that can be used from SAP Cloud Platform Open Connectors.

    ![Copy API Key](03-copy-api-key.png)

    >If you have not created API Key already click on Create to create one that can be used to connect securely from SAP Cloud Platform Open Connectors

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Connect to third-party application)]

1. Enter your [SAP Cloud Platform trial](https://account.hanatrial.ondemand.com/).

    - Navigate to the **Cloud Foundry** environment by clicking on **Enter Your Trial Account** and click on **trial**.

    ![Logon CP Trial](00-Login-trial-account.png)

2. From the **Subscriptions** tab, search and select **Open Connectors** tile.

3. Click **Go to Application**.

    ![OCN Service Enable](05-OCN-Service-enable.png)

    >You are on SAP Cloud Platform Open Connectors home or landing page.

    ![OCN Landing Page](06-OCN-LandingPage.png)

3. Click **Connectors** tab to view all the available pre-built connectors.

    ![Click Connectors](07-Click-Connectors-tab.png)

4. In the search bar, enter **`CRM`** to view all connectors for third-party CRM Hub.

    ![HubSpot CRM](08-Hub-Spot-CRM.png)

    >In this exercise, `HubSpot` is used and therefore select the `HubSpot` CRM connectors. You can also explore and try out third-party connectors of your choice from the available connectors catalog.

5. Hover over the **HubSpot CRM** and select the option **Authenticate** to connect to your own HubSpot tenant.

    ![Authenticate](09-authenticate.png)

    >You are on **Create Authenticated Connector** page.

6.  Create an instance of the connector and set up authentication.

    - Enter **`myhubspot`** as the name for your authenticated connector.

    - Select `apikey` from the **Authentication Type** drop down.

    - Click **Show Optional Field** to enter your HubSpot API key.

    ![Provide API Key](10-provide-api-key.png)

    - Enter your HubSpot API key and then click **Create Instance**.

    ![Create Instance](11-create-instance.png)

    >This creates an authenticated connector instance to your own HubSpot tenant.

    >In case you have selected an OAuth authentication based flow, then click **Create Instance** and you are redirected to your third-party OAuth token provider for an OAuth authorization code flow.

8.  To try out the REST APIs from the specific third-party application, click **Test in the API Docs**.

    ![Test API Docs](12-Test-api-docs.png)

    >After the authenticated connection to your third-party application has been created successfully, you can test the connection from SAP Cloud Platform Open Connectors.

9. Select any of the available resources and click **Try it Out** to test the connectivity to third-party applications, and then click **Execute**.

    ![Try Out](13-try-out.png)

    >This shows the CURL command with the harmonized REST URIs to your third-party application and the normalized authentication token which can be used to securely and seamless connect to your third-party application.

    >The harmonized URIs and authentication headers can be used to connect securely to your third-party application via SAP Cloud Platform from any application that supports connecting to an HTTP endpoints.

    ![Curl Command](14-Curl-command.png)

[VALIDATE_2]
[ACCORDION-END]
