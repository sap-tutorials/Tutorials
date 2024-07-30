---
parser: v2
auto_validation: true
primary_tag: products>sap-integration-suite
tags: [  tutorial>beginner, topic>cloud, programming-tool>odata, products>sap-integration-suite, products>sap-gateway ]
time: 15
---
# Create an API Proxy
<!-- description --> Learn how SAP Integration Suite, API Management can leverage the OData catalog service to retrieve relevant information from a SAP Gateway system and pre-populate required information for an API proxy.

## Prerequisites  
- **Tutorials:**  [Create an API Provider System](hcp-apim-create-provider)

## You will learn  
SAP Integration Suite, API Management uses three main components to expose APIs.
- The API Provider is used to abstract the connection to the backend / target system
- The API Proxy is the actual API which contains the logic to connect to the target system. Here you can model the flow, add security policies, transform the incoming message or look for content injections
- The API Product which bundles one or more API Proxies before they are exposed in the API Business Hub Enterprise so they can be consumed by a developer

## Intro
In this tutorial you will learn how to create an API Proxy based on the API Provider created in the previous step. You will learn how SAP Integration Suite, API Management can leverage the OData catalog service to retrieve relevant information from a SAP Gateway system and pre-populate required information for the API Proxy

---

### Learn about creating API proxy and Policy Editor


[Create an API proxy](https://blogs.sap.com/2016/06/22/part-6-overview-of-api-proxy-policies/) and take a look at the Policy Editor. You will start to look into the details of building an API Proxy that contains some real life functionality.



### Access the API Proxies


Open the **Integration Suite** and choose **Design Develop, and Manage APIs**.

![Open SAP API Management API Portal](01-access_api_portal_cf.png)



### View and create APIs

To view your APIs that you have previously created, select from the **Hamburger Menu** in the upper left corner, choose **Configure** and select **APIs**.

![Click on Develop](03-manage-cf.png)

This will open the list of previously created APIs.

To create a new API from this page, click **Create**.

![Click on Create](04-CreateAPI-cf.png)


### Select API provider


From the drop-down select the `SAPDeveloperSystemES5` API Provider created in the previous tutorial.

![Select Provider System](05-API_Provider-cf.png)


### Discover the services


Click on **Discover**.

![Click on Discover](06-Discover-cf.png)


### Select the services


Select the `GWSAMPLE_BASIC` services by using the search bar to search for _sample_.

> You can use the Search to narrow down the list.

![Select the GWSAMPLE_BASIC service](07-Sample-OK-cf.png)

Click on **OK**.

The remaining fields from the API Proxy creation screen are populated. Click **Create**.

> In the trial version, there is only 1 option for the Host Alias which is your default trial account. Leave this as is.

![Click on Create](08-Create-cf.png)


### Save your API


Click on **Save**.

![Click on Save](09-Save-cf.png)


### View APIs created


Status of the API Proxy is **Not Deployed** as we have not deployed the API

Click on the arrow next to **Create API** to go back to the overview page

![Go back to Overview](10-GoBackToOverview-cf.png)

A new API Proxy has been created

![API Proxy has been created](11-Overview-cf.png)


### Deploy Created API Proxy


Click on action button from right side and select **Deploy**

![Deploy Proxy](12-Deployproxy-cf.png)

API proxy is deployed.


### Test your API Proxy


To Test your API proxy, navigate to **Test** from navigation bar and select **APIs**.
![Navigate Test](13-Navtest-cf.png)

Select your API and provide the user name and password and click **OK**.

![Test Authentication](14-Testauth-cf.png)

Click on Send.

![Get Request](15-Send-cf.png)

You should get the response.

