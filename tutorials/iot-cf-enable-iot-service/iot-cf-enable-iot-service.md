---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Enable SAP Cloud Platform Internet of Things for Cloud Foundry
description: Create a service instance and a service key to enable the Internet of Things service.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn
- How to enable the Internet of Things Service
- How to create a service instance
- How to create a service key
- How to delete a service instance

### Time to Complete
15 min

## Next Steps
- **Tutorials:** [Create User and Tenant](tutorials/iot-cf-create-user-tenant)

---

[ACCORDION-BEGIN [Step 1: ](Create a Service Instance)]

>Contact the SAP Sales team to purchase a license for the SAP Cloud Platform Internet of Things Service. You can reach the SAP Sales team at [https://cloudplatform.sap.com](https://cloudplatform.sap.com) → **Contact Us**.

1.  Log on to the [SAP Cloud Platform Cockpit](https://account.hana.ondemand.com) with your user credentials.

2.  If you work in an enterprise account, add quotas to the services you purchased in your subaccount to make them visible on the **Service Marketplace**.

    For more information, please refer to section [Configure Entitlements and Quotas for Subaccounts](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5ba357b4fa1e4de4b9fcc4ae771609da.html) in the SAP Cloud Platform documentation.

3.  Navigate to the space in which you want to create a service instance.

    For more information, please refer to section [Navigate to Orgs and Spaces](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5bf87353bf994819b8803e5910d8450f.html) in the SAP Cloud Platform documentation.

4.  Choose **Services** > **Service Marketplace**.

5.  Choose **Internet of Things**.

6.  Choose **Instances**.

7.  Choose **New Instance**.

    The system opens the **Create Instance** dialog.

8.  Choose **Next**.

9.  Optional:

    You can enable and disable the internal SQL (Internet of Things Service internal database) and the HTTP processing services with the following parameters. For more information, please refer to the [Internet of Things Message Processing](https://help.sap.com/viewer/a7172eb02bf54229add4664fff702676/Cloud/en-US) documentation.

    **Specify Parameters** for the internal SQL and HTTP processing services in the JSON format or by selecting a JSON file. If no parameters are specified, the internal SQL processing service is activated by default.

    ```JSON
        {
         "processingServices": [
          {
            "name": "sql"
          },
          {
            "name": "http",
            "properties": {
              "processing.http.url": "<endpoint_url>",
              "processing.http.headers": [{
                "name": "<http_header>",
                "value": "<http_header_value>"
              }]
            }
          }
         ]
        }   

    ```
10. Choose **Next**.

11. Choose **Next**.

12. Enter an **Instance Name** for your service instance.

13. Choose **Finish**.

    You get a notification that the creation of service instance is in progress.

14. To open the Internet of Things Service Cockpit of the service instance, you have created choose **Open Dashboard** in the **Actions** column.

    To log on to the Internet of Things Service Cockpit, first create a service key in the Internet of Things Service Cockpit. For more information, please refer to section **Create a Service Key** in this tutorial.

    >It may take some time after the creation process has finished before the Internet of Things Service Cockpit can be reached.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Service Key)]

You have created a service instance as described in the previous step. To log on to a service instance, you need to create a service key in the [SAP Cloud Platform Cockpit](https://account.hana.ondemand.com)

1.  Navigate to the space in which you want to create a service key.

2.  Choose **Services** > **Service Marketplace**.

3.  Choose **Internet of Things**.

4.  Choose **Instances**.

5.  Choose the **Name** of your service instance.

6.  Choose **Service Keys**.

7.  Choose **Create Service Key**.

    The system opens the **Create Service Key** dialog.

8.  Enter a **Name**.

9.  Choose **Save**.

    A new service key for your service instance is created. It contains the Internet of Things Service Cockpit `password`, `url`, and `username`.

    ```JSON
    {
"password": "r3B3KkyXEmuSyDF",
"instanceId": "80e91683-c952-4cef-ae88-37edb89410f7",
"cockpitUrl": "https://80e91683-c952-4cef-ae88-37edb89410f7.eu10.cp.iot.sap/iot/cockpit",
"username": "root"
}

    ```

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Delete a Service Instance)]

1.  Navigate to the space in which you want to delete the service instance.

2.  Choose **Services** > **Service Instances**.

3.  To delete a service instance choose the recycle bin icon in the **Actions** column.


[DONE]

[ACCORDION-END]
