---
title: Enable SAP Cloud Platform Internet of Things for Cloud Foundry
description: Create a service instance and a service key to enable the Internet of Things service.
auto_validation: true
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things,topic>internet-of-things, topic>cloud ]
---

<!-- loio86b94ea5deab476d92d377cf988d046e -->

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
- **Tutorials:** [Create User and Tenant](https://developers.sap.com/tutorials/iot-cf-create-user-tenant.html)

---

[ACCORDION-BEGIN [Step 1: ](Create a Service Instance)]

> Note:
>  Contact the SAP Sales team to purchase a license for the SAP Cloud Platform Internet of Things Service. You can reach the SAP Sales team at [https://cloudplatform.sap.com](https://cloudplatform.sap.com) → *Contact Us*.

1.  Log on to the [SAP Cloud Platform Cockpit](https://account.hana.ondemand.com) with your user credentials.

2.  If you work in an enterprise account, add quotas to the services you purchased in your subaccount to make them visible on the *Service Marketplace*.

    For more information, please refer to section [Add Quotas to Subaccounts Using the Cockpit](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5ba357b4fa1e4de4b9fcc4ae771609da.html) in the SAP Cloud Platform documentation.

3.  Navigate to the space in which you want to create a service instance.

    For more information, please refer to section [Navigate to Orgs and Spaces in the Cockpit](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5bf87353bf994819b8803e5910d8450f.html) in the SAP Cloud Platform documentation.

4.  Choose *Services* > *Service Marketplace*.

5.  Choose *Internet of Things*.

6.  Choose *Instances*.

7.  Choose *New Instance*. The system opens the *Create Instance* dialog.

8.  Choose *Next*.

9.  Optional:

    You can enable and disable the SQL and HTTP processing services with the following parameters. For more information, please refer to the [Internet of Things Message Processing](https://help.sap.com/viewer/a7172eb02bf54229add4664fff702676/Cloud/en-US) documentation.

    *Specify Parameters* for the SQL and HTTP processing services in the JSON format or by selecting a JSON file. If no parameters are specified, the SQL processing service is activated by default.

    ```bash
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

    > Note:
     Updating parameters for processing services is possible through the Cloud Foundry Command Line Interface (cf CLI). For more information, please refer to section [update-service](https://cli.cloudfoundry.org/en-US/cf/update-service.html) in the Cloud Foundry CLI Reference Guide.

    Choose *Next*.

    > Note:
     An [SAP IoT Application Enablement](https://help.sap.com/viewer/p/SAP_IOT_APPLICATION_SERVICES) integration can be triggered by creating a ticket in the [SAP Support Portal](https://support.sap.com/en/index.html) using the component **BC-NEO-SVC-IOT**.

    > The integration needs to be triggered before modeling any entities in the Internet of Things Service. Please provide

    > - the *URL* of the targeted Internet of Things Service instance
    > - the *subaccount ID* of your SAP IoT Application Enablement tenant

    > within your ticket.
    >
    For more information, how to find your *subaccount ID*, please refer to section [Navigate to Global Accounts and Subaccounts](https://help.sap.com/viewer/e275296cbb1e4d5886fa38a2a2c78c06/Cloud/en-US/0874895f1f78459f9517da55a11ffebd.html) in the SAP Cloud Platform documentation.
    >

10. Choose *Next*.

11. Enter an *Instance Name* for your service instance.

12. Choose *Finish*.

    You get a notification that the creation of service instance is in progress.

13. To open the Internet of Things Service Cockpit of the service instance, you have created choose *Open Dashboard* in the *Actions* column.

    To log on to the Internet of Things Service Cockpit, first create a service key in the Internet of Things Service Cockpit. For more information, please refer to section *Create a Service Key* in this tutorial.


    > Note:
     It may take some time after the creation process has finished before the Internet of Things Service Cockpit can be reached.
    >

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Service Key)]

You have created a service instance as described in the previous step. To log on to a service instance, you need to create a service key in the [SAP Cloud Platform Cockpit](https://account.hana.ondemand.com)

1.  Navigate to the space in which you want to create a service key.

2.  Choose *Services* > *Service Marketplace*.

3.  Choose *Internet of Things*.

4.  Choose *Instances*.

5.  Choose the *Name* of your service instance.

6.  Choose *Service Keys*.

7.  Choose *Create Service Key*.

    The system opens the *Create Service Key* dialog.

8.  Enter a *Name* and choose *Save*.

    A new service key for your service instance is created. It contains the Internet of Things Service Cockpit `password`, `url`, and `username`.

    ```bash
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

2.  Choose *Services* > *Service Instances*.

3.  To delete a service instance choose the recycle bin icon in the *Actions* column.


[DONE]

[ACCORDION-END]
