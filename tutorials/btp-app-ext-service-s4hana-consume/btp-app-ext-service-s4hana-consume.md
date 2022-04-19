---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Add Services and Consume an External Service from SAP S/4HANA
description: This tutorial shows you how to prepare and deploy your CAP application and test it with SAP S/4HANA connectivity.
keywords: cap
auto_validation: true
time: 20
tags: [tutorial>intermediate, tutorial>license, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-s-4hana]
primary_tag: software-product-function>sap-cloud-application-programming-model
---
## Prerequisites
 - You have developed your CAP application using the tutorials [Add the Consumption of an External Service to Your CAP Application](btp-app-ext-service-add-consumption) and [Consume the External Service in the UI of Your Application](btp-app-ext-service-consume-ui) or download the result from this [branch](https://github.com/SAP-samples/cloud-cap-risk-management/tree/ext-service-consume-ui) to continue here.
 - [Prepare SAP S/4HANA System by Activating the Suitable OData Service](btp-app-ext-service-odata-service)
 - [Configure Systems in Cloud Connector](btp-app-ext-service-s4hana-consume)
 - On SAP BTP side:
    - You have an [enterprise](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/171511cc425c4e079d0684936486eee6.html) global account in SAP BTP.
    - You must be an administrator of the SAP BTP global account where you want to register your SAP S/4HANA system.
    - You need to [Prepare for SAP BTP Development](btp-app-prepare-btp) if you start with the result from an example branch.
    - Your SAP BTP subaccount has quota for the services `SAP Launchpad service` and `SAP HTML5 Applications Repository service` as described in [Prepare for SAP BTP Development](btp-app-prepare-btp).
    - You have to [Use an existing SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#42a0e8d7-8593-48f1-9a0e-67ef7ee4df18) or [Set up a new SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#3b20e31c-e9eb-44f7-98ed-ceabfd9e586e) for the deployment. After the deployment you need to perform steps 14-17 starting with step [Subscribe to SAP Launchpad Service](https://developers.sap.com/de/tutorials/btp-app-launchpad-service.html#57352c79-1a09-4054-a77d-626fac957404) from the tutorial [Add the SAP Launchpad service](btp-app-launchpad-service).
 - On SAP S/4HANA side:
    - You have a dedicated SAP S/4HANA system.
    - You must be an administrator of the SAP S/4HANA system.
 - On Cloud Connector side:
    - You have downloaded the right Cloud Connector version for your operating system from [SAP Development Tools](https://tools.hana.ondemand.com/#cloud) and have installed Cloud Connector on your machine in accordance with the [installation instructions](https://help.sap.com/viewer/cca91383641e40ffbe03bdc78f00f681/LATEST/en-US/57ae3d62f63440f7952e57bfcef948d3.html) on the SAP Help Portal.


## Details
### You will learn
 - How to prepare your CAP application
 - How to deploy your CAP application
 - How to test your CAP application with SAP S/4HANA connectivity


To start with this tutorial use the result in the [`ext-service-consume-ui`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/ext-service-consume-ui) branch.

---

[ACCORDION-BEGIN [Step 1: ](Add the Connectivity service)]
In this tutorial, you do the final steps to use the external service in the CAP application on SAP BTP. You create an on-premise destination with your technical user's name and password. By binding the Connectivity and the Destination services to your CAP service, it can access the service using the destination.

Add the following lines to your `mta.yaml` file:

<!-- snippet mta.yaml resources: cpapp-connectivity -->
```YAML[3-7]
resources:
   ...
 - name: cpapp-connectivity
   type: org.cloudfoundry.managed-service
   parameters:
     service: connectivity
     service-plan: lite
```

> The Destination service is already added to the `mta.yaml`. If you look at the resources that already exist in the `mta.yaml`, you'll see that the Destination service is already there, so you don't have to add it yourself.

```YAML[3-19]
resources:
    ...
  - name: cpapp-destination
  type: org.cloudfoundry.managed-service
  parameters:
    config:
      HTML5Runtime_enabled: true
      init_data:
        instance:
          destinations:
          - Authentication: NoAuthentication
            Name: ui5
            ProxyType: Internet
            Type: HTTP
            URL: https://ui5.sap.com
          existing_destinations_policy: update
      version: 1.0.0
    service: destination
    service-plan: lite
```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Bind the Connectivity and Destination services to your CAP service)]
Add the following lines to the `cpapp-srv` module in your `mta.yaml` file:

<!-- snippet mta.yaml modules: cpapp-srv -->
```YAML[12-13]
modules:
   ...
 - name: cpapp-srv
 # ------------------------------------------------------------
   type: nodejs
   path: gen/srv
   requires:
    # Resources extracted from CAP configuration
    - name: cpapp-db
    - name: cpapp-uaa
    - name: cpapp-logs
    - name: cpapp-connectivity
    - name: cpapp-destination
   provides:
    - name: srv-api      # required by consumers of CAP services (e.g. approuter)
      properties:
        srv-url: ${default-url}
 # -------------------- SIDECAR MODULE ------------------------
```


The Destination service lets you find the destination information that is required to access a remote service or system from your Cloud Foundry application.
To consume the external service from our cloud deployed application, we will create a Destination and a Destination Service.

[VALIDATE_1]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Create a Destination in the SAP BTP cockpit)]
1. Enter your **Global Account**. If you are using a trial account, choose **Go To Your Trial Account**.

2. Choose **Account Explorer**.

3. Choose your subaccount in the **Subaccounts** tab.

4. Choose **Connectivity** &rarr; **Destinations**.

    !![Destinations](destinations.png)

5. Choose **New Destination**.

    !![New Destination](new_destination.png)

6. Configure the following settings:

    | Property | Value |
    |--|--|
    | Name | cpapp-bupa
    | Type | HTTP
    | URL  | http://{Virtual host}:{Virtual port}
    | Proxy Type | On Premise
    | Authentication | `BasicAuthentication`
    | User | `CPAPP` (as defined in [Create a technical user](btp-app-#create-a-technical-user))
    | Password | `Welcome1` (as defined in [Create a technical user](btp-app-#create-a-technical-user))

    > Replace `{Virtual host}` and `{Virtual port}` in the URL with the values you defined in [Add system mapping](btp-app-#add-system-mapping).

    !![Configure Settings](configure_settings.png)

7. Choose **Save**.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Set Destination)]
To set the destination for `API_BUSINESS_PARTNER` service, add the following lines for productive profile to your `package.json` file:

```JSON[5-10]
    "API_BUSINESS_PARTNER": {
        ...
        "[sandbox]": {
        ...
        },
        "[production]": {
            "credentials": {
                "path": "/sap/opu/odata/sap/API_BUSINESS_PARTNER",
                "destination": "cpapp-bupa"
            }
        }
```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Deploy your CAP application)]
If you use the [SAP Continuous Integration and Delivery (CI/CD) service on SAP Business Technology Platform](btp-app-ci-cd-btp), you just need to push the commit to your **main** branch and wait for the deployment to be completed.

Otherwise, deploy your application as [Multi-Target Application (MTA)](btp-app-cap-mta-deployment).

1. Build the MTA module from your project root folder:

    ```Shell/Bash
    mbt build -t ./
    ```

    This creates a `mtar` file `cpapp_1.0.0.mtar` in the current folder (option: `-t ./`).

    > In case you get the error `On Windows, this driver only supports the x64 architecture. You node process is: ia32`, check if you have installed the right version of Node.js for your operating system.

2. Deploy the module to your current Cloud Foundry space:

    ```Shell/Bash
    cf deploy cpapp_1.0.0.mtar
    ```

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 6: ](Test your CAP application with SAP S/4HANA connectivity)]
When creating new entries in the **Risks** application, you should be able to use the value help for **Supplier** to see all the values from the remote SAP S/4HANA system.

> Don't forget to perform steps 14-17 from the tutorial [Add the SAP Launchpad service](btp-app-launchpad-service) before you continue with this step to be able to create entries in the **Risks** application.

1. Open your **SAP BTP Launchpad Service** site as described in [Add the SAP Launchpad Service](btp-app-launchpad-service).

2. Choose the **Risks** application.

3. Choose **Go**.

    You'll see a message `No data found. Try adjusting filter or search criteria.`

    > Why there's no data in the `Risks` application?

    > As explained in section [Exclude CSV files from deployment](btp-app-cap-mta-deployment), test files should never be deployed to an SAP HANA database as table data. For this reason, there's no test files in the deployment archive.

4. Choose **Create**.

    !![Create Risk](create_risk.png)

5. Open the value help for the **Supplier** field.

    !![Risk Object Page](risk_object_page.png)

6. You should see a list of suppliers.

    !![RiskTitle](suppliers-list.png)

Congratulations! You have completed all tutorials.


<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=btp-app-ext-service-s4hana-consume" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
<!--TAIL END-->

[DONE]
The result of this tutorial can be found in the [`ext-service-s4hana-consume`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/ext-service-s4hana-consume) branch.


[ACCORDION-END]
---
