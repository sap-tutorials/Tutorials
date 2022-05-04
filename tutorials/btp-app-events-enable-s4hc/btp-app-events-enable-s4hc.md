---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Enable Events from SAP S/4HANA Cloud to SAP BTP
description: This tutorial shows you how to enable events to be sent from your SAP S/4HANA Cloud system to SAP BTP.
keywords: cap
auto_validation: true
time: 20
tags: [tutorial>intermediate, tutorial>license, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-api-management, software-product>sap-hana-cloud, software-product>sap-s-4hana-cloud]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Add the Consumption of an External Service to Your CAP Application](btp-app-ext-service-add-consumption)
 - [Consume the External Service in the UI of Your Application](btp-app-ext-service-consume-ui)
 - [Register Your SAP S/4HANA Cloud System](btp-app-ext-service-s4hc-register)
 - [Use Your SAP S/4HANA Cloud Service for Your Deployed CAP Application](btp-app-ext-service-s4hc-use)
 - [Set Up Your CAP Application for Eventing](btp-app-events-app-setup-s4hc)
 - When starting with the result from the [`events-app-setup-s4hc`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/events-app-setup-s4hc) branch, refer to [Create a Directory for Development](btp-app-create-directory) to see a recommended approach for organizing directories and details how to copy the content of the branch.
 - On SAP BTP side:
    - You have an [enterprise](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/171511cc425c4e079d0684936486eee6.html) global account in SAP BTP.
    - You must be an administrator of the SAP BTP global account where you want to register your SAP S/4HANA Cloud system.
    - You need to [Prepare for SAP BTP Development](btp-app-prepare-btp) if you start with the result from an example branch.
    - Your SAP BTP subaccount has quota for the services `SAP Launchpad service` and `SAP HTML5 Applications Repository service` as described in [Prepare for SAP BTP Development](btp-app-prepare-btp).
    - You have to [Use an existing SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#42a0e8d7-8593-48f1-9a0e-67ef7ee4df18) or [Set up a new SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#3b20e31c-e9eb-44f7-98ed-ceabfd9e586e) for the deployment. After the deployment, you need to perform step 14-17, starting with step [Subscribe to SAP Launchpad Service](https://developers.sap.com/de/tutorials/btp-app-launchpad-service.html#57352c79-1a09-4054-a77d-626fac957404) from the tutorial [Add the SAP Launchpad service](btp-app-launchpad-service).
 - On SAP S/4HANA Cloud side:
    - You have a dedicated SAP S/4HANA Cloud tenant.
    - You must be an administrator of the SAP S/4HANA Cloud system.
    - You need to connect this system to your SAP BTP global account, if you'd like to build extension applications for your SAP S/4HANA Cloud system.


## Details
### You will learn
 - How to add an event topic to your event channel

---

[ACCORDION-BEGIN [Step 1: ](Add the Business Partner topic to your event channel)]
1. Log on to your SAP S/4HANA Cloud system.

2. Go to **Enterprise Event Enablement**.

      !![s4h12](s4h12.png)

      If you can't find **Enterprise Event Enablement**, you can also use the **Search** field:
         !![Search for Enterprise Event Enablement](s4h12_1.png)

3. To start, you have to set relevant filters. Open the value help for the **Channel** field.

      !![Channel Value Help](s4h12_2.png)

4. In the dialog **Define Conditions: Channel**, add a `RISK` condition and choose **OK**.

      !![Risk Condition](s4h12_3.png)

5. Choose **Go**. You will see the `SAP_CF_XF_RISK` channel in the filtered list.

      !![s4h13](s4h13.png)

4. Choose the channel and then choose **Create** in section **Outbound Topics**.

      !![s4h14](s4h14.png)

5. Open the value help of the **Topic** field.

      !![s4h15](s4h15.png)

6. Select the topic `sap/s4/beh/businesspartner/v1/BusinessPartner/*`.

      !![s4h16](s4h16.png)

7. Choose **Create**.

      !![s4h17](s4h17.png)

8. Once creation is done, the selected topic will be visible in the channel.

      !![s4h18](s4h18.png)

> You can also bind multiple topics for the same active channel.

[VALIDATE_1]
[ACCORDION-END]
---
