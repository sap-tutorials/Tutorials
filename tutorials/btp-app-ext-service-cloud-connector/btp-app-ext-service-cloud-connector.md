---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Configure Systems in Cloud Connector
description: This tutorial shows you how to configure the Cloud Connector to connect your SAP S/4HANA system to SAP BTP.
keywords: cap
auto_validation: true
time: 10
tags: [tutorial>intermediate, tutorial>license, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-s-4hana]
primary_tag: software-product-function>sap-cloud-application-programming-model
---
## Prerequisites
 - [Prepare SAP S/4HANA System by Activating the Business Partner OData Service](btp-app-ext-service-odata-service)
- On SAP S/4HANA side:
    - You have a dedicated SAP S/4HANA system.
    - You must be an administrator of the SAP S/4HANA system.
 - On Cloud Connector side:
    - You have downloaded the right Cloud Connector version for your operating system from [SAP Development Tools](https://tools.hana.ondemand.com/#cloud) and have installed Cloud Connector on your machine in accordance with the [installation instructions](https://help.sap.com/viewer/cca91383641e40ffbe03bdc78f00f681/LATEST/en-US/57ae3d62f63440f7952e57bfcef948d3.html) on the SAP Help Portal.

## Details
### You will learn
- How to configure the Cloud Connector to connect your SAP S/4HANA system to SAP BTP
- How to add subaccount, system mapping and resource

---

[ACCORDION-BEGIN [Step 1: ](Configure system access in Cloud Connector)]
The Cloud Connector serves as a link between SAP BTP applications and on-premise systems. It combines an easy setup with a clear configuration of the systems that are exposed to the SAP BTP. At the same time, the cloud connector lets you use existing on-premise assets without exposing the entire internal landscape. See [Cloud Connector documentation](https://help.sap.com/viewer/cca91383641e40ffbe03bdc78f00f681/LATEST/en-US/e6c7616abb5710148cfcf3e75d96d596.html) for more details.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Add subaccount)]
1.    Open your [Cloud Connector administration UI](https://localhost:8443/).

    > - Adjust the port if you specified another one during the installation. You might have to use the external IP of your system.
    > - You can find detailed information on installing the Cloud Connector and logging in to the administration UI in [Install the Cloud Connector](https://developers.sap.com/tutorials/hana-cloud-mission-extend-08.html#10dcb97c-ab26-4ee1-973e-6f1f6638b079).


2.    Choose **Add Subaccount**.

    !![Add Subaccount](cloud-connector-1-1.png)

3.    Enter the required data in the **Add Subaccount** dialog and choose **Save**.

    !![Add Subaccount](cloud-connector-1.png)

    > - You can look up the required data in SAP BTP cockpit.
    > - For example, the field **Subaccount** requires the ID.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Add system mapping)]
1.    Choose **Cloud to On-Premise** on the left.

    !![Cloud to On-Premise](cloud-connector-1-2.png)

2.    Choose <kbd>+</kbd> on the right.

    !![System Mapping](cloud-connector-2.png)

3.    In the **Add System Mapping** dialog, select `ABAP System` from the dropdown list in the **Backend Type** field and choose **Next**.

    !![Select Backend Type](cloud-connector-3.png)

4.    Select `HTTPS` from the dropdown list in the **Protocol** field and choose **Next**.

    !![Select Protocol](cloud-connector-4.png)

5.    Enter your values for the fields **Internal Host** and **Internal Port**, and choose **Next**.

    !![Select Screen](cloud-connector-5a.png)

    > Tip for field values

    > - **Internal Host**: this is the internal IP of your SAP S/4HANA system.
    > - **Internal Port**: this is set to `44300` for HTTPS. However, you can look up the specific HTTPS port on your SAP S/4HANA system using the transaction `SMICM`.

6. Enter your values for the fields **Virtual Host** and **Virtual Port**, and choose **Next**.

    !![Select Screen](cloud-connector-5b.png)

    > Tip for field values

    > - It's up to you what values to put in the fields **Virtual Host** and **Virtual Port**. We've provided exemplary values for your convenience.

7.    Select `None` for **Principal Type** and choose **Next**.

8.    Select `Use Virtual Host` for **Host in Request Header** and choose **Next**.

    !![Select Host](cloud-connector-6.png)

9.    (Optional) Add a **Description** for your system mapping and choose **Next**.

    !![Add Description](cloud-connector-7.png)

10.    Review the values, enable **Check Internal Host**, and choose **Finish**.

    !![Check values](cloud-connector-8.png)

[VALIDATE_1]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Add resource)]
1. Make sure that your system is selected under section **Mapping Virtual to Internal System** and choose <kbd>+</kbd> under section **Resources Of <YOUR_VIRTUAL_HOST>** (in this example, `s4h-cpapp:443`).

    !![Button](cloud-connector-9.png)

2. In the **Add Resource** dialog, insert `/` for **URL Path**.

3. Set **Access Policy** to `Path and All Subpaths`.

4. Enter a description for the resource (optional).

5. Choose **Save**.

    !![Enter data](cloud-connector-10.png)

    The resource appears in the table.

    !![Added Resource](cloud-connector-11.png)

[DONE]
[ACCORDION-END]
---