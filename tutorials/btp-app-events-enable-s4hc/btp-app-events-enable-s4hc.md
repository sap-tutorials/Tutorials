---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Enable Events from SAP S/4HANA Cloud to SAP BTP
description: This tutorial shows you how to enable events to be sent from your SAP S/4HANA Cloud system to SAP BTP.
keywords: cap
auto_validation: true
time: 20
tags: [tutorial>intermediate, tutorial>license, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-api-management, software-product>sap-hana-cloud, software-product>sap-s-4hana-cloud]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Set Up Your CAP Application for Eventing](btp-app-events-app-setup-s4hc)


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