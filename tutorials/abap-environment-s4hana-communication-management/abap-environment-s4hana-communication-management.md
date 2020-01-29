---
auto_validation: true
title: Connect to SAP S/4HANA Cloud system with SAP Cloud Platform ABAP Environment
description: Get to know how to connect your S/4HANA Cloud system with SAP Cloud Platform ABAP Environment.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform, tutorial>license ]
time: 5
---

## Prerequisites  
  - SAP Cloud Platform ABAP Environment user
  - ADT version 2.96 or higher

## Details
### You will learn
  - How to download trust
  - How to create a role for communication management

In this tutorial, wherever `XXX` appears, use a number (e.g. `000`).

---


[ACCORDION-BEGIN [Step 1: ](Open SAP Cloud Platform Cockpit)]
Open SAP Cloud Platform Cockpit and select your global account.

![Open SAP Cloud Platform Cockpit](cockpit.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open Subaccounts)]
Click **Subaccounts**.

![Open Subaccounts](subaccounts.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Select Subaccount)]
Select your Cloud Foundry subaccount.

![Select Subaccount](foundry.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Download Trust)]
Go to **Destinations** and click **Download Trust** to get the certification.

![Download Trust](trust.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open SAP S/4HANA Cloud system)]
Open SAP S/4HANA Cloud system as an administrator.

![Open SAP S/4HANA Cloud system](s4hana.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Select Maintain Business Roles)]
Go to Identity and Access Management and select the **Maintain Business Roles** tile.

![Select Maintain Business Roles](identity.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create new business role)]
Click **New** to create a new business role.

![Create new business role](new.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Define business role and business role ID)]

  1. Define your business role and your business role ID.

      ![Define business role and business role ID](role.png)

  2. Search `SAP_CORE_BC_COM`, select it, apply the change and click OK.

      ![Define business role and business role ID](add.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
