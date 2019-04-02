---
title: Maintain Platform Subaccounts and SAP Web IDE Permissions
description: Add new subaccount to the SAP Cloud Platform global account and give permission to access SAP Web IDE to everyone.
primary_tag: topic>abap-development
tags: [  tutorial>beginner, topic>abap-development, topic>abap-extensibility  ]
time: 15
---

## Prerequisites   
  - You need access to a subaccount on SAP Cloud Platform in the Neo environment.

## Details
### You will learn
- How to add a new subaccount
- How to give members the permission to enter SAP Web IDE

This tutorial describes how to add new subaccounts to SAP Cloud Platform. Furthermore you learn how to give members the permission to enter SAP Web IDE.

---

[ACCORDION-BEGIN [Step 1: ](Enter SAP Cloud Platform global account)]
Enter SAP Cloud Platform and select your global account.

![Enter SAP Cloud Platform subaccount](sapcp.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create new subaccount)]
Click **New Subaccount**.

![Create new subaccount](members.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add display name to subaccount)]
Add a display name to your new subaccount. Click **Save**.

![Add display name to subaccount](newsubaccount.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Check created subaccount)]
`New_Subaccount` will be shown.

![Check created subaccount](subaccount.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Search for SAP Web IDE)]
Go to **Services**, search for **SAP Web IDE** and select it.

![Search for SAP Web IDE](webide.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Configure your service)]
Click **Configure Service** on SAP Cloud Platform to configure your service.

![Configure your service](configure.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Give SAP Web IDE access to everyone)]
You are able to change the **Application Permissions**. Change the assigned role from your **`WebIDEPermission`** to **Everyone**. Save your changes.

![Give SAP Web IDE access to everyone](permission.png)

Now you have the permission to enter SAP Web IDE.

[ACCORDION-END]
