---
auto_validation: true
title: Create an SAP Cloud Platform ABAP Environment Trial User
description: Create an trial user and ABAP cloud project with SAP Cloud Platform ABAP environment.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 5
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
 - You have created a trial account on SAP Cloud Platform (https://developers.sap.com/tutorials/hcp-create-trial-account.html).
 - You have created a cloud foundry trial account (https://developers.sap.com/tutorials/cp-cf-create-account.html).

## Details
### You will learn  
  - How to create an trial user
  - How to create an ABAP Cloud project

---
[ACCORDION-BEGIN [Step 1: ](Select ABAP Trial)]
  1. Open SAP Cloud Platform Trial Cockpit to logon to your existing Cloud Foundry trial account.
     (https://account.hanatrial.ondemand.com/#/home/welcome)

     ![Select ABAP Trial](account.png)

  2. Go to your space dev.

      ![Select ABAP Trial](account2.png)

  3. Click **Services**.

      ![Select ABAP Trial](account3.png)

  4. Click **Service Marketplace**.

      ![Select ABAP Trial](account4.png)
 
  5. Search for **ABAP Trial** and select it.

      ![Select ABAP Trial](account5.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create instance)]
  1. You can create a new instance on the SAP Cloud Platform ABAP Environment Trial. Therefore select **Instances**.

      ![Create instance](instance.png)

  2. Click **New Instance**.

      ![Create instance](instance2.png)

  3. Click **Next**.

      ![Create instance](instance3.png)

  4. Add your e-mail address and click **Next**.

      ![Create instance](instance4.png)

  5. Click **Next**.

      ![Create instance](instance5.png)

  6. Create an instance:

     - Instance Name: `<your_name>`

     Click **Finish**.

      ![Create instance](instance6.png)

  7. Now your instance appears on the instance overview.

      ![Create instance](instance7.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create service key)]
  1. Click on your instance.

      ![Create service key](key.png)

  2. Click **Create Service Key**.

      ![Create service key](key2.png)

  3. Create a service key:

     - Name: ADT

     Click **Save**.

      ![Create service key](key3.png)

  4. Now your service key appears. Copy your service key for later use.

      ![Create service key](key4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open ABAP Development Tools )]
For TechEd users:

Click **>>** on your windows taskbar and select **SAP Development Tools** > **ABAP in Eclipse - `CAA361`**.  

![Create service definition](adt.png)

For other users:

Open your local ABAP Development Tools (ADT). You can download the latest version from (https://tools.hana.ondemand.com/#abap).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create ABAP cloud project)]
  1. Select **File** > **New** > **ABAP Cloud Project**.

      ![Create ABAP cloud project](project.png)

  2. Select **Service Key** and click **Next >**.

      ![Create ABAP cloud project](project2.png)

  3. Paste your service key and click **Next >**.

      ![Create ABAP cloud project](project3.png)

  4. Logon to your ABAP trial account with your e-mail address and password.

      ![Create ABAP cloud project](project4.png)

  5. Click Finish.

      ![Create ABAP cloud project](project5.png)

  6. Your trial system appears on the project explorer.

      ![Create ABAP cloud project](project6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
