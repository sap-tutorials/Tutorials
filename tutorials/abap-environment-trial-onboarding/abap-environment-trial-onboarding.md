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
 - You have created a trial account on SAP Cloud Platform:  [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).
 - You have created a cloud foundry trial account: [Create a Cloud Foundry Account](cp-cf-create-account).

## Details 
### You will learn  
  - How to create an trial user
  - How to create an ABAP Cloud project

---
[ACCORDION-BEGIN [Step 1: ](Configure SAP Cloud Platform Entitlements)]
  1. Open SAP Cloud Platform Trial Cockpit to logon to your existing Cloud Foundry trial account
     <https://cockpit.hanatrial.ondemand.com/>.

      ![Select ABAP Trial](logon.png)

  2. Select **Cloud Foundry Trial**.

      ![Select ABAP Trial](entitlement.png)


  3. Set **Europe (Frankfurt)** as your region and click **OK**, if you haven't done yet. Otherwise move on with step **1.6**.

      ![Select ABAP Trial](entitlement2.png)

  4. Now your Cloud Foundry Trial is ready to use. Click **Go to Space**.

      ![Select ABAP Trial](entitlement3.png)

  5. Result: You can see your dev space.

      ![Select ABAP Trial](entitlement4.png)

  6. Switch to your global account and select **Entitlements**.

      ![Select ABAP Trial](entitlement5.png)

  7. Select **Subaccount Assignments**.

      ![Select ABAP Trial](entitlement6.png)

  8. Select **trial** as subaccount and click **Go**.

      ![Select ABAP Trial](entitlement7.png)

  9. If you see all trial services, move on with step **2.1**.
     If you don't see any trial services, then click **Configure Entitlements**.

      ![Select ABAP Trial](entitlement9.png)

     Click **Add Service Plans**.

      ![Select ABAP Trial](entitlement10.png)

     Select **ABAP Trial**, check **shared** and click **Add 1 Service Plan**.

      ![Select ABAP Trial](entitlement11.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Select ABAP Trial)]

  1. Go to your space **dev**.

      ![Select ABAP Trial](account2.png)

  2. Click **Services**.

      ![Select ABAP Trial](account3.png)

  3. Click **Service Marketplace**.

      ![Select ABAP Trial](account4.png)

  4. Search for **ABAP Trial** and select it.

      ![Select ABAP Trial](trial.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create instance)]
  1. You can create a new instance on the SAP Cloud Platform ABAP Environment Trial. Therefore select **Instances**.

      ![Create instance](instance.png)

  2. Click **New Instance**.

      ![Create instance](instance2.png)

  3. Click **Next**.

      ![Create instance](instance3.png)

  4. Add your e-mail address:

      - "email": "example@email.com"

     Click **Next**.

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

[ACCORDION-BEGIN [Step 4: ](Create service key)]
  1. Click on your instance.

      ![Create service key](key.png)

  2. Select **Service Keys**.

      ![Create service key](key0.png)

  3. Click **Create Service Key**.

      ![Create service key](key2.png)

  3. Create a service key:

     - Name: ADT

     Click **Save**.

      ![Create service key](key3.png)

  4. Now your service key appears. Copy your service key for later use.

      ![Create service key](key4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open ABAP Development Tools )]
For TechEd users:

Click **>>** on your windows taskbar and select **SAP Development Tools** > **ABAP in Eclipse - `CAA361`**.  

![Create service definition](adt.png)

For other users:

Open your local ABAP Development Tools (ADT). You can download the latest version from <https://tools.hana.ondemand.com/#abap>.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Create ABAP cloud project)]
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


[ACCORDION-BEGIN [Step 7: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]
---
