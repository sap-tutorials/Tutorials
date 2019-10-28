---
auto_validation: true
title: Create an SAP Cloud Platform ABAP Environment Trial User
description: Create a trial user and ABAP cloud project with SAP Cloud Platform ABAP environment.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 5
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
 - You have created a **trial account on SAP Cloud Platform**:  [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).
 - You have a **subaccount and dev space with Europe (Frankfurt) as region**.

## Details
### You will learn  
  - How to create a trial user
  - How to create an ABAP Cloud project

>You should have a subaccount on Cloud Foundry Trial with **Europe (Frankfurt)** as Region.

>**If you don't have a subaccount with this region**, please **create a subaccount and space with Europe (Frankfurt)** as region to make the SAP Cloud Platform ABAP Environment Trial visible as a service in your service marketplace.

>You can use following for the subaccount creation:
>![Select ABAP Trial](start.png)

>Enter a subaccount, for e.g. trial. Select **Cloud Foundry as Environment**, **Amazon Web Services (AWS) as Provider**, **Europe (Frankfurt)** as region and your **global account as subdomain**. Click **Create**.
>

---
[ACCORDION-BEGIN [Step 1: ](Select ABAP Trial)]
  1. Select **Enter Your Trial Account** to get to your trial account.

      ![Select ABAP Trial](welcome.png)
    If you aren't logged on to your SAP Cloud Platform Trial Cockpit, then please logon to <https://cockpit.hanatrial.ondemand.com/> and select **Enter Your Trial Account**.


  2. **You can see your trial subaccount with Europe (Frankfurt) as region**.

      Click **trial** to get to your trial subaccount.

      ![Select ABAP Trial](welcometrial.png)

  3. As we use Cloud Foundry every app and service is scoped to a space.
     Therefore choose in your trial subaccount your space **dev** to navigate to it.

      ![Select ABAP Trial](dev.png)


  4. Click **Service Marketplace** to get to the service overview, where you can select ABAP Trial as a service.

      ![Select ABAP Trial](account4.png)

  5. **If you see the ABAP Trial tile, then select it move on with step 3.1 - Create instance** to create your instance.

     **If you don't see the ABAP Trial tile, then move on with step 2.1 - Configure SAP Cloud Platform Entitlements** to configure your SAP Cloud Platform Entitlements.

      ![Select ABAP Trial](trial.png)

[DONE]
[ACCORDION-END]
 
[ACCORDION-BEGIN [Step 2: ](Configure SAP Cloud Platform Entitlements)]

  1. **Skip step 2 - Configure SAP Cloud Platform Entitlements**, if you could enter **ABAP Trial**.

     Otherwise configure your entitlements to make your ABAP Trial tile visible. Navigate back to your **trial** subaccount and select **Entitlements**.

      ![Select ABAP Trial](entitlement5.png)

  2. Click **Configure Entitlements** to add services to your subaccount.

      ![Select ABAP Trial](ent.png)

  3.  Click **Add Service Plans** to add ABAP Trial as a service.

      ![Select ABAP Trial](ent2.png)

  4.  Select **ABAP Trial**, check **shared** and click **Add 1 Service Plan**.

      ![Select ABAP Trial](entitlement11.png)

  5.  Click **Save**.

      ![Select ABAP Trial](ent3.png)

  6.  Now ABAP Trial is listed as a service.

      ![Select ABAP Trial](ent4.png)

  7.  Navigate to your space dev, select **Service Marketplace** and **ABAP Trial**.

      ![Select ABAP Trial](abaptrial.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create instance)]
  1. Create a new instance in your SAP Cloud Platform ABAP environment trial, therefore select **Instances**.

      ![Create instance](instance.png)

  2. Click **New Instance** to create a new instance.

      ![Create instance](instance2.png)

  3. Your service plan is shared, therefore click **Next**.

      ![Create instance](instance3.png)

  4. Add your registration e-mail address.

      - "email": "example@email.com"

     Click **Next**.

      ![Create instance](instance4.png)

  5.  You don't need to bind an application for your instance, therefore click **Next**.

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

  2. Select **Service Keys**. The service key is needed for the connection to the SAP Cloud Platform ABAP environment system in ADT.

      ![Create service key](key0.png)

  3. Click **Create Service Key** to create your service key.

      ![Create service key](key2.png)

  4. Create a service key:

     For example:

     - Name: ADT

     Click **Save**.

      ![Create service key](key3.png)

  5. Now your service key appears. Copy your service key for later use.
     The service key enables the connection to the SAP Cloud Platform ABAP environment system in ADT.

     ![Create service key](key4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Open ABAP Development Tools )]

Open your local ABAP Development Tools (ADT). You can download the latest version from <https://tools.hana.ondemand.com/#abap>.

![Create service definition](adt_logo.png)

For TechEd users:

Click **>>** on your windows taskbar and select **SAP Development Tools** > **ABAP in Eclipse - `CAA361`**.  

![Create service definition](adt.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Create ABAP cloud project)]
  1. Select **File** > **New** > **Other** > **ABAP Cloud Project** and click **Next >**.

      ![Create ABAP cloud project](cloud.png)

  2. Select **Service Key** and click **Next >**.

      ![Create ABAP cloud project](project2.png)

  3. Paste your service key you've copied earlier from the SAP Cloud Platform Trial Cockpit and click **Next >**.

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
