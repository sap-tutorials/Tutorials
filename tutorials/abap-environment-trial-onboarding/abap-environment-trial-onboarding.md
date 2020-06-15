---
auto_validation: true
title: Create an SAP Cloud Platform ABAP Environment Trial User
description: Create a trial user and ABAP cloud project with SAP Cloud Platform ABAP environment.
primary_tag: products>sap-cloud-platform--abap-environment
tags: [  tutorial>beginner, topic>abap-development, products>sap-cloud-platform ]
time: 15
author_name: Merve Temel
author_profile: https://github.com/mervey45
---

## Prerequisites  
 - You have created a **trial account on SAP Cloud Platform**:  [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account).
 - You have a **subaccount and dev space with Europe (Frankfurt) or US East (VA) as region**.

## Details
### You will learn  
  - How to create a trial user
  - How to create an ABAP Cloud project

>You should have a subaccount on Cloud Foundry Trial with **Europe (Frankfurt)** or **US East (VA)** as Region.

---

[ACCORDION-BEGIN [Step 1: ](Enter your trial account)]

1. In your web browser, open the [SAP Cloud Platform trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to the trial global account by clicking **Enter Your Trial Account**.

    !![Trial global account](01_Foundation20Onboarding_Home.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region (select the region closest to you). Your user profile will be set up for you automatically.  

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    >![Account setup](02_Foundation20Onboarding_Processing.png)

3. From your global account page, choose the **`trial`** tile to access your subaccount. The region is either **Europe (Frankfurt)** or **US East (VA)**.

    ![Select ABAP Trial](welcometrial.png)

4. As we use Cloud Foundry every app and service is scoped to a space.
   Therefore choose in your trial subaccount your space **dev** to navigate to it.

    ![Select ABAP Trial](dev.png)

5. Click **Service Marketplace** to get to the service overview, where you can select ABAP Trial as a service.

    ![Select ABAP Trial](account4.png)

6. Select **ABAP Trial** tile.

    ![Select ABAP Trial](trial.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create instance)]
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

[ACCORDION-BEGIN [Step 3: ](Create service key)]
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

  5. Now your service key appears. Download your service key for later use.
     The service key enables the connection to the SAP Cloud Platform ABAP environment system in ADT.

     ![Create service key](servicekeyx.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Open ABAP Development Tools )]

Open your local ABAP Development Tools (ADT). You can download the latest version from <https://tools.hana.ondemand.com/#abap>.

![Create service definition](adt_logo.png)

For TechEd users:

Click **>>** on your windows taskbar and select **SAP Development Tools** > **ABAP in Eclipse - `CAA361`**.  

![Create service definition](adt.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Create ABAP cloud project)]
  1. Select **File** > **New** > **Other** > **ABAP Cloud Project** and click **Next >**.

      ![Create ABAP cloud project](cloud.png)

  2. Select **Service Key** and click **Next >**.

      ![Create ABAP cloud project](project2.png)

  3. Select **Import** to import your downloaded service key.

      ![Create ABAP cloud project](projectx1.png)

  4. You can find your service key in your default download folder. Search for **`ADT.json`**, select it and click **Open**.

      ![Create ABAP cloud project](projectx2.png)

  5. Now your service key is pasted. Click **Next**.

      ![Create ABAP cloud project](project3.png)

  6. Logon to your ABAP trial account with your e-mail address and password.

      ![Create ABAP cloud project](project4.png)

  7. Click Finish.

      ![Create ABAP cloud project](project5.png)

  8. Your trial system appears on the project explorer.

      ![Create ABAP cloud project](project6.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](Test yourself)]

[VALIDATE_1]
[ACCORDION-END]


<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=abap-environment-trial-onboarding" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
