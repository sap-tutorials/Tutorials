---
title: Create Service Instance for Document Information Extraction
description: Create a service instance and the associated service key for Document Information Extraction, one of the SAP AI Business Services, using SAP Business Technology Platform (SAP BTP) Trial.
auto_validation: true
time: 15
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-business-technology-platform, products>sap-ai-business-services, products>document-information-extraction]
primary_tag: topic>machine-learning
---

## Prerequisites
- You have created a trial account on SAP BTP: [Get a Free Account on SAP BTP Trial](hcp-create-trial-account)
- You have a subaccount and dev space with **Europe (Frankfurt)** or **US East (VA)** as region: [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements). See also [Create a Subaccount](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/261ba9ca868f469baf64c22257324a75.html).

## Details
### You will learn
  - How to check your Document Information Extraction entitlements
  - How to create a service instance of Document Information Extraction
  - How to create a service key for your service instance
---

[ACCORDION-BEGIN [Step 1: ](Enter your trial account)]

1. In your web browser, open the [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to the trial global account by clicking **Enter Your Trial Account**.

    !![Trial global account](01_Foundation20Onboarding_Home.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region. **Please select Europe (Frankfurt) or US East (VA)**. Your user profile will be set up for you automatically.

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    >!![Account setup](02_Foundation20Onboarding_Processing.png)

3. From your global account page, choose the `trial` tile to access your subaccount.

    ![Subaccounts](enter-trial-account.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Check entitlements)]

To try out Document Information Extraction, you need to make sure that your subaccount is properly configured.

1. On the navigation side bar, click **Entitlements** to see a list of all eligible services. You are entitled to use every service in this list according to the assigned service plan.

2. Search for **Document Information Extraction Trial**. ***If you find the service in the list, you are entitled to use it. Now you can set this step to **Done** and proceed with Step 3.***

    ![Entitlements](check-entitlements.png)

***ONLY if you DO NOT find the service in your list, proceed as follows:***

  1. Click **Configure Entitlements**.

    ![Configure Entitlements](configure-entitlements.png)

  2. Click **Add Service Plans**.

    ![Add Service Plan](add-service-plans.png)

  3. Select **Document Information Extraction Trial**, and choose the **default** service plan. Click **Add 1 Service Plan**.

    ![Add Service Plan](add-entitlements.png)

  4. **Save** your **Entitlements** changes.

    ![Add Service Plan](save-entitlements.png)    

You are now entitled to use Document Information Extraction and create instances of the service.

>For more details on how to configure entitlements, quotas, subaccounts and service plans on SAP BTP Trial, see [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Access your space)]

You will find all applications and services in spaces. By default, trial accounts only have the **dev** space available.

Click **Spaces** on the navigation side bar and select the **dev** space to open it.

![Spaces](access-space.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Access service via Service Marketplace)]

The **Service Marketplace** is where you find all the services available on SAP BTP.

1. To access it, click **Service Marketplace** on the navigation side bar.

    ![Service Marketplace](access-service-marketplace.png)

2. Next, search for **Document Information Extraction** and click the tile to access the service.

    ![Document Information Extraction in Service Marketplace](access-dox.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create service instance)]

You will now create an instance of your service.

Click **Create Instance** to start the creation dialog.

![Service Instance](create-instance.png)

In the dialog, leave the default value for the service and the service plan. Enter a name for your new instance as `doc-info-extraction-inst` and click **Create Instance** to skip the other steps and create the instance.

![Create Instance](create-instance-dialog.png)

In the following dialog, click on **View Instance** to be navigated to the list of your service instances.

![View Instances](view-instances.png)

You have successfully created a service instance for Document Information Extraction.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Create service key)]

You are now able to create a service key for your new service instance. Service keys are used to generate credentials to enable apps to access and communicate with the service instance.

  1. Click the navigation arrow to open the details of your service instance. Then, click the dots to open the menu and select **Create Service Key**.

      ![Service Key](create-service-keys.png)

  2. In the dialog, enter `doc-info-extraction-key` as the name of your service key. Click **Create** to create the service key.

      ![Create Service Key](create-service-key-name.png)

You have successfully created a service key for your service instance. You can now either view the service key in the browser or download it.

![View Service Key](view-service-key.png)

You will need the service key values to create your `access_token` in the next tutorial: [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token).

[VALIDATE_1]
[ACCORDION-END]
