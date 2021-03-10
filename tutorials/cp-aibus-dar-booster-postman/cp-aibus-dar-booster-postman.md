---
title: Set Up Account for Data Attribute Recommendation and Download Postman Sample Files
description: Use a booster in SAP Business Technology Platform (SAP BTP) Trial to automatically create a service instance, and download Postman environment and collection JSON sample code files for Data Attribute Recommendation.
auto_validation: true
time: 5
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-business-technology-platform, products>sap-ai-business-services, products>data-attribute-recommendation]
primary_tag: topic>machine-learning
---

## Prerequisites
- You have created a trial account on SAP BTP: [Get a Free Account on SAP BTP Trial](hcp-create-trial-account)
- You have a subaccount and dev space with **Europe (Frankfurt)** as region: [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements). See also [Create a Subaccount](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/261ba9ca868f469baf64c22257324a75.html).

## Details
### You will learn
  - How to access your trial account
  - What are interactive guided boosters
  - How to use the **Set up account for Data Attribute Recommendation** booster to assign entitlements, update your subaccount, create a service instance, and also to download Postman environment and collection JSON sample code files.
---

[ACCORDION-BEGIN [Step 1: ](Enter your trial account)]

1. In your web browser, open the [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to the trial global account by clicking **Enter Your Trial Account**.

    !![Trial global account](01_Foundation20Onboarding_Home.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region. **Please select Europe (Frankfurt)**. Your user profile will be set up for you automatically.

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    >!![Account setup](02_Foundation20Onboarding_Processing.png)

    >For more details on how to configure entitlements, quotas, subaccounts and service plans on SAP BTP Trial, see [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Run booster)]

SAP BTP creates interactive guided boosters to automate cockpit steps, so users can save time when trying out the services.

Now, you will use the **Set up account for Data Attribute Recommendation** booster to automatically assign entitlements, update your subaccount and create a service instance for Data Attribute Recommendation.

1. On the navigation side bar, click **Boosters**.

    !![Postman](access-booster.png)

2. Search for **Set up account for Data Attribute Recommendation** and click the tile to access the booster.

    !![Postman](access-booster-tile.png)

3. Click **Start**.

    !![Postman](booster-start.png)

    >If you have more than one subaccount, the booster will choose automatically the correct subaccount and space, but this will require that you click **Next** twice and **Finish** once, before being able to see the **Success** dialog box.

    !![Postman](booster-success.png)

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Download Postman sample files)]

Download Postman environment and collection sample files. Make a local copy of the files.

!![Postman](booster-success-postman.png)

>If you face any issue with the booster **Set up account for Data Attribute Recommendation**, you can alternatively follow the steps in [Create Service Instance for Data Attribute Recommendation](cp-aibus-dar-service-instance) to create the service key for Data Attribute Recommendation manually, and download and edit the Postman environment and collection sample files, as described in [Set Up Postman to Call Data Attribute Recommendation APIs](cp-aibus-dar-setup-postman).

Congratulations, you have completed this tutorial. You are now all set to [Install Postman REST Client](api-tools-postman-install) and [Set Up Postman to Call Data Attribute Recommendation APIs](cp-aibus-dar-setup-postman).

[DONE]
[ACCORDION-END]
