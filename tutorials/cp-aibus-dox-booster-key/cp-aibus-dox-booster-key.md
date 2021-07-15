---
title: Set Up Account for Document Information Extraction and Get Service Key
description: Get access to the service Swagger UI using a booster in SAP Business Technology Platform (SAP BTP) Trial that automatically creates a service instance, and the associated service key for Document Information Extraction.
auto_validation: true
time: 5
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, products>sap-business-technology-platform, products>sap-ai-business-services, products>document-information-extraction]
primary_tag: topic>machine-learning
---

## Prerequisites
- You have created a trial account on SAP BTP: [Get a Free Account on SAP BTP Trial](hcp-create-trial-account)
- You have a subaccount and dev space with **Europe (Frankfurt)** or **US East (VA)** as region: [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements). See also [Create a Subaccount](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/261ba9ca868f469baf64c22257324a75.html).

## Details
### You will learn
  - How to access your trial account
  - What are interactive guided boosters
  - How to use the **Set up account for Document Information Extraction** booster to assign entitlements, update your subaccount, create a service instance and the associated service key for Document Information Extraction.

---

[ACCORDION-BEGIN [Step 1: ](Enter your trial account)]

1. In your web browser, open the [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to the trial global account by clicking **Enter Your Trial Account**.

    !![Trial global account](01_Foundation20Onboarding_Home.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region. **Please select Europe (Frankfurt) or US East (VA)**. Your user profile will be set up for you automatically.

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    >!![Account setup](02_Foundation20Onboarding_Processing.png)

    >For more details on how to configure entitlements, quotas, subaccounts and service plans on SAP BTP Trial, see [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Run booster)]

SAP BTP creates interactive guided boosters to automate cockpit steps, so users can save time when trying out the services.

Now, you will use the **Set up account for Document Information Extraction** booster to automatically assign entitlements, update your subaccount, create a service instance and the associated service key for Document Information Extraction.

1. On the navigation side bar, click **Boosters**.

    !![Service Key](access-booster.png)

2. Search for **Set up account for Document Information Extraction** and click the tile to access the booster.

    !![Service Key](access-booster-tile.png)

3. Click **Start**.

    !![Service Key](booster-start.png)

    >If you have more than one subaccount, the booster will choose automatically the correct subaccount and space, but this will require that you click **Next** twice and **Finish** once, before being able to see the **Success** dialog box.

    !![Service Key](booster-success.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Get service key)]

You have successfully used the booster **Set up account for Document Information Extraction** to create a service key for Document Information Extraction.

1. Click **Download Service Key**.

    !![Service Key](booster-success-key.png)

2. Make a local copy of the service key.

    !![Service Key](booster-service-key.png)

>If you face any issue with the booster **Set up account for Document Information Extraction**, you can alternatively follow the steps in [Create Service Instance for Document Information Extraction](cp-aibus-dox-service-instance) to create a service key for Document Information Extraction manually.

You are now all set to [Get OAuth Access Token for Document Information Extraction Using Any Web Browser](cp-aibus-dox-web-oauth-token). Step 4 is optional. If you're not interested, you can set it to **Done** and go directly to the next tutorial.

[VALIDATE_1]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Access service instance and service key (optional))]

> This is an optional step. Use it only if you want to access the service instance and service key, you created with the **Set up account for Document Information Extraction** booster, without having to run it once again.

Do the following to access your service instance and service key, without having to run the **Set up account for Document Information Extraction** booster once again:

1. Close the booster **Success** dialog box.

    !![Service Key](leave-success.png)

2. Access your trial account.

    !![Service Key](trial-account.png)

3. Click **Subaccounts** on the navigation side bar and select **trial** to access your subaccount.

    !![Service Key](subaccount.png)

4. Click **Instances and Subscriptions** on the navigation side bar. You see the service instance you created with the **Set up account for Document Information Extraction** booster.

    !![Service Key](service-instance.png)

5. Click the navigation arrow to open the details of your service instance. Then, click the dots to **View**, **Download** or **Delete** your service key.

    !![Service Key](service-key.png)

Congratulations, you have completed this tutorial.

[DONE]
[ACCORDION-END]
