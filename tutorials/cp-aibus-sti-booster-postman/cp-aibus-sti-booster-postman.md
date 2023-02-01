---
parser: v2
auto_validation: true
time: 5
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-business-services, software-product>service-ticket-intelligence]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

# Use Trial to Set Up Account for Service Ticket Intelligence and Download Postman Sample Files
<!-- description --> Use a booster in SAP BTP Trial to automatically create a service instance, and download Postman environment and collection JSON sample code files for Service Ticket Intelligence.

## Prerequisites
- You have created a trial account on SAP BTP: [Get a Free Account on SAP BTP Trial](hcp-create-trial-account)
- You have a subaccount and dev space with **US East (VA)** as region: [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements). See also [Create a Subaccount](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/261ba9ca868f469baf64c22257324a75.html). You can also use old trial subaccounts created before October 2021 with **Europe (Frankfurt)** as region.

## You will learn
  - How to access your trial account
  - What are interactive guided boosters
  - How to use the **Set up account for Service Ticket Intelligence** booster to assign entitlements, update your subaccount, create a service instance, and also to download Postman environment and collection JSON sample code files for the Service Ticket Intelligence service.
---

### Go To Your Trial Account


1. In your web browser, open the [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to the trial global account by clicking **Go To Your Trial Account**.

    <!-- border -->![Trial global account](01_Foundation20Onboarding_Home.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region. **Please select US East (VA)**. Your user profile will be set up for you automatically.

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    ><!-- border -->![Account setup](02_Foundation20Onboarding_Processing.png)

    >For more details on how to configure entitlements, quotas, subaccounts and service plans on SAP BTP Trial, see [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements).



### Run booster


SAP BTP creates interactive guided boosters to automate cockpit steps, so users can save time when trying out the services.

Now, you will use the **Set up account for Service Ticket Intelligence** booster to automatically assign entitlements, update your subaccount and create a service instance for Service Ticket Intelligence.

1. On the navigation side bar, click **Boosters**.

    <!-- border -->![Postman](access-booster.png)

2. Search for **Set up account for Service Ticket Intelligence** and click the tile to access the booster.

    <!-- border -->![Postman](access-booster-tile.png)

3. Click **Start**.

    <!-- border -->![Postman](booster-start.png)

    >If you have more than one subaccount, the booster will choose automatically the correct subaccount and space, but this will require that you click **Next** twice and **Finish** once, before being able to see the **Success** dialog box.

    <!-- border -->![Postman](booster-success.png)




### Download Postman sample files


Download Postman environment and collection sample files. Make a local copy of the files.

<!-- border -->![Postman](booster-success-postman.png)

>If you face any issue with the booster **Set up account for Service Ticket Intelligence**, you can alternatively follow the steps in [Use Trial to Create a Service Instance for Service Ticket Intelligence](cp-aibus-sti-service-instance) to create the service key for Service Ticket Intelligence manually, and download and edit the Postman environment and collection sample files, as described in [Set Up Postman to Call Service Ticket Intelligence APIs](cp-aibus-sti-setup-postman).

Congratulations, you have completed this tutorial. You are now all set to [Install Postman REST Client](api-tools-postman-install) and [Set Up Postman to Call Service Ticket Intelligence APIs](cp-aibus-sti-setup-postman).

