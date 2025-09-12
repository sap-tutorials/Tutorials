---
parser: v2
auto_validation: true
time: 5
tags: [tutorial>beginner, topic>machine-learning, topic>artificial-intelligence, topic>cloud, software-product>sap-business-technology-platform, software-product>sap-ai-services, software-product>sap-document-ai]
primary_tag: topic>machine-learning
author_name: Juliana Morais
author_profile: https://github.com/Juliana-Morais
---

# Use Trial to Set Up Account for SAP Document AI and Go to Application
<!-- description --> Get access to the SAP Document AI basic UI using a booster in SAP Business Technology Platform (SAP BTP) Trial that automatically creates a service instance, and subscribes you to the UI application for SAP Document AI.

## Prerequisites
- You have created a trial account on SAP BTP: [Get a Free Account on SAP BTP Trial](hcp-create-trial-account)
- You have a trial subaccount and dev space with **US East (VA)** as region: [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements). See also [Create a Subaccount](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/261ba9ca868f469baf64c22257324a75.html).

## You will learn
  - How to access your trial account 
  - What are interactive guided boosters
  - How to use the **Set up account for SAP Document AI** booster to assign entitlements, update your subaccount, create a service instance, subscribe to and access the SAP Document AI basic UI.

---

### Go to your trial account


1. In your web browser, open the [SAP BTP Trial cockpit](https://cockpit.hanatrial.ondemand.com/).

2. Navigate to your trial global account by clicking **Go To Your Trial Account**.

    <!-- border -->![Trial global account](trial.png)

    >If this is your first time accessing your trial account, you'll have to configure your account by choosing a region. **Please select US East (VA)**. Your user profile will be set up for you automatically.

    >Wait till your account is set up and ready to go. Your global account, your subaccount, your organization, and your space are launched. This may take a couple of minutes.

    >Choose **Continue**.

    >For more details on how to configure entitlements, quotas, subaccounts and service plans on SAP BTP Trial, see [Manage Entitlements on SAP BTP Trial](cp-trial-entitlements).



### Run booster


SAP BTP creates interactive guided boosters to automate cockpit steps, so users can save time when trying out the services.

Now, you will use the **Set up account for SAP Document AI** booster to automatically assign entitlements, update your subaccount, create a service instance, subscribe to and access the SAP Document AI basic UI.

1. On the navigation side bar, click **Boosters**.

    <!-- border -->![UI application](access-booster.png)

2. Search for **SAP Document AI** and click **Start**.

    <!-- border -->![UI application](access-booster-tile.png)

    >If you have more than one subaccount, the booster will choose automatically the correct subaccount and space, but this will require that you click **Next** twice and **Finish** once, before being able to see the **Success** dialog box.

    ![UI application](booster-success.png)




### Go to Application


1. Right click **Go to Application**, then click ***Copy link*** to save the link to be able to open the application once again in the future.

    ![UI application](booster-success-app-link.png)

2. Click **Go to Application**.

    ![UI application](booster-success-app.png)

You have successfully used the booster **Set up account for SAP Document AI** to subscribe to and access the SAP Document AI basic UI.

<!-- border -->![UI application](app.png)

>If you face any issue with the booster **Set up account for SAP Document AI**, you can alternatively follow the steps in [Subscribe to the SAP Document AI Basic UI](cp-aibus-dox-ui-sub) to subscribe to the user interface application manually.

Congratulations, you’ve completed this tutorial. You're now all set to [Use Machine Learning to Extract Information from Documents](cp-aibus-dox-ui).