---
parser: v2
auto_validation: true
time: 10
tags: [ tutorial>beginner, programming-tool>sapui5, products>sap-fiori, products>sap-business-technology-platform, products>sap-workflow, software-product-function>sap-cloud-application-programming-model, topic>mobile, products>sap-mobile-cards, products>mobile-development-kit-client]
primary_tag: products>sap-business-application-studio
author_name: Raz Korn
author_profile: https://github.com/raz-korn
---

# Set Up SAP Business Application Studio for Development
<!-- description --> SAP Business Application Studio is a development environment available on SAP Business Technology Platform. Before you can start developing using SAP Business Application Studio, administrators must perform the required onboarding steps that are described in this tutorial.

## Prerequisites
 - You have an SAP BTP Trial account ([Get a Free Account on SAP BTP Trial](hcp-create-trial-account))

## You will learn
  - How to set up SAP Business Application Studio

## Intro
The main focus of this tutorial is setting up SAP Business Application Studio in the trial environment.

For setting up SAP Business Application Studio in an Enterprise Account refer to [Getting Started](https://help.sap.com/viewer/9d1db9835307451daa8c930fbd9ab264/Cloud/en-US/19611ddbe82f4bf2b493283e0ed602e5.html) topic of the SAP Business Application Studio Administrator Guide.

In an Enterprise Account you can set up SAP Business Application Studio as a Free-Tier service by checking its **free** plan during its entitlement configuration.

---

### Log into SAP BTP Trial

1. Go to <https://account.hanatrial.ondemand.com> and log in to your SAP BTP cockpit.

2. You might be asked to accept the legal terms. Check the box and click **Accept**.

    <!-- border -->![BTPlegalterms](BTP-Terms-.png)

### Check Cloud Foundry Enablement

1. Click **Go To Your Trial Account** to open the list of available trial subaccounts. 

    <!-- border -->![enter your trial account](EnterTrial.png)
 
2. Select the subaccount you want to use for this tutorial.

    <!-- border -->![select subaccount](SelectSubaccount.png)
 
3. In the overview page, check that Cloud Foundry is enabled.

    <!-- border -->![Cloud Foundry Enabled](CFEnabled.png)
 
4. If Cloud Foundry is not enabled, press the button **Enable Cloud Foundry**.

    <!-- border -->![Enable Cloud Foundry](EnableCF.png)

### Check for Needed Entitlements

1. Go to **Entitlements**. 

    <!-- border -->![Entitlements](Entitlements.png)

2. Make sure the entitlements listed below are added. If you can’t find the entitlements make sure to add them as described here: [Setting Up Your Trial Account | SAP Help Portal](https://help.sap.com/docs/btp/sap-business-technology-platform/kyma-env-setting-up-your-trial-account?locale=f16df12fab9f4fe1b8a4122f0fd54b6e.html).

    Authorization and Trust Management Service – all 4 plans should be added.

    <!-- border -->![Entitlements](Entitlements01.png)
 
    SAP Build Work Zone, standard edition – add the following two plans:

    <!-- border -->![Entitlements](Entitlements02.png)
 
    SAP Business Application Studio - add the following plan:

    <!-- border -->![Entitlements](Entitlements03.png)


### Launch SAP Business Application Studio

1. Go back to **Trial Home**.

    <!-- border -->![TrialHome](TrialHome.png)

2. Click **SAP Business Application Studio** to launch SAP Business Application Studio.

    <!-- border -->![launch sap business application studio](BTP-Access-AppStudio-.png)

    >In recently created trial accounts, SAP Business Application Studio is subscribed to by default.

    >If you receive an error message and your recently created account was created with error, it is recommended that you re-create your account: delete the account and then [Get a Free Account on SAP BTP Trial](hcp-create-trial-account).

    >**CAUTION:** If you receive an error message and your account was not created recently, you need to add a subscription to SAP Business Application Studio. Answer the validation question of the current step, go to the next step in this tutorial, and proceed from there.

3. You might be asked to accept the legal terms. Check the box and click **OK**.

    <!-- border -->![legalterms](AppStudio-Terms-.png)

4. A new tab opens. If you have not created a dev space, the welcome page for SAP Business Application Studio loads.

    <!-- border -->![sap business application studio welcome](BAS-Welcome--.png)

    >If this is not the first dev space, the dev space manager for SAP Business Application Studio loads.

    ><!-- border -->![Create Dev Space](BAS-Dev-Space-Manager-Empty-.png)

5. Click **My Dev Spaces** to open the SAP Business Application Studio dev space manager page.

    <!-- border -->![sap business application studio welcome](BAS-Welcome-.png)

    >Dev space manager for SAP Business Application Studio
    ><!-- border -->![sap business application studio dev space manager](BAS-Dev-Space-Manager-Empty-.png)

    >**Bookmark this page!**

    >If you create a bookmark to this page, it is easy to get back to SAP Business Application Studio later.

    <br>
    >**PAY ATTENTION:** If you reached this point in the tutorial, you have successfully on-boarded SAP Business Application Studio. Answer the validation question of the current tutorial step, and mark the rest of the steps as "Completed".

### Add SAP Business Application Studio subscription to subaccount

Depending on when you created the SAP BTP Trial account, this subscription might already be added in the subaccount. If you are not subscribed to SAP Business Application Studio, follow this step to add the SAP Business Application Studio subscription to a subaccount.

1. Click **Enter Your Trial Account** to access the Cloud Foundry environment.

    <!-- border -->![Access SAP BTP Trial](BTP-Access-AppStudio--.png)

2. Select the tile of the subaccount in which you want to enable the SAP Business Application Studio subscription.

    >For the trial environment, SAP Business Application Studio is only available on:

    > - Amazon Web Services (AWS) - US East (VA) region (US10 trial).

    > - Microsoft Azure - Singapore region (AP21 trial).

    >An updated list of regions is available in [SAP Discovery Center > Service Plan > Trial](https://discovery-center.cloud.sap/#/serviceCatalog/business-application-studio?tab=service_plan&licenseModel=free).

    >When creating an SAP BTP Trial account a **trial** subaccount is generated.

    <!-- border -->![subaccount](Cockpit-Select-Subaccount-.png)

3. From the navigation area, click **Service Marketplace**.

    <!-- border -->![opensubscriptions](Cockpit-Navigate-to-Subscriptions-.png)

4. In the **Service Marketplace** page, search for **`studio`**.

    <!-- border -->![filterservice](Cockpit-Filter-and-Select-AppStudio-Subscription-.png)

5. Click **Actions** icon (three dots) to open the list of available actions.

    <!-- border -->![subscribe](Cockpit-Subscribe-.png)

6. Click **Create** to launch the wizard for subscribing to SAP Business Application Studio.

    <!-- border -->![subscribe](Cockpit-Subscribe-2-.png)

7. In the wizard verify that `SAP Business Application Studio` is selected in the **Service** field and `trial` is selected in the **Plan** field.

    <!-- border -->![subscribewizard](Cockpit-Create-Service-Wizard-.png)

8. Click **Create** to subscribe to SAP Business Application Studio.

    <!-- border -->![subscribewizard](Cockpit-Create-Service-Wizard--.png)

9. A **Creation in Progress** popup appears. Click **View Subscription** to view the SAP Business Application Studio subscription in the **Instances and Subscriptions** page.

    <!-- border -->![subscriptioninprogress](Cockpit-Subscription-in-Progress-.png)

### Launch SAP Business Application Studio

1. Click **Go to Application**.

    <!-- border -->![gotoapplication](Cockpit-Go-to-Application-.png)

2. You might be asked to accept the legal terms. Check the box and click **OK**.

    <!-- border -->![legalterms](AppStudio-Terms-.png)

3. An **Access Denied** page may appear. Log out from SAP Business Application Studio and then log in as depicted below.

    <!-- border -->![Logout](AppStudio-Access-Denied-Logout-.png)
    &nbsp;
    <!-- border -->![Login](AppStudio-Access-Denied-Login-.png)

4. Enter your credentials, and click **Log On**.

    <!-- border -->![authentication](AppStudio-Authentication-.png)

5. You might be asked to accept the legal terms. Check the box and click **OK**.

    <!-- border -->![legalterms](AppStudio-Terms-.png)

6. A new tab opens and the welcome page for SAP Business Application Studio loads.

    <!-- border -->![sap business application studio welcome](BAS-Welcome--.png)

7. Click **My Dev Spaces** to open the SAP Business Application Studio dev space manager page.

    <!-- border -->![sap business application studio welcome](BAS-Welcome-.png)

    >Dev space manager for SAP Business Application Studio
    ><!-- border -->![sap business application studio dev space manager](BAS-Dev-Space-Manager-Empty-.png)

    >**Bookmark this page!**

    >If you create a bookmark to this page, it is easy to get back to SAP Business Application Studio later.


---

Congratulations!

With this, you have successfully completed the setup of SAP Business Application Studio.
