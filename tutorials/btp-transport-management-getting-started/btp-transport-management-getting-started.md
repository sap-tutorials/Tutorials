---
author_name: Anke Ravalitera
author_profile: https://github.com/Anke2016
keywords: tutorial
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product>sap-cloud-transport-management, software-product>sap-content-agent-service, software-product>sap-integration-suite ]
primary_tag: software-product>sap-cloud-transport-management
parser: v2
---
# Get Started with SAP Cloud Transport Management Service
<!-- description --> Learn how to set up SAP Cloud Transport Management Service in an SAP BTP Trial account, or in an existing SAP BTP subaccount. 

## Prerequisites
 - If you are new to SAP BTP, follow the tutorial [View the SAP BTP from 10,000 Meters](cp-explore-cloud-platform).
 - If you do not have an SAP BTP account, follow the tutorial [Get an Account on SAP BTP Trial](hcp-create-trial-account).

## You will learn
   - How to create a subaccount in an SAP BTP global account
   - How to configure entitlements to SAP Cloud Transport Management service
   - How to subscribe to SAP Cloud Transport Management service
   - How to set up role collections required for SAP Cloud Transport Management service
   - How to create a service instance of SAP Cloud Transport Management service and a service key 

## Intro

>Please note that it involves a significant amount of manual work to migrate an SAP Cloud Transport Management landscape from a trial account to a standard instance (for productive use). In contrast, it is very **easy to switch a free instance** of SAP Cloud Transport Management **to a standard instance**, keeping all your configuration intact. We therefore recommend to use a subaccount in a CPEA-enabled global account if available to you. To do this, follow the steps under *Enterprise Account*. 

For more information about SAP Cloud Transport Management service, see the SAP Help Portal at [SAP Cloud Transport Management Service](https://help.sap.com/docs/cloud-transport-management).

### Open the SAP BTP cockpit

[OPTION BEGIN [Trial Account]]


1. To access BTP Cockpit of your trial account, go to your [SAP BTP Trial landing page](https://account.hanatrial.ondemand.com/trial/#/home/trial) and choose **Go To Your Trial Account**. (The name of the button may vary.)    

2. In your global trial account, you have the default **trial** subaccount.

    ![Create CS Subaccount 00](screenshots/get-started-00.png)  

    SAP recommends using SAP Cloud Transport Management in a separate administrative subaccount to facilitate access control and separation of concerns. Therefore, you need a separate subaccount where you subscribe to SAP Cloud Transport Management and create a service instance and key for API access to the service.

3. To create that new subaccount for SAP Cloud Transport Management service, in the **Account Explorer** of your global account, choose **Create > Subaccount**.
   
    ![Create CS Subaccount 01](screenshots/get-started-01.png)  

4. Enter a **Display Name** (1), here `Central Services` and select a **Region** (2). Choose **Create** (3).

    ![Create CS Subaccount 02](screenshots/get-started-03.png)  

    The new subaccount is created. 

5. Click on the **Central Services** tile.

    ![Create CS Subaccount 04](screenshots/get-started-04.png)  

6. You're in the overview of the **Central Services** subaccount. You need to enable Cloud Foundry in the subaccount to create a Cloud Foundry org in which you can create a space. To do this, choose **Enable Cloud Foundry**.

    ![Create CS Subaccount 05](screenshots/get-started-05.png)

6. Keep the suggested values unchanged, and choose **Create**.

    ![Create CS Subaccount 06](screenshots/get-started-06.png)

    The Cloud Foundry org is created.

    As a next step, create a space in the org. A space is a shared location on the platform in which you can deploy and maintain applications, and connect them to services. You can create multiple spaces within one org. 

7. To create a space in this org, choose **Create Space**.

    ![Create CS Subaccount 07](screenshots/get-started-07.png)

6. Enter a space name, here `cTMS` (1), keep the default space roles unchanged, and choose **Create** (2).

    ![Create CS Subaccount 08](screenshots/get-started-08.png)

8. The new **cTMS** space is displayed in the list of spaces. 

    ![Create CS Subaccount 09](screenshots/get-started-09.png)  
    
The following steps are required in SAP BTP trial, because the default **trial** subaccount has default entitlements for **SAP Cloud Transport Management** that you'll need in the **Central Services** subaccount. Entitlements are the service plans that you're entitled to use. To use the entitlements in the **Central Services** subaccount, remove them from **trial** first.  


1. In your global account in SAP BTP trial, click on the **trial** tile.
   
    ![Create CS Subaccount 10](screenshots/get-started-10.png)

2. Choose **Entitlements** from the navigation on the left (1), filter the entries for **transport** (2), and choose **Edit** (3). 

    ![Create CS Subaccount 11](screenshots/get-started-11.png)

3. To remove the entitlements for *Cloud Transport Management* from **trial**, click on the trash bin icons at the end of the row.

    ![Create CS Subaccount 12](screenshots/get-started-12.png)

4. Save your changes.

    ![Create CS Subaccount 13](screenshots/get-started-13.png)

    The entitlements for Cloud Transport Management were removed from the **trial** subaccount. 

    ![Create CS Subaccount 14](screenshots/get-started-14.png)  

See also on SAP Help Portal: [Managing Subaccounts Using the Cockpit](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/55d0b6d8b96846b8ae93b85194df0944.html?locale=en-US&state=PRODUCTION&version=Cloud), [Org Administration Using the Cockpit](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/c4c25cc63ac845779f76202360f98694.html?locale=en-US&state=PRODUCTION&version=Cloud)


[OPTION END]

[OPTION BEGIN [Enterprise Account]]

1. To access SAP BTP Cockpit of your enterprise account, choose [https://cockpit.btp.cloud.sap](https://cockpit.btp.cloud.sap).
   Depending on your own geo location, this URL will redirect you to the closest regional SAP BTP Cockpit URL.

2. In your global account, navigate to the subaccount in which you want to use SAP Cloud Transport Management. You might also want to create a new subaccount for this tutorial, if you have the necessary authorizations.

    ![Subaccounts](screenshots/0010%20Subaccount%20overview.png)

3. If Cloud Foundry is not yet configured in your subaccount, you have to do it now. For that, click on **Enable Cloud Foundry** in your subaccount overview.

    ![EnableCloudFoundry](screenshots/0020%20Enable%20Cloud%20Foundry.png)

4. In the dialog window, stay with the defaults and potentially adapt (shorten) the **Org Name**, which still has to be unique. Click on **Create**

    ![EnableCloudFoundry2](screenshots/0030%20Enable%20Cloud%20Foundry2.png)

5. You need to create a space in your Cloud Foundry environment. For this, click on **Create Space** in your subaccount overview.

    ![CreateSpace](screenshots/0040%20Create%20space.png)

6. In the dialog window, provide a **Space Name**, leave the selected roles unchanged, and click on **Create**.

    ![CreateSpace2](screenshots/0050%20Create%20space2.png)

7. Your subaccount overview should look like this:

    ![SubaccountConfiguredWithCF](screenshots/0060%20subaccount%20configured%20with%20CF.png)

> See also on SAP Help Portal: [Initial Setup](https://help.sap.com/docs/cloud-transport-management/sap-cloud-transport-management/initial-setup)

[OPTION END]

---

### Configure Entitlements to SAP Cloud Transport Management Service

In general, entitlements are required to define access of a subaccount to a service.

[OPTION BEGIN [Trial Account]]

Since you've removed the entitlements for SAP Cloud Transport Management service from the **trial** subaccount in the previous step, the entitlements are available in the **Central Services** subaccount. 

1.  In the **Account Explorer** of the global account, click on the **Central Services** tile.

    ![Move Entitlements 15](screenshots/get-started-15.png)

2. In the subaccount overview, choose **Entitlements** from the navigation on the left (1), and choose **Edit** (2).

    ![Move Entitlements 16](screenshots/get-started-16.png)

3.  To add entitlements, choose **Add Service Plans**.

    ![Move Entitlements 17](screenshots/get-started-17.png)

4.  In the search field, filter for **transport** (1) and select **Cloud Transport Management** (2). 

    ![Move Entitlements 18](screenshots/get-started-18.png)

5.  Select both plans (1) - **standard** for API access to Cloud Transport Management, and **lite (Application)** for access to the user interface - and choose **Add 2 Service Plans** (2).

    ![Move Entitlements 19](screenshots/get-started-19.png)

6. Filter for **transport** (1) to see the assigned plans for SAP Cloud Transport Management service. Save your changes (2).

    ![Move Entitlements 20](screenshots/get-started-20.png)

[OPTION END]


[OPTION BEGIN [Enterprise Account]]

1. To display the existing entitlements for SAP Cloud Transport Management, select the **Entitlements** link on your subaccount's overview screen. In a new subaccount (or in a subaccount that hasn't been used for SAP Cloud Transport Management), you will normally not find entitlements for SAP Cloud Transport Manangement.

    ![ExistingEntitlements](screenshots/0100%20Existing%20Entitlements.png)

2. To change the entitlement settings, click on **Edit**.

    ![EditEntitlements](screenshots/0110%20Edit%20Entitlements.png)

3. To add the missing entitlements, click on **Add Service Plans**.     

    ![AddServicePlans](screenshots/0120%20Add%20service%20plans.png)

4. In the dialog window, search for *Cloud Transport Management* (for example by entering *trans*). Click on the entry **Cloud Transport Management**. From the **Available Plans**, select **standard** (without *Application*) and **free (Application)**. Click on **Add 2 Service Plans**.

    ![SelectServicePlans](screenshots/0130%20Select%20cTMS%20service%20plans.png)

5. Choose **Save**.

    ![SaveServicePlans](screenshots/0140%20Save%20service%20plans.png)

[OPTION END]

See also on SAP Help Portal: [Configuring Entitlements to SAP Cloud Transport Management](https://help.sap.com/docs/TRANSPORT_MANAGEMENT_SERVICE/7f7160ec0d8546c6b3eab72fb5ad6fd8/13894bed9e2d4b25aa34d03d002707f9.html?locale=en-US)

---

### Subscribe to SAP Cloud Transport Management Service

To enable the usage of the user interface of SAP Cloud Transport Management service, subscribe to the Cloud Transport Management application.

[OPTION BEGIN [Trial Account]]

1. In your subaccount, go to **Services > Service Marketplace** (1). Filter for **transport** (2). The *Cloud Transport Management* tile is displayed (3).    

    ![Subscription1](screenshots/Subscription01.png)

2. Select the three dots (...) on the tile, and choose **Create**.

    ![Subscription2](screenshots/Subscription02.png)

3. On the **New Instance or Subscription** dialog, from the **Plan** dropdown box (1), select **lite** (2) and choose **Create** (3).

    ![Subscription3](screenshots/Subscription03.png)

4. On the **Creation in Progress** dialog, choose **View Subscription**.

    ![Subscription4](screenshots/Subscription04.png)

5. You've subscribed to the *Cloud Transport Management* application. 

    ![Subscription5](screenshots/Subscription05.png)



[OPTION END]

[OPTION BEGIN [Enterprise Account]]

1. In your subaccount, go to **Services > Service Marketplace**. Use the *Search* field to filter for *transport management*. The *Cloud Transport Management* tile is displayed. Click on it.

    ![ServiceMarketplace](screenshots/0200%20Service%20Marketplace.png)

2. To create a subscription, choose **Create**.

    ![CreateSubscription1](screenshots/0210%20Create%20subscription1.png)

3. Select the *free* subscription plan. If you have access to a paid subscription, this would show as *standard* in the subscription plan list, in addition to the *standard* entry in the instance list.

    ![SelectSubscriptionPlan](screenshots/0220%20Select%20Subscription%20Plan.png)

4. Click on **Create**.

    ![CreateSubscrition2](screenshots/0230%20Create%20subscription2.png)

5. The subscription is in progress. Choose **View Subscription**.

    ![ViewSubscription](screenshots/0240%20View%20Subscription.png)

6. You've subscribed to the *Cloud Transport Management* application.

    ![SubscriptionEstablished](screenshots/0250%20Subscription%20established.png)


[OPTION END]

See also on SAP Help Portal: [Subscribing to Cloud Transport Management](https://help.sap.com/docs/TRANSPORT_MANAGEMENT_SERVICE/7f7160ec0d8546c6b3eab72fb5ad6fd8/7fe10fc1baae444e9315579786d623b9.html?locale=en-US)

---


### Set Up Role Collections

After successful subscription, you need to configure user access to the SAP Cloud Transport Management application. You create different role collections for the different SAP Cloud Transport Management roles, and assign roles to the role collections based on the application templates. Afterwards, you assign the role collections to users or user groups. 

>Although the screenshots show the steps in the trial account, they are identical and therefore also valid for an enterprise account.

1.  Before you can use the user interface of SAP Cloud Transport Management, you'll need to set up role collections and assign roles and permissions to your user, which you will do in this step. To create role collections for the roles that you want to use for the service, choose **Security > Role Collections** from the navigation on the left (1). Filter for role collections called *TMS* (2). The delivered role collections are displayed. To create a new role collection, choose **Create** (3).    

    ![Roles2](screenshots/Roles02.png)

2. To create a role collection for administrative tasks, enter a name, here `TMS Admin` (1), a description (optional), and choose **Create** (2).

    ![Roles3](screenshots/Roles03.png)

3. The role collection was created.

    ![Roles4](screenshots/Roles04.png)

4. To assign the SAP Cloud Transport Management **Administrator** role to the new role collection, go to the **Roles** tab of the subscription details. To do this, choose **Services > Instances and Subscriptions** from the navigation on the left (1), and select the arrow at the end of the **Cloud Transport Management** row (2).

    ![Roles3a](screenshots/Roles03a.png) 

5. Select the **Roles** tab.

    ![Subscription6](screenshots/Subscription06.png)

5. On the **Roles** tab, the default role templates for SAP Cloud Transport Management are displayed. In the row of the **Administrator** role template, choose the **+** button.

    ![Roles5](screenshots/Roles05.png)

7.  On the **Add to Role Collection** dialog box, select the **TMS Admin** role collection (1), and choose **Add** (2).

     ![Roles6](screenshots/Roles06.png)

8. The role collection was added to the **Administrator** role template.

     ![Roles7](screenshots/Roles07.png)

9.  You can now add users to the new role collection. To do this, choose **Security > Role Collections** from the navigation on the left (1). Filter for the *TMS* role collections (2). Select the arrow at the end of the **TMS Admin** row (3).

     ![Roles8](screenshots/Roles08.png)

10. In the details of the **TMS Admin** role collection, choose **Edit**.

    ![Roles9](screenshots/Roles09.png)

11. You can add individual users or user groups to the role collection. In the tutorial, add an individual user. To do this, select the identity provider (here: **Default identity provider**). In the ID field, enter an existing e-mail address and choose `Enter` (1). The **E-Mail** field is automatically filled with the selected e-mail address. Save your changes (2).

    ![Roles10](screenshots/Roles10.png)

12. The role collection was added to the selected user. You see that **1** user is displayed.

    ![Roles11](screenshots/Roles11.png)

If you want to create other role collections for other tasks, such as the **Import Operator** for import tasks, you can repeat the steps. However, this is not required for the tutorial. 



See also on SAP Help Portal: [Setting Up Role Collections](https://help.sap.com/docs/TRANSPORT_MANAGEMENT_SERVICE/7f7160ec0d8546c6b3eab72fb5ad6fd8/eb134e02d2074918bcc5af34f50fb19f.html?locale=en-US)

---

### Create a Service Instance and a Service Key

A service instance is required to enable the usage of SAP Cloud Transport Management service using programmatic access (using API Remote Call), for example if you want to use the service to export content directly in your application. To create a destination to SAP Cloud Transport Management service, you need to provide a service key. 

>Although the screenshots show the steps in the trial account, they are identical and therefore also valid for an enterprise account.

1. In your subaccount, choose **Services > Instances and Subscriptions** (1). Choose **Create** (2).

    ![ServiceInstance1](screenshots/ServiceInstance01.png)

4. From the **Services** dropdown menu, select **Cloud Transport Management** (1). From the **Plan** dropdown menu, select the **standard** plan of the **Instances** type (2). 

    ![ServiceInstance3](screenshots/ServiceInstance03.png)

5. Enter an **Instance Name** (1), and choose **Create** (2). 
    >You don't have to choose **Next**, because the next step isn't necessary for SAP Cloud Transport Management. It's a default step when creating service instances, allowing you to enter JSON parameters, but SAP Cloud Transport Management doesn't support JSON parameters.

    ![ServiceInstance4](screenshots/ServiceInstance04.png)

6. The service instance is being created.

    ![ServiceInstance5](screenshots/ServiceInstance05.png)

7. When the instance is created, you can create the service key. Select the three dots **(...)** at the end of the row and choose **Create Service Key**.

    ![ServiceInstance7](screenshots/ServiceInstance07.png)

8. On the **New Service Key** dialog, enter a name for the service key (1), here `cTMS1-key`, and choose **Create** (2).

    ![ServiceInstance8](screenshots/ServiceInstance08.png)

9. The service key is created. To display it, select the three dots **(...)** at the end of the row and choose **View**.

    ![ServiceInstance9](screenshots/ServiceInstance09.png)
 
10. The service key looks as follows. For the destination to SAP Cloud Transport Management service, you need, for example, the values of `clientid`, `clientsecret`, and `url` in the `uaa` section. 

    ![ServiceInstance10](screenshots/ServiceInstance10.png)



See also on SAP Help Portal: [Creating a Service Instance and a Service Key](https://help.sap.com/docs/TRANSPORT_MANAGEMENT_SERVICE/7f7160ec0d8546c6b3eab72fb5ad6fd8/f44956035ce54684b1dbb9e4d23c37d2.html?locale=en-US)

---

### Verify access to Cloud Transport Management

You should now be able to access the user interface of SAP Cloud Transport Management service. 


1. To check this, in your subaccount, choose **Services > Instances and Subscriptions** (1). In the **Subscriptions** section, choose the **Cloud Transport Management** link or the *Go to Application* icon to the right of it (2).

    ![OpencTMSUI](screenshots/OpenCTMSUI.png)

2. In a new tab, you should now see the **Overview** page of your **SAP Cloud Transport Management** service instance. Currently, it looks quite empty which is expected from a new instance. 

    ![cTMSUIAccessSuccessful](screenshots/0310%20cTMS%20UI%20access%20successful.png)


This concludes the tutorial. Congratulations!

For more information, see the [SAP Cloud Transport Management](https://help.sap.com/docs/cloud-transport-management) documentation on SAP Help Portal.