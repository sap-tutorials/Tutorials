---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Prepare for SAP BTP Development
description: Learn how to prepare SAP BTP and Cloud Foundry for application deployment.
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, topic>node-js, products>sap-business-technology-platform, products>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Set Up Local Development using VS Code](btp-app-set-up-local-development)
 - [Create a Directory for Development](btp-app-create-directory)
 - [Create a CAP-Based Application](btp-app-create-cap-application)
 - [Create an SAP Fiori Elements-Based UI](btp-app-create-ui-fiori-elements)
 - [Add Business Logic to Your Application](btp-app-cap-business-logic)
 - [Create a UI Using Freestyle SAPUI5](btp-app-create-ui-freestyle-sapui5)
 - [Add More Than One Application to the Launch Page](btp-app-launchpage)
 - [Implement Roles and Authorization Checks in CAP](btp-app-cap-roles)
 - You have an SAP BTP global account, a subaccount, and a Cloud Foundry space with the required entitlements to deploy the application.

## Details
### You will learn
 - How to create an account for SAP BTP
 - How to configure Cloud Foundry in your SAP BTP subaccount
 - How to assign entitlements

---

[ACCORDION-BEGIN [Step 1: ](Overview)]

> ### To earn your badge for the whole mission, you'll need to mark all steps in a tutorial as done, including any optional ones that you may have skipped because they are not relevant for you.

You need an SAP BTP account to deploy the services and applications.
In general, you have a choice of the following options:

**Trial:** *(recommended)* Use a trial account if you just want to try out things and don't want to use any of the parts of this tutorial productively. The usage is free of cost and all the services that you need for this tutorial gets automatically assigned to your trial account.

**Live:** There are multiple live landscapes available in different data centers around the globe. Live landscapes are intended for productive usage and development projects.


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Create a Trial account)]

You can [register for a trial account](https://www.sap.com/cmp/td/sap-cloud-platform-trial.html) and access it [here](https://cockpit.hanatrial.ondemand.com/cockpit#/home/trial).

A global account, a subaccount, a Cloud Foundry org, and space with some entitlements that are sufficient to do this tutorial are set up for you.

> If you use an existing `trial` account then service entitlements may be missing. If you encounter such situation, we suggest deleting your `trial` account and create a new one.

> New service offerings are not added to existing `trial` accounts automatically.

> In case you face a problem when creating a service instance or an application is missing for subscription later in the tutorial, please do the following:

> 1. Go to your **trial** subaccount.
> 2. Choose **Entitlements**.
> 3. Choose **Configure Entitlements**.
> 4. Choose **Add Service Plans**.
> 5. Search for the missing Service Plans and add it with **Add <x> Service Plans**.
> 6. Choose **Save**.

Continue with step **Log on from the Command Line** at the end of this tutorial.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Create a Live Account)]

If you choose to create an account on Live, you have to select a number of services that you need to subscribe to, for example, an SAP HANA database. For each service, there are so-called `entitlements`, which are basically the service plans and the number of units that you want from each service, when you create an account you need to provide these also.

The following services with their service plans and entitlements are required for the different modules of the tutorial and will be needed to create the global account and subaccount.

<!-- external version with reduced content according the external scenarios -->

| Service                           | Plan       | Amount | Unit         | Tutorial                                |
| --------------------------------- | ---------- | ------ | ------------ | --------------------------------------- |
| Application Runtime               | `MEMORY`     | 4      | GB           | [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment)   |
| Application Logging               | `standard`   | 1      | instances    | [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment)   |
| SAP HANA Schemas & HDI Containers | `hdi-shared` | 1      | instances    | [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment)   |
| SAP HANA Cloud                    | `hana`       | 1      | instances    | [Set Up the SAP HANA Cloud Service](btp-app-hana-cloud-setup)     |
| Launchpad Service                 | `standard`   | 1      | active users | [Add the SAP Launchpad Service](btp-app-launchpad-service) |


> The services mentioned below are Utility Services, no entitlement needed

| Service                          | Plan        | Amount | Unit         | Tutorial                                |
| -------------------------------- | ----------- | ------ | ------------ | --------------------------------------- |
| Launchpad Service                | `standard`    | 1      | active users | [Add the SAP Launchpad Service](btp-app-launchpad-service) |
| Authorization & Trust Management | `application` | 1      | instances    | [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment)   |



At least the services for the tutorials [Set Up the SAP HANA Cloud Service](btp-app-hana-cloud-setup) and [Deploy Your Multi-Target Application (MTA)](btp-app-cap-mta-deployment) need to be entitled.

[VALIDATE_1]

[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Create a Global account)]

As the result of the previous steps there will be a Cloud Foundry space with required entitlement to deploy the service. This requires the creation of an SAP BTP global account, subaccount, space, and the assignment of the required entitlements.

1. Go to **SAP BTP Control Center**:

    [https://int.controlcenter.ondemand.com/index.html](https://controlcenter.ondemand.com/index.html)

2. Choose the ( &#x2B; ) button in the upper right corner.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Enter account info)]

1. Enter a global account name.

2. Enter a description.

3. (Optional) If you have a Service Inventory ID, choose the radio button **Yes** under the section "Service Provider Account" and enter it.

4. Choose **Next**.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 6: ](Enter business info)]

1. Enter the cost center number.

2. Read & check the disclaimer.

3. Choose **Next**.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 7: ](Assign services)]

1. Select the services specified in the table in the previous step **Create a Live Account**.

2. Choose **Next**.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 8: ](Set entitlements)]

1. Set the entitlements from the table in the previous step **Create a Live Account**.

2. Choose the **Add** button for each of them.

3. Choose **Create Account**.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 9: ](Create subaccount)]

1. Go to **SAP BTP Control Center**:

    [https://int.controlcenter.ondemand.com/index.html](https://controlcenter.ondemand.com/index.html)

2. Select your global account in the dropdown menu.

3. Choose **Open in Cockpit**.

    !![Open global account in cockpit](open_global_account.png)

4. Choose **Subaccounts** in the left navigation pane.

5. Choose the **New Subaccount** button.

6. To fill the **New Subaccount** dialog, enter a **Display Name**.

    > Use a short name for your project and add the prefix for the landscape, for example: `<project name>-cf-eu10`. Don't select the checkbox **Neo**!

7. Enter a subdomain.

    > Only valid HTTP domain characters are allowed.

8. Choose **Create**.

9. Wait for the completion of the subaccount creation.

10. Choose the tile with your new subaccount.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 10: ](Configure Cloud Foundry in your subaccount)]

This creates a [Cloud Foundry (CF) Org](https://docs.cloudfoundry.org/concepts/roles.html#orgs) in your subaccount. There's always one Cloud Foundry org per subaccount. Later on, when you log on to Cloud Foundry, it asks you which Cloud Foundry org you want to log on to. For any development in you subaccount, you need to choose this org for your subaccount.

1. Choose **Enable Cloud Foundry**.

2. Enter a **Cloud Foundry Org Name** and choose **OK** (suggestion: use `SAP_` as prefix, for example: `SAP_<project name>`).

3. (Optional) In the left navigation pane under the section **Cloud Foundry**, choose **Org Members** and add users for your new account. Your own user should already be there. You can add other users if needed and assign a role to them.

4. On the left-hand pane under **Security**, choose **Administrators** and add the required security administrators. Again, your user should be-filled. You can add other users if needed.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 11: ](Assign entitlements)]

In this section, you assign a portion of the entitlements that you've bought for your global account to the individual subaccounts. In this, you have only one subaccount. If you have 3 subaccounts, for example, and have bought 100 units of the HTML5 service, you could assign 50 units to the first subaccount, 20 to the second, and the remaining 30 to the third subaccount.

1. In your subaccount, choose **Entitlements** in the left-hand pane.

2. Choose **Configure Entitlements**.

3. Choose **Add Service Plans**.

4. Go through the Entitlements according to the table in the previous step **Create a Live Account** and add the required plans for each of them.

5. Choose the + or - symbol to change the quota for the services according to the table in the previous step **Create a Live Account**.

6. Choose **Save**.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 12: ](Create a Cloud Foundry space)]

Next to Cloud Foundry orgs there are also [Spaces](https://docs.cloudfoundry.org/concepts/roles.html#spaces). Each Cloud Foundry org can have 0 to n spaces, you create just one here.

1. Open the subaccount page in the SAP BTP cockpit.

2. Choose **Spaces** in the left pane under the section **Cloud Foundry**.

3. Choose **Create Space**.

4. Enter a space name (suggestion: If different sub projects exist in the org: `<sub project name>-<stage name>`, otherwise: `<stage name>`; where stage name is the release stage, for example: `dev`, `val`, `prod`)

5. Choose **Create**.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 13: ](User assignment in the Cloud Foundry space)]

1. Open the space page in the SAP BTP cockpit (you can just choose the tile for the space that was created).

2. Choose **Members** in the left navigation pane.

3. (Optional) Choose **Add Members** to add all required users, again your own user should already be part of the list.

4. (Optional) Add a DL of your `CFDeployment` technical user as Space Manager if you have one.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 14: ](Log on from the command line)]

[OPTION BEGIN [Trial]]

Prepare for the next steps by logging on to Cloud Foundry and targeting your space in the account.


If you don't know whether you're logged on to Cloud Foundry or if you're wondering to which Cloud Foundry org and space are you logged on, you can always use `cf target` in a terminal to find out. If you aren't logged on already, go to your SAP BTP Cockpit by using one of the following links, depending on the landscape you want to deploy to SAP BTP cockpit:

[https://cockpit.hanatrial.ondemand.com/](https://cockpit.hanatrial.ondemand.com/)

1. Select your **Global Account** and then the **Subaccount** to which you want to deploy your service and application.

2. On the subaccount page, gather all the data to log in to Cloud Foundry (CF):

    - `API Endpoint`
    - `Org Name`
    - `Space Name`

    !![CF Data in SAP BTP Cockpit](cfdatacloudcockpit.png)


    For your convenience, this is the API endpoint for your landscape:

    [https://api.cf.eu10.hana.ondemand.com](https://api.cf.eu10.hana.ondemand.com)
3. Open a terminal.

4. Set the Cloud Foundry API endpoint:

    ```Shell/Bash
    cf api <API Endpoint of your landscape>
    ```

5. Log in to your Cloud Foundry account, using your SAP BTP credentials:

    ```Shell/Bash
    cf login
    ```



[OPTION END]
[OPTION BEGIN [Live]]

Prepare for the next steps by logging on to Cloud Foundry and targeting your space in the account.


If you don't know whether you're logged on to Cloud Foundry or if you're wondering to which Cloud Foundry org and space are you logged on, you can always use `cf target` in a terminal to find out. If you aren't logged on already, go to your SAP BTP Cockpit by using one of the following links, depending on the landscape you want to deploy to SAP BTP cockpit:

[https://account.hana.ondemand.com/](https://account.hana.ondemand.com/)


1. Select your **Global Account** and then the **Subaccount** to which you want to deploy your service and application.

2. On the subaccount page, gather all the data to log in to Cloud Foundry (CF):

    - `API Endpoint`
    - `Org Name`
    - `Space Name`

    !![CF Data in SAP BTP Cockpit](cfdatacloudcockpit.png)


    For your convenience, this is the API endpoint for your landscape:

    [https://api.cf.eu10.hana.ondemand.com ](https://api.cf.eu10.hana.ondemand.com)


3. Open a terminal.

4. Set the Cloud Foundry API endpoint:

    ```Shell/Bash
    cf api <API Endpoint of your landscape>
    ```

5. Log in to your Cloud Foundry account, using your SAP BTP credentials:

    ```Shell/Bash
    cf login
    ```



[OPTION END]


[DONE]
[ACCORDION-END]
---
