---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Set Up SAP Business Application Studio
description: This tutorial shows you how to set up your environment for application development to get started with this tutorial using SAP Business Application Studio.
auto_validation: true
time: 10
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, topic>node-js, products>sap-business-technology-platform, products>sap-fiori, products>sap-business-application-studio]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - You have an SAP BTP global account, a subaccount, and a Cloud Foundry space with the required entitlements to access SAP Business Application Studio

## Details

### You will learn
 - How to set up your environment for application development
 - How to create and use a dev space


---

[ACCORDION-BEGIN [Step 1: ](Using SAP Business Application Studio)]

As mentioned in section [Editors](btp-app-set-up-local-development#editors), you can also use SAP Business Application Studio instead of VS Code. [SAP Business Application Studio](https://help.sap.com/viewer/9d1db9835307451daa8c930fbd9ab264/Cloud/en-US) is the successor of the SAP Web IDE. Like the SAP Web IDE, it's mostly an online IDE but a local version is also available. Many of the so-called extensions that are available for [VS Code](btp-app-set-up-local-development#using-visual-studio-code-vs-code) are also available for SAP Business Application Studio and vice versa.

If you decide to use SAP Business Application Studio, your benefits are:

- You don't have to install or update any of the packages yourself. They all come with your SAP Business Application Studio instance.
- In some cases, you have additional productivity features that are available only in SAP Business Application Studio but not in VS Code.

If you go for SAP Business Application Studio, you first have to get access to SAP BTP, it can only be used with a user there. At a later stage of the tutorial, this is required anyway.

> _You can still perform the following steps locally and without any access the SAP BTP_

> - Create a CAP application with its own persistency and access to a remote SAP S/4HANA system
> - Create an SAP Fiori elements and a SAPUI5 application on top of the application

If you're looking for a deployment to SAP BTP (obviously), want to use the SAP Fiori launchpad, develop a multi tenant app, and use workflow, you'll need SAP BTP access later anyway.

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 2: ](Set Up Business Application Studio in your SAP BTP Account)]

[OPTION BEGIN [Trial]]

If you don't have an SAP BTP account already, see tutorial [Prepare for SAP BTP Development](btp-app-prepare-btp) for the choices and how to set it up. Afterwards you either have a Trial or Live account.

1. Go to [SAP BTP Cockpit Trial](https://cockpit.hanatrial.ondemand.com/).
2. Choose **SAP Business Application Studio** under **Quick Tool Access**.

    !![SAP BAS Quick Access](sap_btp_bas_quick_access.png)

    > _SAP BTP trial accounts get a subscription to SAP Business Application Studio out of the box. Please note that you're using the trial version of SAP Business Application Studio. See section [Restrictions](https://help.sap.com/viewer/9d1db9835307451daa8c930fbd9ab264/Cloud/en-US/a45742a719704bdea179b4c4f9afa07f.html) in the SAP Business Application Studio documentation for more details on how your development environment can be affected._


[OPTION END]
[OPTION BEGIN [Live]]

If you don't have an SAP BTP account already, see tutorial [Prepare for SAP BTP Development](btp-app-prepare-btp) for the choices and how to set it up. Afterwards you either have a Trial or Live account.

1. Go to [SAP BTP Cockpit](https://account.hana.ondemand.com/) and choose the **Global Account** that you've created.
2. Choose your subaccount.

> _Don't forget to create an `entitlement` for SAP Business Application Studio as well, as shown for other `entitlements` in [Prepare for SAP BTP Development](btp-app-prepare-btp#required-service-entitlements)._

First, subscribe to SAP Business Application Studio:

1. In your subaccount, choose **Service Marketplace** in the left-hand navigation.
2. Search for **SAP Business Application Studio**.
3. Choose **Create**.

    !![SAP Business Application Studio Subscription](sap_btp_bas.png)

4. Choose `standard` as **Plan**.

5. Choose **Create**.

Then, assign role collections:

6. Navigate back to your subaccount overview with the breadcrumbs.
7. On the left-hand pane, expand **Security** and then choose **Trust Configuration**.
8. Choose `SAP ID Service`.

9. Enter your E-Mail address with which you're registered for SAP BTP.
10. Choose **Assign Role Collection** and then add the following role collections one by one, by choosing **Assign Role Collection**.
    - `Business_Application_Studio_Administrator`
    - `Business_Application_Studio_Developer`
    - `Business_Application_Studio_Extension_Deployer`

[OPTION END]


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Create a Dev Space)]

> _If you have already accessed SAP Business Application Studio, continue with step 5 here._

11. Navigate back to your subaccount overview with the breadcrumbs.
12. On the left-hand pane, choose **Instances and Subscriptions**.
13. Choose **Go to Application** in the **Subscriptions** category for **SAP Business Application Studio** application or choose the !![SAP Business Application Studio](go_to_bas.png) icon.

    !![SAP Business Application Studio](bas_go_to_application.png)

14. Log in with your SAP BTP account user and password.

    > _Save the URL in your favorite link list. You'll need this URL again._

16. On the Dev Spaces selection UI, choose **Create Dev Space**.

17. Enter `cpapp` as the Dev Space name.

18. Choose **Full Stack Cloud Application** as the application kind.

19. Choose **Workflow Management** as additional SAP Extensions you want to install.

20. Choose **Create Dev Space**.


!![SAP Business Application Studio Dev Space](bas_dev_space.png)

Wait until the Dev Space has been created. This can take some time.

[VALIDATE_1]

[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ](Open the Dev Space in SAP Business Application Studio)]

1. Open the Dev Space by choosing its name.
2. In the opened editor, choose **Open Folder**.
3. Select the `projects` folder in the **Open Folder** dialog.
4. Choose **Open**.

!![SAP Business Application Studio Open Workspace](bas_open_projects.png)

SAP Business Application Studio will refresh and select the `projects` folder as the workspace root.

[DONE]
[ACCORDION-END]
---
