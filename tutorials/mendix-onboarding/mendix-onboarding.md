---
title: Get Started with SAP Cloud Platform Rapid Application Development by Mendix
description: Set up your SAP Cloud Platform account so you can use SAP Cloud Platform Rapid Application Development by Mendix to build applications to be deployed on SAP Cloud Platform.
primary_tag: products>sap-rad-by-mendix
author_name: Paola Laufer
auto_validation: true
tags: [  tutorial>beginner, topic>cloud, products>sap-cloud-platform, products>sap-rad-by-mendix  ]
time: 10
---

## Prerequisites  
  - You have access to an SAP Cloud Platform account. If you don't have an account, you can open a trial one. See the [tutorial](https://developers.sap.com/tutorials/hcp-create-trial-account.html) or [documentation](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/d61c2819034b48e68145c45c36acba6e.html#loio42e7e54590424e65969fced1acd47694).
  - You are using a Windows desktop.

## Details  
### You will learn  
- How to set up your environment to use SAP Cloud Platform Rapid Application Development
- How to open SAP Cloud Platform Rapid Application Development

You can build business applications for the SAP Cloud Platform Cloud Foundry environment using SAP Cloud Platform Rapid Application Development by Mendix, without needing to write code.

In this tutorial, you will set up your SAP Cloud Platform account so you can use SAP Cloud Platform Rapid Application Development to build applications to be deployed on SAP Cloud Platform.

>This tutorial assumes that you are using a trial Cloud Foundry environment.

> The Mendix Studio Pro, for building your application, is available for Windows platforms only.


---

[ACCORDION-BEGIN [Step 1: ](Cloud Foundry trial)]
Go to [https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial](https://cockpit.hanatrial.ondemand.com/cockpit/#/home/trial).

Log into your SAP Cloud Platform account.

!![SAP Cloud Platform](mendix-onboarding-newLogIn.png)

On the welcome screen of the SAP Cloud Platform cockpit, click **Enter Your Trial Account** to see your global account.

!![enter trial account](enter-trial-new.png)
> Bookmark the link for fast and quick access to the cockpit.  

The global trial account contains **one** subaccount and space. Navigate to subaccount by clicking on the tile named **trial** (this name may vary if you created the subaccount manually).

!![enter subaccount](global-account-new.png)


To get to the space in which your applications and services live, click **dev** in the **Spaces** section.

!![enter space](sub-account22.png)


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Open the Mendix portal)]
Go to the [Discovery Center](https://discovery-center.cloud.sap/#/servicessearch/mendix) and click on the **Rapid Application Development by Mendix** tile.

Under the **Tools** section, click **Mendix Development Portal**.


!![Rapid Application Development](opening-mendix-devportal.png)


This opens the registration page.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create a Mendix account)]
Choose to **Sign in**.

!![Sign in](portal1.png)

Choose to **sign in with SAP**.

!![Sign in with SAP](mendixSigninWithSAP-New.png)

Choose your SAP Cloud Platform region from the drop-down and click **Select region**.

!![Select region](mendixSelectRegion-New.png)

This will redirect you to SAP Cloud Platform's login page. Enter your SAP Cloud Platform credentials and sign in.

!![Login SAP](CP-signin.png)

Choose **Authorize**.

!![Authorize](mendixSAPAuthorize-New.png)

Choose **Confirm**.

!![Confirm](mendixConfirm-New.png)

Enter your name and a password and choose **Create**.

!![Register](mendixCreateUser-New.png)

Select the best option and choose **Next**.

!![Skip step](mendixSkipStep-New.png)

Select the reason you signed up for Mendix and choose **Submit**.

!![Reason](mendixReason-New.png)

Now you're in the Mendix development portal.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create app and environment)]
You need to set up an environment for your SAP app. An environment points to the Cloud Foundry environment that you want to deploy to.

You can do this now, or you can do this when you are ready to deploy. When you deploy, you will be prompted to create an environment. Environments are created for each project.

In the development portal home page, choose **Create App**.

!![Open Mendix portal](mendix-onboarding7-New.png)

In the **App Templates** section, enter `SAP Fiori` to see the related templates.

!![SAP templates](find-template.png)

Choose the **Blank App for SAP Fiori themed apps** template.

!![Choose template](select-template.png)

Choose **Select Template** to view the template details.

!![Create app](open-details.png)

Choose **Select This Template**.

!![Create app](see-details.png)

Enter a name, and choose **Create App**.

!![Enter name](add-name.png)

Select a region for SAP Cloud Platform and choose **Next**.

!![Sign in to SAP Cloud Platform](mendix-onboarding11-New.png)

Choose the Cloud Foundry environment to which you want to deploy.

- Select **cfapps.YourRegion.hana.ondemand.com** for the domain. For example, `cfapps.eu10.hana.ondemand.com`.

-	Select **hanatrial-schema** for the database.

Choose **Create**.

!![Choose Cloud Foundry environment](add-domain.png)

The environment is created. To see it, go to **Environments**.

!![Environments](mendix-onboarding13b-new.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Build your app)]
To start working on your app, choose **Edit App**.

Then choose **Edit in Mendix Studio Pro**.

!![Build app](mendix-onboarding14-new.png)

You build apps using the Mendix Studio Pro, which is an application installed on your desktop. If you don't have the Mendix Studio Pro, install it by going to the [app store](https://appstore.home.mendix.com/link/modeler/).

!![Mendix Studio Pro](mendixModeler.png)


[VALIDATE_5]

[ACCORDION-END]
