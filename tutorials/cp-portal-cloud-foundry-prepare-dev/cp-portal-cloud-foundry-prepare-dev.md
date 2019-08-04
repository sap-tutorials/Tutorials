---
title: Prepare the SAP Cloud Platform Cloud Foundry Environment for Development
description: To allow developers to create their own content, the subaccount must be configured.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>sapui5, products>sap-cloud-platform, products>sap-fiori, products>sap-cloud-platform-portal, products>sap-web-ide]
primary_tag: products>sap-cloud-platform-for-the-cloud-foundry-environment
---

## Prerequisites
 - You've created a SAP Cloud Platform trial account in the Neo environment so that you can use the SAP Web IDE Full-Stack service to create apps. You can also use a subaccount in your SAP Cloud Platform global account. If you don't have a subaccount on the Neo environment, follow this [tutorial](webide-innovation-beta).
 - If you are using your own Cloud Foundry subaccount (and not a trial account),  your subaccount should have the relevant entitlements and quotas. This should be configured by your Cloud Platform administrator.

## Details
### You will learn
  - How to prepare the Cloud Foundry subaccount for development.
    >Note that if you are working with a trial account, you can skip this tutorial


By default, your subaccount is configured to subscribe to content. If you intend to add your own developed content, you need to configure the subaccount.

Our overall goal in this group of tutorials is to create apps in SAP Web IDE and make them available for use in a Portal site.
 Since you'll be developing and deploying apps in the Cloud Foundry environment, you  will need a Space. The Space provides you with all the services you need to develop your app.

If you are using a trial account, go to Step 1.  If you are using your own subaccount, go to Step 2.



[ACCORDION-BEGIN [Step 1: ](Set up your own subaccount for development )]


1. In your SAP Cloud Platform global account, click your subaccount and then click **Enable Cloud Foundry**.

    ![Enable Cloud Foundry](0005_enable_cloud_foundry.png)

2. In the popup that opens, enter the **Organization Name** as `portal-dev_companyportal` and click **Create**.

    > An Organization is a development account.

    ![Create Organization](0006_organization_name.png)

    Once you've created the Organization, a **Spaces** menu is added to the side navigation panel.

3. Click **Spaces** and then click **New Space**.

    ![Create space](0007_create_space.png)

4. Enter `dev` as the **Space Name** and click **Save**.

    ![Name space](0008_name_space.png)

Before you can continue developing, you should make sure you have enough resources available such as memory. For example, in the image below, you can see that our Space currently has a free memory of 4GB. If you do not have available memory or run into issues, contact your SAP Cloud Platform administrator and ask them to provide the relevant entitlements and quotas. Your subaccount should have the following entitlements:


-	Application Runtime

-	Portal

  ![View quotas and apps](0009_view_quotas.png)

[VALIDATE_7]
[ACCORDION-END]
