---
title: Create a Service Instance of SAP Cloud Platform Business Rules
description: Create a service instance using SAP Cloud Platform cockpit to enable the Business Rules service.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-business-rules
author_name: Vandana Vasudevan
author_profile: https://github.com/VandanaVasudevan
---

## Prerequisites
 - **Tutorials:** [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account)
 - Configure entitlement for **Business Rules** in your subaccount. For more information, see [Configure Entitlements and Quotas for Subaccounts](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/5ba357b4fa1e4de4b9fcc4ae771609da.html)

## Details
### You will learn
  - How to enable Business Rules service
  - How to create a service instance of Business Rules

SAP Cloud Platform Business Rules is an Intelligent Business Process Management service that lets you digitize and automate decision making. You can author and manage your decisions using the **Manage Rules Project** application of business rules service. For more information on SAP Cloud Platform Business Rules, see [SAP Cloud Platform Business Rules](https://help.sap.com/viewer/product/BUSINESS_RULES/Cloud/en-US).

To get started with SAP Cloud Platform Business Rules, you have to create a service instance of business rules. Also, you can create the service key of the business rules service instance, which lets you consume the business rules API from SAP API Business Hub or any REST clients. You will need the service instance and service keys in the tutorials that follow.

[ACCORDION-BEGIN [Step 1: ](Open SAP Cloud Platform cockpit)]

1. In your Web browser, open the [SAP Cloud Platform](https://account.hanatrial.ondemand.com/cockpit) cockpit. If you do not have a trial account, see Prerequisites.

2. Choose **Enter Your Trial Account**.

    ![Cockpit landing page](landing_page.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open Business Rules tile)]

1. Choose your subaccount.

    ![Trial account](enablebr_1.png)

2. Navigate to **Spaces** and then choose your space.

    ![choose spaces](enablebr_2.png)   

3. In the navigation area, choose **Services** > **Service Marketplace** , search for business rules and then choose **Business Rules** tile.

    ![enable business rules](enablebr_3.png)

    The following service overview page opens:

    ![Service overview](enablebr_4.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a service instance)]

1. In the navigation area, choose **Instances** and then choose **New Instance**.

    ![New Instance](enablebr_5.png)

2. Choose the service plan **lite** and choose **Next**.

    ![Lite plan](enablebr-6.png)

3. No parameters are needed as shown in the following image, so choose **Next**.

    ![parameters](enablebr-7.png)

4. In the next screen, choose **Next** as we don't need to bind any application.

    ![No applications](enablebr-8.png)

5. In the **Instance Name** field, provide an **Instance Name** of your choice and then choose **Finish**.

    ![Instance name](enablebr-9.png)

    >Note the service instance name as it is required to access the Manage Rules Project application to author and manage business rules.

    The new instance is displayed in the list and the status present under the **Last Operation** changes to Created.

    ![Service instance created](enablebr_10.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create service key for service instance)]

1. Choose the business rules service instance that you created.

    ![choose service instance](service-instance01.png)

2. In the navigation area, choose **Service Keys** and then choose **Create Service Key**.

    ![service keys](service-instance02.png)

3. Provide a **Name** of your choice and then choose **Save**.

    ![service key name](service_instance03.png)

    The service key is generated as shown:  

    ![service key](service_instance04.png)


[VALIDATE_1]
[ACCORDION-END]
