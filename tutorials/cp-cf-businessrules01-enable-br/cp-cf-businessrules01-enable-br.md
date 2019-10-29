---
title: Enable the Business Rules Service
description: Enable the Business Rules service of SAP Cloud Platform and create a service instance of Business Rules.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: products>sap-cloud-platform-business-rules
author_name: Vandana Vasudevan
author_profile: https://github.com/VandanaVasudevan
---

## Prerequisites
 - **Tutorials:** [Get a Free Trial Account on SAP Cloud Platform](hcp-create-trial-account)

## Details
### You will learn
  - How to enable Business Rules service
  - How to create a service instance of Business Rules

You can create a service instance of business rules to get started with SAP Cloud Platform Business Rules. Also, you can create the service key of the business rules service instance, which lets you consume the business rules API from SAP API Business Hub or any REST clients. You will need the service instance and service keys in the tutorials that follow.

[ACCORDION-BEGIN [Step 1: ](Open SAP Cloud Platform cockpit)]

1. In your Web browser, open the [SAP Cloud Platform](https://cockpit.hanatrial.ondemand.com/cockpit) cockpit. If you do not have a trial account, see Prerequisites.

2. Choose **Enter Your Trial Account**.

    ![Cockpit landing page](landing_page.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open Business Rules tile)]

1. Choose your subaccount.

    ![Trial account](enablebr-1.PNG)

2. Navigate to **Spaces** and then choose your space.

    ![choose spaces](enablebr-2.PNG)

3. In the navigation area, choose **Service** > **Service Marketplace** and then choose **Business Rules** tile.

    ![enable business rules](enablebr-3.PNG)

    The following service overview page opens:

    ![Service overview](enablebr-4.PNG)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create service instance of Business Rules)]

1. In the navigation area, choose **Instances** and then choose **New Instance**.

    ![New Instance](enablebr-5.PNG)

2. Keep the default options as is, provide an **Instance Name** of your choice, then choose **Finish**.

    ![Instance name](enablebr-6.PNG)

    You can see the instance creation status in the **Business Rules – Instances** page as shown:

    ![Business rules service instance](enablebr-7.PNG)

    You can see the following status once the service instance is created:

    ![Service instance created](enablebr-8.PNG)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Create service key of service instance)]

1. Choose the business rules service instance that you created.

    ![choose service instance](service_instance1.png)

2. In the navigation area, choose **Service Keys** and then choose **Create Service Key**.

    ![service keys](service_instance2.png)

3. Provide a **Name** of your choice and then choose **Save**.

    ![service key name](service_instance3.png)

    The service key is generated as shown:

    ![service key](service_instance4.png)


[VALIDATE_1]
[ACCORDION-END]
