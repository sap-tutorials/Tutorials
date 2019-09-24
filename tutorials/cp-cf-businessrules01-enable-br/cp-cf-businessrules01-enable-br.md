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

## Details
### You will learn
  - How to enable Business Rules service
  - How to create a service instance of Business Rules

You can create a service instance of business rules to get started with SAP Cloud Platform Business Rules. Also, you can create the service key of the business rules service instance, which lets you consume the business rules API from SAP API Business Hub or any REST clients. You will need the service instance and service keys in the tutorials that follow.

[ACCORDION-BEGIN [Step 1: ](Create SAP Cloud Platform trial account)]

Open [SAP Cloud Platform Cockpit](https://cockpit.hanatrial.ondemand.com) and then create a trial account in the Cloud Foundry environment. For more information, see [Get a Trial Account](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/42e7e54590424e65969fced1acd47694.html?q=create%20a%20trial).

If you have already created a trial account in the Cloud Foundry environment, perform the following steps:

1. Choose your trial account.

    ![Trial account](enablebr-1.PNG)

2. In the navigation area, choose **Spaces** and then choose your space.

    ![choose spaces](enablebr-2.PNG)   


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create service instance of Business Rules)]


1. In the navigation area, choose **Service Marketplace** and then choose **Business Rules** tile.

    ![enable business rules](enablebr-3.PNG)

    The following service overview page opens:

    ![Service overview](enablebr-4.PNG)

2. In the navigation area, choose **Instances** and then choose **New Instance**.

    ![New Instance](enablebr-5.PNG)

3. Keep the default options as is, provide an **Instance Name** of your choice, then choose **Finish**.

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
