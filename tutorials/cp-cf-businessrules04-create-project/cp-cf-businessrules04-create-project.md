---
title: Create a Project to Author Rules
description: Create a business rules project to determine the equipment for a new employee based on employee details.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud, products>sap-business-technology-platform, products>sap-btp--cloud-foundry-environment]
primary_tag: products>sap-business-rules-service
author_name: Vandana Vasudevan
author_profile: https://github.com/VandanaVasudevan
---
## Prerequisites
 - Ensure that you setup the **Workflow Management** service to access the **Manage Rule Projects** application. For more information, see [Set Up Workflow Management in Cloud Cockpit](cp-starter-ibpm-employeeonboarding-1-setup).

## Details
### You will learn
  - How to create a project to author rules

Business Rules is a capability of SAP Workflow Management service that lets you digitize and automate decision making. You can author and manage your decisions using the **Manage Rule Projects** application of business rules service.

A project is used to configure and manage the entities of business rules. Entities of business rules include data objects, rules, rule sets, and rule services. You can create projects in the **Manage Rule Projects** application, which can be accessed from the **Workflow Management** home screen. For more information, see [Business Rules](https://help.sap.com/viewer/product/BUSINESS_RULES/Cloud/en-US).

[ACCORDION-BEGIN [Step 1: ](Open Manage Rules Project application)]

1. Log on to **Workflow Management** home screen, and choose **Manage Rule Projects** application tile.

    ![Workflow Management - MRP](Create-Screenshot1.png)

2. In the **Manage Projects** screen, choose +.

    ![Create a new project](Create-Screenshot2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enter details of the project)]

Enter the following details of the project:

|  Field Name     | Value
|  :------------- | :-------------
|  Name           | **`DetermineEquipment01`**
|  Label           | **`DetermineEquipment01`**
|  Description    | **`Business rules to determine equipment for the new hire.`**
|  System            | **`Cloud Runtime`**
|  Expression Language    | **`2.0`**

**Label** and **Description** are a mandatory fields. **Description** field does not support special characters.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Save your project)]

Choose **Save**.

![Save the project](Create-Screenshot3.png)

[VALIDATE_1]

[ACCORDION-END]

<p style="text-align: center;">Give us 55 seconds of your time to help us improve</p>

<p style="text-align: center;"><a href="https://sapinsights.eu.qualtrics.com/jfe/form/SV_0im30RgTkbEEHMV?TutorialID=cp-cf-businessrules04-create-project&graphics=true" target="_blank"><img src="https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/data/images/285738_Emotion_Faces_R_purple.png"></a></p>
