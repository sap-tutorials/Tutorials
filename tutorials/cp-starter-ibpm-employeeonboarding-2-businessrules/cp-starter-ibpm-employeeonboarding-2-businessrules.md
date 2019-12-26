---
title: Set Up Business Rules for Determining Equipment
description: Import, configure, and deploy business rules to determine equipment for new hire using SAP Cloud Platform Business Rules
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform
---

## Details
### You will learn
- How to quickly set up and deploy business rules in your account

**SAP Cloud Platform Business Rules** is a platform service, which enables business users and application developers to automate their decisions and enhance their decision-making experience. It offers a simplified Fiori-based rule authoring tool and a central rule repository with various deployment options to different `runtimes`.

In this sample content, you will learn how to author and manage rules to identify the list of equipment to be provisioned for the newly hired employee based on their role and work location using SAP Cloud Platform Business Rules.

---


[ACCORDION-BEGIN [Step 2: ](Configure a system)]

To import the business rules project from API Business Hub, you need to configure a system which points to the API Hub destination and then use this system to import the project.

1. In your web browser, open the business rules editor.

    >You should have gotten the business rules editor link in the last step of the setup tutorial. The link should be like this:

    >`https://trial-userid-dev-bpmservicesflp-approuter.cfapps.eu10.hana.ondemand.com/comsapbpmrule.ruleeditor/index.html`

2. Open **Configure Systems**.

3. Click **+** to add a system.

    ![Create System](CreateSystem.png)

4. Enter the following details:

    |  Field Name     | Value
    |  :------------- | :-------------
    |  Name           | `APIHub`
    |  Destination    | `BUSINESSRULES_APIHUB` or the name of the destination you created in the setup tutorial

5. **Save** the system configuration.

    ![Create System](CreateSystem_2.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Import business rules)]

1. Click **Design** icon and select **Import Project from API Business API Hub**.

    ![Import Business Rules](ImportAPIHub.png)

2. In the popup, do the following:
    - In the **System** field, select `APIHub` or the name you entered while configuring the system.
    - From the options shown in the **Content Package Name** dropdown, choose `Equipment Determination Business Rules`.
    - Choose **Go**.
    - Select the latest version of the project named `Equipment Determination Rules`.  
    - Choose **Import**.

    ![Import Business Rules](ImportRules.png)

3. You will see the business rules project imported and in **Active** status.

    ![Import Business Rules](ImportRules_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Deploy business rules)]

1. Select the business rules project to navigate into the project.

    ![Deploy Business Rules](deploy-ruleservice_1.png)

2. Switch to the **Rule Services** tab.

3. Click **Deploy** to deploy rule services `DetermineEquipmentRuleservice` and `EquipmentApprovalRuleservice`

    ![Deploy Business Rules](deploy-ruleservice_2.png)

4. Select the **SAP Cloud Platform** system.

    ![Deploy Business Rules](deploy-ruleservice_3.png)

5. Wait for the deployment to complete. You will get a popup message once the deployment is successful.

> Only deployed business rules can be consumed via APIs published in API Business Hub.

[VALIDATE_6]
[ACCORDION-END]

---
