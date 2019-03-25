---
title: Prepare to Create Workflows in SAP Cloud Platform
description: Enable and configure services and assign the user roles you'll need for creating workflows.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, topic>cloud, products>sap-cloud-platform ]
time: 5
---

## Prerequisites  
 - **Tutorials:** [Sign up for a free trial account on SAP Cloud Platform](hcp-create-trial-account)

## Details
### You will learn  
  - How to enable the workflow service in your SAP Cloud Platform account
  - How to assign the roles you need to develop and use workflow applications

---

[ACCORDION-BEGIN [Step 1: ](Open the SAP Cloud Platform cockpit)]

1. In your Web browser, open the [SAP Cloud Platform](https://account.hanatrial.ondemand.com/cockpit) cockpit. If you do not have a trial account, see Prerequisites.

2. Choose **Neo Trial**.

    ![Choose Neo Trial](choose-neo-trial.png)

3. Select **Services** from the left-hand navigation.

    ![Select Service](select-services.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Enable the Portal service for your account)]

Search for the **Portal** service. Then select it, and choose **Enable**.

![Enable the Portal service](portal-enabled.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Enable SAP Web IDE Full-Stack)]

1. Go back to the services overview by choosing the **Back** button.
2. Search for **SAP Web IDE Full-Stack**. Then select it, and choose **Enable**.
> Note: The workflow features are only available with SAP Web IDE Full-Stack.

    ![Enable SAP Web IDE](webide-enabled.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Enable the workflow service)]

1. Go back to the services overview by choosing the **Back** button.
2. Search for the **Workflow** service. Then select it, and choose **Enable**.
> Note: If the enablement fails, try again – this could be a temporary problem.

    ![Enable the workflow service](workflow-enabled.png)

[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Set up the workflow editor)]

To use the workflow editor, you need to first activate the extension in the SAP Web IDE.

  1. On the **Workflow** tile, choose **SAP Web IDE Full-Stack**.
  2. Open **Tools** | **Preferences**, and then select **Extensions**.
  3. Find the **Workflow Editor** extension in the list, and switch it on.

  4. Choose **Save** and then **Refresh**.

    ![Workflow Editor Feature](workflow-editor.png)

[DONE]
[ACCORDION-END]

---
