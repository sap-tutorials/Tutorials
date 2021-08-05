---
title: Run the Capital Expenditure Live Process
description: Run the capital expenditure live process and approve tasks using My Inbox.
auto_validation: true
time: 15
tags: [ tutorial>beginner, products>sap-business-technology-platform]
primary_tag: products>sap-business-technology-platform
author_name: Deeksha R
author_profile: https://github.com/Deeksha-R
---

## Prerequisites
- [Set Up Workflow Management in Cloud Cockpit](cp-starter-ibpm-employeeonboarding-1-setup).

## Details
### You will learn
  - How to start a workflow instance
  - How to approve tasks in My Inbox

In this tutorial, you start a new instance of the process variant that you have created. This instance will have the total cost of the order as 50000, which requires a manual approval. Upon starting the instance, the task will be available in My Inbox for your approval.

---

[ACCORDION-BEGIN [Step 1: ](Start a workflow instance)]
1. Navigate to the Workflow Management home screen by choosing **home**.

    !![Home](cp-cf-workflowmanagement-runcapex-home.png)


2. In the Workflow Management home screen, choose the **Monitor Workflow - Workflow Definitions** tile.

    !![Workflow Definitions](cp-cf-workflowmanagement-runcapex-wfdef.png)

3. Choose the **High Value Investments** workflow definition, then choose **Start New Instance**.

    !![Start New Instance](cp-cf-workflowmanagement-runcapex-startinstance.png)

4. In the **Start New Instance** popup menu, replace the existing JSON snippet with the below snippet. Then, replace the **`UserId`** field with your SAP trial email ID. Finally, choose **Start New Instance and Close**.

    ```JSON
    {
      "RequestId": "IAP-2020-180",
      "Title": "App Creation",
      "Requester": {
        "FirstName": "John",
        "LastName": "Doe",
        "Email": "John.Doe@example.com",
        "UserId": "jdoe",
        "Comments": "Please Approve"
      },
      "Investment": {
        "TotalCost": 50000,
        "Type": "Software",
        "CAPEX": 10000,
        "OPEX": 2000,
        "ROI": 5,
        "IRR": 5,
        "Country": "Germany",
        "BusinessUnit": "Purchasing",
        "Description": "Provide a fresh experience for our customers by providing new apps for our services"
      },
      "Sustainability": {
        "EnergyEfficiency": 10,
        "CO2Efficiency": 20,
        "EnergyCostSavings": 15,
        "WaterSavings": 10
      },
      "internal": {

      }
    }
    ```

      !![Payload](cp-cf-workflowmanagement-runcapex-payload.png)

4. Choose **Show Instances**.

    !![Show Instance](cp-cf-workflowmanagement-runcapex-showinstance.png)


You can view the workflow instance created for approval. You can navigate to the **Execution Log** to ensure that the instance is at the local manager approval stage as shown.

!![Instance Details](cp-cf-workflowmanagement-runcapex-instancedetails.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Approve tasks)]
  In this step, you can first approve the task created as a local manager. After the approval, the process moves to the next approval step to the CFO approval. You will again receive a task in My Inbox, where you can approve the task to complete the capital expenditure approval process.

1. Navigate to Workflow Management home screen, choose **My Inbox** tile.

    You can see that there is one task that requires your approval.

    !![My Inbox](cp-cf-workflowmanagement-runcapex-myinbox.png)

2. Choose the approval task from the **All Tasks** list. You can view details of the task that requires your action such as, Investment Details, Sustainability, Investment Requester, History, and Comments.

    !![All Tasks](cp-cf-workflowmanagement-runcapex-myinbox2.png)

3. Choose **Approve** to approve the capital expenditure request.

    !![Approve Task](cp-cf-workflowmanagement-runcapex-myinbox3.png)

    >Similarly, after the local manager approval, you would have a new task in the **My Inbox** tile for your approval as a CFO.

    >!![CFO Approval](cp-cf-workflowmanagement-runcapex-CFO.png)

This completes your sample capital expenditure live process approval.

[VALIDATE_1]
[ACCORDION-END]

---
