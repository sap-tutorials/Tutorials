---
title: Create Workflow Instances for Process Visibility
description: Create workflow instances to see how events from different types of workflow activities combine to provide an overview to analyze the progress and identify inefficiencies in the workflow.
auto_validation: true
time: 10
tags: [tutorial>beginner, products>sap-business-technology-platform]
primary_tag: products>sap-business-technology-platform
author_name: Kavya M Gowda
author_profile: https://github.com/I540620
---
## Prerequisites
 - You have set up the **Workflow Management** service. For more information, see the [Set Up Workflow Management in Cloud Cockpit](cp-starter-ibpm-employeeonboarding-1-setup) tutorial.
 - You have set up the business rules for determining equipment. To setup business rules, see steps 1 to 4 in [Configure Business Rules, Workflow and Process Visibility](cp-starter-ibpm-employeeonboarding-2-configure) tutorial.

## Details
### You will learn
  - How to start a workflow instance for a scenario

Once you have modelled and activated the visibility scenario, navigate to the Monitor Workflows application and start a new instance of the workflow. Once the workflow has started, you will see the start and other events being received in the Event Acquisition application. You can process them using the Monitor Visibility Scenarios application.

---

[ACCORDION-BEGIN [Step 1: ](Start a new instance of workflow)]
1. Log on to the Workflow Management home screen and choose the **Monitor Workflows** tile.

    !![Home screen](FLP.png)

2. Search and select the workflow definition **onboard** for which you want to create an instance and click **Start New Instance**.

    !![New instance creation](Start-New-Instance-03.png)

2. In the available payload, provide your trial email ID in the highlighted snippet and then click **Start New Instance**.

    !![Payload](Payload-04.png)

3. Click **Show Instances** to view the created instance.

    !![Show instances](Show-Instance-05.png)

    You should be able to see the newly created workflow instance.

    !![Show instance details](Show-instances-06.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open new workflow)]

1. Click **Home** and navigate to the home screen.

    !![Home](home.png)

    You should be able to see a new task in the **My Inbox** tile in the running state.

    !![My Inbox](cp-cf-workflowmanagement-runcapex-myinbox.png)

2. Click the **My Inbox** tile to open the application. You can see the new task in the list.

    !![approve](approve-equipment.png)

    Before you **Confirm** the task, access the Event Acquisition application to familiarize yourself with the acquired events. For more information on Event Acquisition application, refer to [Monitor Events Acquired Using the Event Acquisition Application](cp-cf-processvisibility-model-manageevents).

    You can move forward in the workflow by choosing the **Confirm** button in **My Inbox**.

    !![Approve equipment](approve-equipment2.png)

[VALIDATE_1]
[ACCORDION-END]


---
