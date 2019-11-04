---
title: Create Your Workflow Project and Its Module
description: Create a workflow project as well as a module using SAP Web IDE.
auto_validation: true
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, products>sap-cloud-platform ]
time: 5
---

## Details
### You will learn  
  - How to create ``multitarget`` application projects for your workflow in the SAP Web IDE.
  - How to deploy these projects to the SAP Cloud Platform.

---
[ACCORDION-BEGIN [Step 1: ](Create a workflow project)]
1. In your web browser, open the cockpit of [SAP Cloud Platform Trial](https://account.hanatrial.ondemand.com/cockpit).

2. Choose **Launch SAP Web IDE**.

    ![Launch SAP Web IDE](launchsapwebide.png)

3. In the navigation area of SAP Web IDE, choose the **Development** icon.

4. Under **Files**, right-click **Workspace** and choose **New** | **Project from Template**.

    ![Create Workflow Project](choose-workspace2.png)

5. In the wizard, set **Environment** to **Cloud Foundry** and select the **Multi-Target Application** template. Then choose **Next**.

    ![Choose Multi-Target Application](select-app.png)

6. Enter a name for the new workflow project, for example, `MyWorkflowProject`, and choose **Next**.

    ![Enter Name](enter-name.png)

7. Do **not** select **Use HTML5 Application Repository**, and choose **Finish**.  

    ![Enter Workflow Name](enter-wf-name.png)

You now see a project with an `mta.yaml` file in your workspace:

![Workflow Project](workflow-project2.png)

> If there are layout issues with the editor window (can't see the properties), simply refresh the SAP Web IDE window (F5).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create a workflow module)]

1. Select and right-click your project, then choose **New** | **Workflow Module**.

    ![Create Module](create-module2.png)

2. Enter the name `MyWorkflow`, then choose **Next**.

    ![Enter Module Name](enter-module-name-corr.png)

3. Enter the workflow name `ApprovalWorkflow`, then choose **Finish**.

    ![Enter Workflow Name](enter-workflow-name2.png)

You now see your new workflow:

![Workflow](workflow-created2.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Adapt the MTA Workflow project)]
1. In your workspace under **`MyWorkflowProject`**, right-click the ``mta.yaml`` file and choose **Open MTA Editor**.

    ![Open MTA Editor](open-wf-mta-editor.png)

2. Switch to the **Resources** tab, and change the name from `workflow_MyWorkflowProject` to `my-workflow-instance`.

3. Under **Type**, select `org.cloudfoundry.existing-service`.

4. Under **Parameters**, select and delete the key entries `service-plan` and `service`.

5. Save your changes.

    ![Change Resource Name](change-resource-name-corr.png)

6. Switch to the **Modules** tab, and check that `my-workflow-instance (resource)` is listed under **Requires**.

    ![Check Requires Section](check-requires.png)

7. Save your changes.

[VALIDATE_9]
[ACCORDION-END]

---
