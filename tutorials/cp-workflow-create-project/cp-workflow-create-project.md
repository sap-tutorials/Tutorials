---
title: Create a new workflow project from a template
description: Create a new workflow project in SAP Cloud Platform using SAP Web IDE.
primary_tag: products>sap-cloud-platform
tags: [  tutorial>beginner, products>sap-cloud-platform ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  
- To create a project for your workflow in the SAP Web IDE Full-Stack.
- To deploy this project to the SAP Cloud Platform.

### Time to Complete
**5 Min**

---

[ACCORDION-BEGIN [Step 1: ](Access the SAP Web IDE Full-Stack)]
1. In your Web browser, open the cockpit of [SAP Cloud Platform](https://account.hanatrial.ondemand.com/cockpit).
2. Choose **Neo Trial**.
3. Select **Services** from the left-hand navigation.
4. Search for the **Workflow** service.
![Search Workflow](search-workflow.png)
3. On the **Workflow** tile, choose **SAP Web IDE for Full-Stack Development**.
![Choose Full-Stack](choose-full-stack.png)

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a new project)]
1. In the navigation area, choose the home icon.
![Choose Home](choose-home.png)
2. Choose **New Project from Template**.
![Choose New Project](new-project.png)
3. Change the **Category** to **Business Process Management**, and then select the **Workflow Project** template.
![Change Category](select-category.png)
4. Choose **Next**.
5. Enter a name for the new workflow project, for example, `MyWorkflowProject`, and choose **Next**.
![Enter Name](enter-name.png)
6. Enter a name for the new workflow, for example, `MyFirstWorkflow` and a description.
![Enter Workflow Name](enter-wf-name.png)
7. Choose **Finish**.   
   You should now see a project with a workflow file in your workspace:
![Workflow Project](workflow-project.png)

> Note: If there are layout issues with the editor window (can't see the properties), simply refresh the SAP Web IDE window (F5).

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Deploy your workflow)]
1. Select the workflow file in the workspace.
    > Note: Make sure that you really selected the file and not the project.
2. Open the context menu by right-clicking on the project, and select **Deploy** | **Deploy to SAP Cloud Platform Workflow**.
![Deploy Workflow](deploy-workflow.png)
In the top right corner, you see a success notification.

  ![Deploy Notification](deploy-notification.png)

[ACCORDION-END]

---
