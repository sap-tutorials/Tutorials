---
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
title: Run the process
description: Run the process
auto_validation: true
time: 15
tags: [ tutorial>intermediate, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-process-automation
---

## Prerequisites
- [Agent Management settings to execute the process with an automation](spa-run-agent-settings)
- [Install and Setup the Desktop Agent](spa-setup-desktop-agent)

## Details
### You will learn
  - How to release, deploy and run the Process
  - How to work on the Tasks
  - How to monitor the Process

---

[ACCORDION-BEGIN [Step 1: ](Release Business Process Project)]

1. In the Process Builder, choose **Release**.

    !![Release](01_Process_final.png)

2. Add a **Version Comment** if needed and choose **Release**.

   !![Release first](02_Release_first_version.png)

3. If it is an additional version, choose the type of version, add a **Version Comment** if needed and choose **Release**.

   !![Release new](02_Release_second_version.png)

4. The project released successfully and is ready to be deployed.

    > If needed, you can refer to the [Documentation](https://help.sap.com/viewer/DRAFT/d668fd319a104511b515d574782b497f/Dev/en-US/5ec3714e12ce487da35c009505eaf3a5.html).

   !![Released](03_Released_first_version.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Deploy the released Process)]

  You can deploy business process projects from each released version of the project in the Process Builder or through the Lobby.

1. From the released version of the business process project in the Process Builder, choose **Deploy**.

  !![Start Deploy](01_Released_first_version.png)

2. In this case there are no **Variables** to set.
   > ## What's going on?
   > Variables allow you to reuse certain information for a given business process project deployment. You use variables to pass parameters to automations. You can create variables in the Process Builder for which you can later set values when deploying the  business process project.

3. Choose **Confirm**.

   !![Deploy confirm first](02_Deploy_first_version.png)

3. Choose **Deploy**.

   !![Deploy](02_Deploy_first_version_confirm.png)

4. The project deployed successfully and is now ready for running and monitoring.

   !![Deployed](03_Deployed_first_version.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the Business Process)]

From the deployed version of the Business Process Project in the Process Builder, open the process **Get Invoice Details**.

  !![Run](01_Open_Order_Processing.png)

2. Select **Invoice Request Form**.

3. Choose the **Copy** icon aside the **Form Link** in the **Trigger Settings**.

    !![Run copy the form link](02_Process_Start.png)

3. Open the Form pasting the **Form Link** in a browser window.

    !![Run open the form](03_Order_Processing_Form.png)

4. Fill the **Invoice Request Form** and choose **Submit**.

  > Do not enter any random value or else the Automation will not give any results.

    !![Run open the form](Form_Inputs.png)

5. The process is triggered. You can now work on the tasks and monitor the process.

    !![Run form successfully submitted](Run_inputs_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Work on the Tasks)]    

1. Start in the **Lobby** and open the **My Inbox** application by selecting the button ![Inbox Icon](02_Inbox_Icon.png) at the top right corner.

    !![Lobby](01_Lobby.png)

2. After opening the **My Inbox** application, you will see on the left-hand side all the tasks listed. Select the task with the invoice number with which you triggered the process.

    !![My Inbox Actions](03_MyInbox_Actions.png)

3. Move on with one of the actions:

    !![Task Actions](04_TaskActions.png)

      - **Approve**, **Reject**, **Show Log** (to see what has been done so far),
      - **Claim** (to reserve this task for you) or
      - **Mail** (to forward this task via email).


4. You could also **sort**, **filter** or **group** the tasks at the bottom of the task list with these buttons:

      !![My Inbox Filter](05_MyInbox_Filter.png)

5. Depending on your selected actions and the information you have provided at the start of the process, the next task would be to **confirm** the invoice.

      !![Confirmation Form](06_ConfirmationForm.png)

      > You have accessed the tasks now directly via the design environment. Of course, in real life this would most likely not be the case. The tasks will be accessed, for example, via the launchpad. Please follow the next steps to do so.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Access the Task via the Launchpad)]    

1. Access the [launchpad via this URL](https://sap-adoption-bootcamps.launchpad.cfapps.sap.hana.ondemand.com/site?siteId=955152a5-9685-45f6-8168-05ca15b6e600#Shell-home).

2. Access and open the **My Inbox** application.

   !![My Inbox Launchpad](07_MyInbox_Launchpad.png)

3. Select **your task** and perform the actions as done before.

   !![My Inbox Launchpad Tasks](08_MyInbox_Launchpad_Tasks.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Monitor Process and Automation)]

1. Navigate to the **Monitor** tab and choose **Process and Workflow Instances**.

    !![Monitor](01_Monitor.png)

2. Choose **Get Invoice Details** instance to check the status of the **CONTEXT** and **EXECUTION LOG**.

    !![Monitor](02_Process_and_Workflow.png)

3. Go to **Automation Jobs** under **Monitor**.
   You can see the Automation ran successfully:

    !![Monitor](03_Automations_Jobs.png)

    Notice how the process instance progresses further to the approval step in the business process.

    !![Monitor](05_Monitor_Process_and_Workflow.png)

[VALIDATE_1]
[ACCORDION-END]

---
