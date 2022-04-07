---
author_name: Chaitanya Priya Puvvada
author_profile: https://github.com/chaitanya-priya-puvvada
title: Run the Business Process
description: Release, Deploy and Run the Business Process
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-process-automation
---

## Prerequisites
  -[Subscribe to SAP Process Automation using Booster in SAP BTP Free tier](spa-subscribe-booster)

## Details
### You will learn
  - How to release and deploy the Process
  - How to run the Process
  - How to monitor the Process

---

[ACCORDION-BEGIN [Step 1: ](Release Business Process Project)]

There are two ways to release individual projects once they are finished:

   - When you're releasing a new business process project, enter a brief summary of the changes in the release **Version Comment** section (optional) then choose **Release**.
   - When you're releasing a modified version of a business process project that is already released, in the release **Version Contains** section, select one of the following:

      - Select **Patches** to indicate bug fixes. It updates the third digit of the version number.
      - Select **Minor Changes** to indicate small modifications. It updates the second digit of the version number.
      - Select **Major Changes** to indicate important modifications potentially leading to incompatibility between versions. It updates the first digit of the version number.

1. In the Process Builder, choose **Release**.

    !![Release](01_Process_final.png)

2. For the first version, add a **Version Comment** if needed and choose **Release**.

    !![Release first](02_Release_first_version.png)

2. For the additional version, choose the type of version, add a **Version** annotation if needed and select **Release**.

    !![Release new](02_Release_second_version.png)

3. The project released successfully and is ready to be deployed.

    !![Released](03_Released_first_version.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Deploy released project)]

You can deploy business process projects from each released version of the project in the Process Builder or through the Lobby.

1. From the released version of the business process project in the Process Builder, choose **Deploy**.

    !![Start Deploy](01_Released_first_version.png)

2. The project deployed successfully and is ready for running and monitoring.

    !![Deployed](03_Deployed_first_version.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run Business Process)]

1. From the deployed version of the Business Process Project in the Process Builder, open the process **Order Processing**.

    !![Run](01_Open_Order_Processing.png)

2. Select the Order Processing Form and choose the **Copy** icon aside the **Form Link**.

    !![Run copy the form link](02_Process_Start.png)

3. Open the form by pasting the **Form Link** in a browser window.

    !![Run open the form](03_Order_Processing_Form.png)

4. Fill the Order details and choose **Submit**.

    !![Run open the form](Form_Inputs.png)

5. The process is triggered. You can now work on the tasks, monitor the process and gain insights.

    !![Run form successfully submitted](Run_inputs_2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Work on the tasks)]

1. Go to the **Lobby** and open the **My Inbox** application by selecting the button ![Inbox Icon](02_Inbox_Icon.png) at the top right corner.

    !![Lobby](01_Lobby.png)

2. After opening the **My Inbox** application, you see on the left-hand side all the tasks listed. **Select the task** with the sales order number with which you triggered the process.

    !![My Inbox Actions](03_MyInbox_Actions.png)

3. Move on with one of the actions:

    - **Submit**, **Show Log** (to see what has been done so far),
    - **Claim** (to reserve this task for you) or
    - **Mail** (to forward this task via email).

4. You can also **sort**, **filter** or **group** your tasks at the bottom of the task list with these buttons:
    !![My Inbox Filter](05_MyInbox_Filter.png)

5. Depending on your selected actions and the information you have provided at the start of the process, the next task could be to **Submit** the order.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Monitor the Process)]

1. Navigate to the **Monitor** tab and choose **Process and Workflow Instances**.

    !![Monitor](01_Monitor.png)

2. Choose **Order Processing** instance to check the status of the **CONTEXT** and **EXECUTION LOG**.

    !![Monitor](02_Process_and_Workflow.png)

[VALIDATE_1]
[ACCORDION-END]


---
