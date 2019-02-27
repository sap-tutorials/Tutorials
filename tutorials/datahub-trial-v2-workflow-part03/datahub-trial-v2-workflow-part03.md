---
title: Create Workflow (part 3), Build a task Workflow in SAP Data Hub, trial edition 2.4
description: Build a task Workflow using SAP Data Hub, trial edition 2.4.
auto_validation: true
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
time: 15
---

## Details
### You will learn  
  - How to build a task workflow using the **Pipeline** operator

This workflow combines both tasks which you created during the previous tutorials into a process which you execute at once.
Please note here in this tutorial GCP refers to Google Cloud platform and AWS refers to Amazon Web Services.

---

[ACCORDION-BEGIN [Step 1: ](Create Workflow)]

Open the modelling environment for building pipelines via SAP Data Hub Modeler. To access the SAP Data Hub Launchpad in AWS or GCP you need go to the chapters 3.3 and 3.4 as described in the [**Getting Started with SAP Data Hub, trial edition**] (https://caldocs.hana.ondemand.com/caldocs/help/Getting_Started_Data_Hub24.pdf) guide. From SAP Data Hub Launchpad you could access the SAP Data Hub Modeler.

>As the above URL is a local URL, it will be accessible only if you are doing the tutorials and have already configured the hosts file. If not, please refer to [Getting Started with SAP Data Hub, trial edition 2.4](https://caldocs.hana.ondemand.com/caldocs/help/Getting_Started_Data_Hub24.pdf) guide.

Enter **DEFAULT** as the **Tenant**, `DATAHUB` as **Username** and the password which you have selected during system setup as **Password** to logon.

Create a new graph and add **`Workflow Trigger`** operator, 2 X **Pipeline** operators and a **Workflow Terminator** to the graph by drag and drop.

![picture_01](./datahub-trial-v2-workflow-part03_01.png)

Connect the `output` out port of the **Workflow Trigger** to the `input` in port of the first **Pipeline** operator. Connect the `output` out port of the first **Pipeline** operator to the `input` in port of the second **Pipeline** operator. Connect the `output` out port of the second **Pipeline** operator to the `stop` in port of the **Workflow Terminator** operator.

Right click on the first **Pipeline** operator and go to **Open Configuration**. Under the parameter **Graph name** select the graph that we have created in the tutorial **Create Workflow (part 1), Enrich data with Data Transform**. In our case, we have named it as **Workflow 1**.

![picture_03](./datahub-trial-v2-workflow-part03_03.png)

Similarly, for the second **Pipeline** operator, select the graph that we have created in the tutorial **Create Workflow (part 2), Aggregate data with Data Transform** under the parameter **Graph Name**. In our case, we have named it as **Workflow 2**. Also increase the parameter Retry interval from 20 to 200 for both the **Pipelines**.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Execute Workflow)]

Once done, save the graph. In our case, we have named it as **Workflow 3**. Now execute the graph using the button at the top of the page. The execution status is displayed in the bottom part of the screen and it changes from **Running** to **Completed** once the execution completes.

![picture_02](./datahub-trial-v2-workflow-part03_02.png)

We have now executed the **Workflow 3** but you will notice in the above screenshot that, **Workflow 3** calls the linked pipeline, **Workflow 1**. Once the execution of **Workflow 1** completes, it starts the execution of **Workflow 2**.

**Remark**: When you look at the `EnrichDevices` or `STATISTICS_DATA` data set, you will not see any changes compared to the previous two tutorials.

[VALIDATE_1]

[ACCORDION-END]
