---
title: Run example pipelines in SAP Data Hub, developer edition 2.4
description: Explore the example pipelines that are delivered with SAP Data Hub, developer edition 2.4.
auto_validation: true
primary_tag: products>sap-data-hub
tags: [  tutorial>beginner, topic>big-data, products>sap-data-hub, products>sap-vora ]
---

## Prerequisites

## Details
### You will learn  
During this tutorial, you will learn how to run the examples pipelines delivered with SAP Data Hub, developer edition.

### Time to Complete
**30 Min**

---

[ACCORDION-BEGIN [Step 1: ](Familiarize yourself with the modelling UI)]
Open the modelling environment for building pipelines via `http://localhost:8090` (SAP Data Hub Modeler).

![picture_01](./datahub-docker-v2-examples_01.png)  

The modelling environment consists of four main parts:
 - **Navigation (1)**: You use this part of the modelling environment to access pipelines (also known as graphs), operators and the repository.
 - **Pipeline editor (2)**: You create and edit pipelines here.
 - **Configuration / documentation (3)**: You use this part of the modelling environment to configure pipelines as well as operators inside pipelines and to access the documentation. The two buttons on the right help you to switch between configuration and documentation.
 - **Status messages, logs, traces (4)**: You use this part of the modelling environment to monitor the execution of pipelines.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Execute an example pipeline)]
Click on the **Data Generator** (or any other) example pipeline (inside the **Navigation**). The pipeline opens in the editor.

Click **Run** to execute the pipeline

When the **Status** tab indicates that the pipeline is running, use the context menu **Open UI** of the **Terminal** operator to see the generated sensor data.

![picture_02](datahub-docker-v2-examples_02.png)

Copy any row of the terminal output and paste it in the frame below and click on **Validate**.


[VALIDATE_1]

Stop the pipeline by clicking **Stop**.
[ACCORDION-END]

---
