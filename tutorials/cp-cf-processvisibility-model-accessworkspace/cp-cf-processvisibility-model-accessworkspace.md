---
title: Track the Workflow Using the Process Workspace Application
description: Track the workflow in real time and analyze performance indicators to gain insights on the needed improvements.
auto_validation: true
time: 10
tags: [tutorial>beginner, products>sap-business-technology-platform]
primary_tag: products>sap-business-technology-platform
author_name: Kavya M Gowda
author_profile: https://github.com/I540620
---

## Prerequisites
 - Ensure that you have setup the **Workflow Management** service to access the **Process Workspace** application. For more information, see the [Set Up Workflow Management in Cloud Cockpit](cp-starter-ibpm-employeeonboarding-1-setup) tutorial.
 - Process the data by using **Monitor Visibility Scenarios** application. For more information, see the [Monitor Your Visibility Scenarios](cp-cf-processvisibility-model-monitorscenario) tutorial.

## Details
### You will learn
  - How to gain visibility on end-to-end processes

You can track the workflow in real-time and analyze performance indicators to gain insights on the needed improvements.

SAP Process Visibility service provides the Process Workspace application to gain visibility on end-to-end processes for line-of-business users. It enables you to track processes in real-time, search and filter instances, analyze performance indicators, and view detailed information about an instance.

---

[ACCORDION-BEGIN [Step 1: ](Access the Process Workspace)]

Log on to **Workflow Management** home screen and choose the  **Process Workspace** tile.
    !![Home screen](homescreen.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Analyze performance indicators for a scenario instance)]

1. In the **Scenarios** table, choose **Employee Onboarding Process**.

    !![Scenario](Scenario-Table.png)

      You can see the Process workspace overview page of **Employee Onboarding Process** scenario as shown below.

      !![Scenario overview page](overview.png)

      Above is the basic default process workspace, if you enhance the scenario to add  more phases, calculated attributes, and performance indicators then you can achieve a dashboard as shown below, which can be used by the business users in their daily work to track workflow instances. For more information, see [SAP Help Portal](https://help.sap.com/viewer/62fd39fa3eae4046b23dba285e84bfd4/Cloud/en-US/df284fd12073454392c5db8913f82d81.html)

      !![Enhanced overview page](Enhanced-Workspace.png)

2. Choose one of the tiles to view details of the workflow instances contributing to the respective performance indicators.

    !![Scenario On track page](overview-Ontrack.png)


    !![Performance indicator view](PPI.png)

3. Choose an instance from the table to navigate to the details view of the scenario instance.

    !![Performance indicator view](PPI - 01.png)

    The scenario instance details view provides detailed information about the selected instance.

    !![Instances view](instance-details-view.png)

    In the scenario instance details view, you can view the following:

    -	**Phases** view displays where exactly the workflow is, how much time has elapsed, how many more phases and steps are left, and any violations which could bring the instance to risk based on the target cycle time.

    !![Phases](Phases.png)

    - **Path** provides an ordered list or a flow chart to show the progress of workflow in terms of time taken to traverse from one task to another. This helps you to identify the bottlenecks, inconsistencies, and possible causes of the delays in the workflow.

    !![Path](Path.png)

[VALIDATE_1]
[ACCORDION-END]
