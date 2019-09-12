---
title: Monitor your Business Scenarios using Monitor Scenarios Application
description: Monitor the business scenario, process data, and schedule event processing job using the Monitor Scenarios application.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud, tutorial>license]
primary_tag: products>sap-cloud-platform
---

## Prerequisites
 - You have the **Monitor Scenarios** application configured on SAP Fiori launchpad. For more information, refer to [Consume the Process Visibility UI Applications Using SAP Fiori Launchpad] (cp-cf-processvisibility-setup-flp).
 - You have the **`PVAdmin`** role assigned to your user

## Details
### You will learn
  - How to view the details about the ongoing or completed  processing runs of a business scenario
  - How to manage the acquired process data of a business scenario

You can manage the activated business scenarios using the **Monitor Scenarios** application.

[ACCORDION-BEGIN [Step 1: ](Access the Monitor Scenarios application)]

1. Log in to SAP Fiori launchpad.

2. Click on the **Monitor Scenarios** tile to open the Monitor Scenarios application.

![Monitor Scenarios tile](Monitor-Scenarios-Tile-01.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](View the details of a scenario definition)]

1. Type and search for the keyword **Employee Onboarding Scenario**, which is the scenario definition name used in this tutorial.

    ![Employee Onboarding Process](Employee-Onboarding-Process-02.png)

    By default, the **Processing Job** is scheduled for every 5 minutes. You will be able to view details about the ongoing or completed  processing runs such as Start Time, Status, Events Processed, Events in Buffer, Instances Processed within this scheduled time interval.

    ![Processing Job](Processing-Job-07.png)

    ![Scenario details](Details-03.png)

3. If you want to see the processing information immediately, click on **Process Data** to process the events manually.

    ![Processing Data](Process-Data-04.png)

4. On successful processing of data, you can see the processing information listed out under Processing Information.

    ![Processing Information](Processing-Information-05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Clear the existing process data of a scenario definition)]

You can clear the process data of a scenario if required. Select the definition from the list and choose **Clear Processed Data**.

![Clear Process Data](Clear-Process-Data-06.png)

You can delete the scenario definition if required. Select the definition from the list and choose **Delete**.

![Delete Process Data](Delete8.png)

[VALIDATE_1]
[ACCORDION-END]




---
