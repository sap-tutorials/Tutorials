---
title: Monitor and Manage Events using the Event Acquisition Application
description: Monitor, import, debug, and delete the events acquired from workflow using the Event Acquisition application.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud]
primary_tag: products>sap-cloud-platform
---

## Prerequisites
 - You have the **Event Acquisition** application configured on SAP Fiori launchpad. For more information, refer to [Consume the Process Visibility UI Applications Using SAP Fiori Launchpad] (cp-cf-processvisibility-setup-flp).
- You have the **`PVTenantOperator`** role assigned to your user

## Details
### You will learn
  - How to view the events that have been acquired by process visibility
  - How to monitor the errors that have occurred while consuming the events that have been pushed to the process visibility service
  - How to import events into the process visibility service

Using the **Event Acquisition** application, you can view the events that have been acquired by process visibility and monitor errors while consuming the events pushed to the service.

[ACCORDION-BEGIN [Step 1: ](Access the Events Acquisition application)]

1. Log into SAP Fiori Launchpad.

2. Click on the **Event Acquisition** tile to open the Events Acquisition application.

    ![Event Acquisition Tile](Event-Acquisition-Tile-01.png)

    You will be able to see the UI of **Event Acquisition** application. By default, no events are shown unless a filter is applied.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](View events acquired by Process Visibility)]

1. Click the filter icon to filter events based on attributes.

    ![Filter](Filter-02.png)

    You can filter the events based on Process Definition ID, Process Instance ID, Event Type, and Timestamp.

2. Click on the required filter and provide the value you used in your business scenario. In this tutorial, we apply the filter based on Process Definition ID and we use the value **onboard**.

    ![Filter values](Filter-Values-03.png)

    ![value onboard](Filter-Value1-04.png)

    The events acquired will be listed.

    ![Events Listed](Events-Listed-05.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import the events acquired by Process Visibility)]

Click the import icon, browse for the required JSON file containing an array of events, and select **Import**.

![Import](Import-07.png)

![Import JSON](Import-json-08.png)

You will be able to import events into the process visibility service.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Monitor the errors while consuming the events)]

Click the **Errors** tab to view errors that have occurred while consuming the events pushed to the process visibility service.

![Errors](Errors-06.png)

You can see error messages specific to each event and the time when a specific error has occurred.

[VALIDATE_1]
[ACCORDION-END]



---
