---
title: Monitor Events Acquired Using the Event Acquisition Application
description: Monitor the events acquired using the Event Acquisition application.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud]
primary_tag: products>sap-cloud-platform
author_name: Kavya Gowda
author_profile: https://github.com/Kavya-Gowda
---

## Prerequisites
 - You have the **Event Acquisition** application configured on SAP Fiori launchpad. For more information, refer to [Consume the Process Visibility UI Applications Using SAP Fiori Launchpad](cp-cf-processvisibility-setup-flp).
- You have the **`PVTenantOperator`** role assigned to your user.

## Details
### You will learn
  - How to view the events that have been acquired by process visibility

Using the **Event Acquisition** application, you can view the events that have been acquired by process visibility.

---

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

For more information on this application, refer to [Event Acquisition application](https://help.sap.com/viewer/62fd39fa3eae4046b23dba285e84bfd4/Cloud/en-US/72a054799c6f41e08b5445b950ac512d.html).

[VALIDATE_1]
[ACCORDION-END]



---
