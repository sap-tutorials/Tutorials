---
title: Consume Measures
description: Consume measures using the Internet of Things Service Cockpit or the Message Processing API.
auto_validation: true
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things, topic>cloud ]
---

<!-- loioaac99adf868e47dc8b51b0203aa13b14 -->

## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn
- How to consume measures of a device in the Internet of Things Service using the the Internet of Things Service Cockpit and the Message Processing API

### Time to Complete
10 min

---

[ACCORDION-BEGIN [Step 1: ](Consume Measures of a Device Using the IoT Cockpit)]

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/<INSTANCE_ID>/iot/cockpit/
    ```

2.  On the *My Tenants* page, select a tenant first and then use the main menu to navigate to the *Devices* section of the *Device Management* category.

    All devices are listed.

3.  Search and select the device.

4.  On the device detail page, choose the *Data Visualization* tab.

5.  Select a *Sensor* from the dropdown list.

6.  Select a *Capability* from the dropdown list.

    > Note:
    >  In case that only one capability is available, it is selected automatically.

7.  Select the *Properties* from the dropdown list you would like to visualize in the chart.

    > Note:
    >  Initially, all properties of the selected capabilities are displayed.


8.  Choose the *Line Chart* or the *Table View* tab to see the measures with supported data formats.

    The chart opens.

    > Note:
    >  The chart only supports these data formats:
    >
    > -   Integer
    >
    > -   Long
    >
    > -   Float
    >
    > -   Double
    >
    > Other formats are shown in the *Table View*.
    >
    >

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Consume Measures of a Device Using the Message Processing API)]

1.  Open the UI for the Message Processing API.

    ```bash
    https://<HOST_NAME>/iot/processing/api/v1/doc/
    ```

    You see the main page with the categories overview.

2.  Choose *Authorize*.

3.  Enter your user credentials.

4. Choose *Authorize*.

5.  Navigate to the *Devices* category.

6.  In the *Measures* category, choose `GET /tenant/{tenantId}/measures/capabilities/{capabilityId}`.

7.  Choose *Try it out*.

8.  In the field `deviceId`, enter the ID of the device you want to inspect.

9.  Enter the ID of the capability in the field `capabilityId`.

10.  Enter the ID of your tenant in the `tenantId` field.

11.  Choose *Execute*.

12.  Scroll to the *Server response* body and *Code*.

    In case of success the response code is `200` and the *Response body* contains the latest measures of the device.

[DONE]

[ACCORDION-END]
