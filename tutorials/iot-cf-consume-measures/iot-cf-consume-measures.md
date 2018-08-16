---
title: Consume Measures
description: Consume measures using the Internet of Things Service Cockpit or the Internet of Things API Service.
auto_validation: false
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things, topic>cloud ]
---

<!-- loioaac99adf868e47dc8b51b0203aa13b14 -->

## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn
- How to consume measures of a device in the Internet of Things Service using the the Internet of Things Service Cockpit and the Internet of Things API

### Time to Complete
10 min

---

[ACCORDION-BEGIN [Step 1: ](Consume Measures of a Device Using the IoT Cockpit)]

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Devices* section of the *Device Management* category.

    All devices are listed.

3.  Search and select the device.

4.  In the device detail page, choose the *Data Visualization* tab.

5.  Select a *Sensor* from the dropdown list.

6.  Select a *Capability* from the dropdown list.

7.  Select the *Properties* from the dropdown list you would like to visualize in the chart.

8.  Choose the *Line Chart* or the *Table View* tab to see the measures with supported data formats.

    The chart opens.

    > Note:
    > The chart only supports these data formats:
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

[ACCORDION-BEGIN [Step 2: ](Consume Measures of a Device Using the API)]

1.  Open the Internet of Things API Service UI.

    ```bash
    https://<HOST_NAME>/iot/core/api/v1/doc/
    ```

    You will see the main page with the categories overview.

2.  Choose *Authorize*.

3.  Log on with your user credentials.

4.  Navigate to the *Devices* category.

5.  In the *Devices* category, choose `GET /devices/{deviceId}/measures`.

6.  Choose *Try it out*.

7. Enter the `deviceId`: the numeric ID of the device you noted down in the previous steps.

8. Choose *Execute*.

9. Scroll to the *Server response* body and *Code*.

    In case of success the response code is `200` and the *Response body* contains all information of the device.

[DONE]

[ACCORDION-END]
