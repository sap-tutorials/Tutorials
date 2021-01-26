---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Send Data with REST
description: Send data to the SAP Cloud Platform Internet of Things Service Cloud using REST.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Install cURL](iot-cf-install-curl) and [Install OpenSSL](iot-cf-install-openssl).

## Details
### You will learn
- How to create a device model for the Internet of Things Service
- How to send data to the Internet of Things Service Cloud using REST
- How to use cURL as a sample client for REST

### Time to Complete
20 min

---

You have to create a device model for the Internet of Things Service using the Internet of Things Service Cockpit.

[ACCORDION-BEGIN [Step 1: ](Create a Capability)]

In the following a capability is created. A capability can be reused since it can be assigned to multiple sensor types. Each capability can have one or many properties.

1.  Log on to the Internet of Things Service Cockpit with your user credentials:

    `https://<HOST_NAME>/<INSTANCE_ID>/iot/cockpit/`

2.  On the **My Tenants** page, select a tenant first.

3.  Use the main menu to navigate to the **Capabilities** section of the **Device Management** category.

4.  Choose **+** above the capabilities list.

5.  In the **General Information** section, enter a **Name**. Optionally enter an **Alternate ID** for the capability.

    **Name**: `MyCapability`

    **Alternate ID**: `1234`

6.  In the **Properties** section, specify additional properties. Choose **+** from the toolbar of the properties table.

7.  Enter a **Name** for the property.

    **Name**: `temperature`

8.  Select a **Data Type** from the dropdown box for the property.

    **Data Type**: `float`

9.  Enter a **Unit of Measure**.

    **Unit of Measure**: `Celsius`

10.  Choose **Create**.

    You get a notification that the capability was created successfully.

11. Note down the **Alternate ID** of the capability.



[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Sensor Type)]

In the following a sensor type is created. The previously created capability is assigned to the sensor type.

1.  Use the main menu to navigate to the **Sensor Types** section of the **Device Management** category.

2.  Choose **+** above the sensor types list.

3.  In the **General Information** section, enter a **Name**. Optionally enter an **Alternate ID** for the sensor type.

    **Name**: `MySensorType`

4.  In the **Capabilities** section, add capabilities for this sensor type. Choose **+** from the toolbar of the capabilities table.

5.  Select the previously created **Capability** from the dropdown box.

6.  Select the capability **Type** from the dropdown box.

    **Type**: `measure`

7.  Choose **Create**.

    You get a notification that the sensor type was created successfully.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a REST Device)]

In the following a device is created. The device entity does not have any sensors attached, yet. The device entity is assigned to the REST gateway.

1.  Use the main menu to navigate to the **Devices** section of the **Device Management** category.

2.  Choose **+** above the devices list.

3.  In the **General Information** section, enter a **Name**, and select a **Gateway** from the dropdown box. Optionally enter an **Alternate ID** for the device.

    **Name**: `MyDevice`

    **Gateway**: `REST Gateway`

    **Alternate ID**: `11223344`

4.  Choose **Create**.

    You get a notification that the device was created successfully.

5.  Note down the **Alternate ID** of the device.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Sensor)]

In the following a sensor is created. The sensor is assigned to the previously created device and is a kind of the previously created sensor type.

1.  Use the main menu to navigate to the **Devices** section of the **Device Management** category.

2.  Search for and select the previously created device.

3.  In the **Sensors** section, choose **+** to add a new sensor.

4.  In the **General Information** section, enter a **Name**, and select the previously created **Sensor Type** from the dropdown box. Optionally enter an **Alternate ID** for the sensor.

    **Name**: `MySensor`

    **Alternate ID**: `4321`

6.  Choose **Add**.

    You get a notification that the sensor was created successfully.

7.  Note down the **Alternate ID** of the sensor.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Generate the Device Certificate)]

1.  Use the main menu to navigate to the **Devices** section of the **Device Management** category.

3.  Choose the previously created device.

4.  On the device details page, choose the **Certificate** tab.

5.  In the **Generate Certificate** dialog, select the type of certificate you want to generate and choose **Generate**.

    >Supported types are PEM and P12. Based on the certificate type you choose, the system downloads a `*-device_certificate.pem` or `*-device_certificate.p12` and a dialog opens, which shows the **Secret** key.

6.  Select and copy the displayed **Certificate Secret** key before closing the dialog or leaving the page, as it cannot be restored at a later point in time.

7.  Rename `*-device_certificate.pem` to `client.pem`or `*-device_certificate.p12` to `client.p12`.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Publish Data With cURL Using REST)]

**Prerequisites:**

-   You have installed the REST client (cURL). A description of how to install the cURL client can be found in the tutorial [Install cURL](iot-cf-install-curl).

-   You have installed OpenSSL. A description of how to install OpenSSL can be found in the tutorial [Install OpenSSL](iot-cf-install-openssl).

-   You have created the device model in step 1-4.

-   You have generated a certificate for your REST device and written down the secret key.


Open the terminal (macOS) or command line tool CMD (Windows) and change the directory to the path of the extracted REST device certificate.

>You must be connected to public Internet. Most corporate networks do not work due to port and protocol restrictions.

**Send data using cURL (only for cURL with OpenSSL on Windows or for cURL without SecureTransport using LibreSSL on macOS).**

1.  Enter and send the message string.

    ```JSON
    curl -v -k -E client.pem:<SECRET_KEY> -H "Content-Type:application/json" -d "<ENCODED_JSON_MESSAGE>" <REST_ENDPOINT>
    ```
   >The `<REST_ENDPOINT>` of the Internet of Things Gateway Cloud is the following URL: `https://<HOST_NAME>/iot/gateway/rest/> measures/<DEVICE_ALTERNATE_ID>`. It ends with the alternate ID of the device you've noted down before.

   Two formats are allowed:

    -   One with the measures specified as an array of arrays.

        ```JSON
        curl -v -k -E client.pem:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \"4321\", \"measures\": [[\"25\"]] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

    -   Another one, specifying measures as an array of JSON objects where the name of each property defined in the capability is the key.

        ```JSON
        curl -v -k -E client.pem:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \"4321\", \"measures\": [{\"temperature\": \"25\"}] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

2.  Check the response code in the terminal. The message must contain the following:

    `* upload completely sent off: <xx> out of <xx> bytes`

    `< HTTP/1.1 202 Accepted`

3.  You can check the incoming values using the **Data Visualization** of the device in the Internet of Things Service Cockpit or the Message Processing API. For more information, please refer to the tutorial [Consume Measures](iot-cf-consume-measures).


**Send data using cURL (only for cURL with SecureTransport on macOS).**

1.  Enter and send the message string.

    ```JSON
    curl -v -k -E client.p12:<SECRET_KEY> -H "Content-Type:application/json" -d "<ENCODED_JSON_MESSAGE>" <REST_ENDPOINT>
    ```
    > Note
    >  The `<REST_ENDPOINT>` of the Internet of Things Gateway Cloud is the following URL: `https://<HOST_NAME>/iot/gateway/rest/> measures/<DEVICE_ALTERNATE_ID>`. It ends with the alternate ID of the device you've noted down before.

    Two formats are allowed:

    -   One with the measures specified as an array of arrays.

        ```JSON
        curl -v -k -E client.p12:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \4321\", \"measures\": [[\"25\"]] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

    -   Another one, specifying measures as an array of JSON objects where the name of each property defined in the capability is the key.

        ```JSON
        curl -v -k -E client.p12:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \"4321\", \"measures\": [{\"temperature\": \"25\"}] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

2.  Check the response code in the terminal. The message must contain the following:

    `* upload completely sent off: <xx> out of <xx> bytes`

    `< HTTP/1.1 202 Accepted`

3.  You can check the incoming values using the **Data Visualization** of the device in the Internet of Things Service Cockpit or the Message Processing API. For more information, please refer to the tutorial [Consume Measures](iot-cf-consume-measures).

[DONE]

[ACCORDION-END]
