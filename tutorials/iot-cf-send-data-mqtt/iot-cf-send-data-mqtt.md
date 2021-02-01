---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Send Data with MQTT
description: Send data to the SAP Cloud Platform Internet of Things Service Cloud using MQTT. As a sample client for MQTT the Paho client is used.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Install the Paho Client](iot-cf-install-paho-client) and [Install OpenSSL](iot-cf-install-openssl).

## Details
### You will learn
- How to create a device model for the Internet of Things Service
- How to send data to the Internet of Things Service Cloud using MQTT
- How to use the Paho client as a sample client for MQTT

### Time to Complete
20 min

## Next Steps
- **Tutorials:** [Consume Measures](iot-cf-consume-measures)

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

    **Alternate ID**: `2345`

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

[ACCORDION-BEGIN [Step 3: ](Create an MQTT Device)]

In the following a device entity is created. The device entity does not have any sensors attached, yet. The device entity is assigned to the MQTT gateway.

1.  Use the main menu to navigate to the **Devices** section of the **Device Management** category.

2.  Choose **+** above the devices list.

3.  In the **General Information** section, enter a **Name**, and select a **Gateway** from the dropdown box. Optionally enter an **Alternate ID** for the device.

    **Name**: `MyDevice`

    **Gateway**: `MQTT Gateway`

    **Alternate ID**: `22334455`

4.  Choose **Create**.

    You get a notification that the device was created successfully.

5.  Note down the **Alternate ID** of the device.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Sensor)]

In the following a sensor entity is created. The sensor is assigned to the previously created device and is a kind of the previously created sensor type.

1.  Use the main menu to navigate to the **Devices** section of the **Device Management** category.

2.  Search for and select the previously created device.

3.  In the **Sensors** section, choose **+** to add a new sensor.

4.  In the **General Information** section, enter a **Name**, and select the previously created **Sensor Type** from the dropdown box. Optionally enter an **Alternate ID** for the sensor.

    **Name**: `MySensor`

    **Alternate ID**: `5432`

5.  Choose **Create**.

    You get a notification that the sensor was created successfully.

6.  Note down the **Alternate ID** of the sensor.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Generate the Device Certificate)]

**Prerequisites:**

You have installed OpenSSL. A description on how to install OpenSSL can be found in the tutorial [Install OpenSSL](iot-cf-install-openssl).

1.  Use the main menu to navigate to the **Devices** section of the **Device Management** category.

2.  Choose the previously created device.

3.  On the device details page, choose the **Certificate** tab.

4.  In the **Generate Certificate** dialog, select the type of certificate you want to generate. For this tutorial please use `pem`.

    >Supported types are PEM and P12. Based on the certificate type you choose, the system downloads a `*-device_certificate.pem` or `*-device_certificate.p12` and a dialog opens, which shows the **Secret** key.

5.  Choose **Generate**.

6.  Select and copy the displayed **Certificate Secret** key before closing the dialog or leaving the page, as it cannot be restored at a later point in time.

7.  Rename `*-device_certificate.pem` to `certificate.pem`.

8.  Open the console in the directory where the previously downloaded `*.pem` file is located.

9.  Enter the following command in the terminal:

    `openssl pkcs12 -export -in certificate.pem -inkey certificate.pem -out client.ks`

    >Use the copied **Secret** key from the previous step for all password requests - **import pass phrase** and **Export Password**.

    A file named `client.ks` is created.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Publish Data Using the Paho Client)]

**Prerequisites:**

-   You need to install Java SE Runtime Environment 8.

    >For ease of use, we recommend that you add `JAVA_HOME` to your PATH environment variables to point to where the JDK software is located.

-   You have installed the MQTT client (Paho). A description of how to install the Paho client can be found in the tutorial [Install the Paho Client](iot-cf-install-paho-client).

-   You have created the device model in step 1-4.

-   You have generated a certificate for your MQTT device and written down the secret key.


1.  Open the Paho client.

    >You must be connected to public Internet. Most corporate networks do not work due to port and protocol restrictions.

2.  Choose **+** in the **Connections** tab to create a new connection.

    The system opens a new tab with the connection details.

3.  Choose the **Options** tab of the connection.

4.  Enable the checkbox **Enable SSL**.

5.  Choose **Browse** for **Key Store Location** and choose the `client.ks` file created for the device in the previous step.

    >You might need to change the file type to  `*.*` in the file selection menu.

6.  Enter the copied or noted **Secret** key into the field for the **Key Store Password**.

7.  Choose **Browse** for **Trust Store Location** and choose the `/jre/lib/security/cacerts` of your local Java installation folder if you use a trusted certification authority, for example Verisign.

    >You might need to change the file type to `*.*` in the file selection menu.

8.  Set the **Trust Store Password** to **`changeit`**.

9.  Choose the **MQTT** tab of the connection.

10. Add the **Server URI** as follows: `ssl://<HOST_NAME>:8883`

11. Add the **Client ID**: The Alternate ID of the device as a string: `22334455`

12. Choose **Connect**.

    The status is changed to **Connected**.

13. Subscribe to the **Topic** in the **Subscription** section.

    Topic: `ack/<DEVICE_ALTERNATE_ID>`

    Enter the recorded `<DEVICE_ALTERNATE_ID>` as a string: `ack/22334455`.

14. Choose **Subscribe**.    

15. Enter the **Topic** in the **Publication** section.

    Topic: `measures/<DEVICE_ALTERNATE_ID>`

    Enter the recorded `<DEVICE_ALTERNATE_ID>` as a string: `measures/22334455`.

16. Enter the **Message** in the **Publication** section.

    Two formats are allowed:

    -   One with the measures specified as array of array.

        ```JSON
        { "capabilityAlternateId": "2345", "sensorAlternateId": "5432", "measures": [["25"]] }
        ```  

    -   Another one, specifying measures as array of JSON objects where the name of each property defined in the capability is the key.

        ```JSON
        { "capabilityAlternateId": "2345", "sensorAlternateId": "5432", "measures": [{"temperature": "25"}] }
        ```

      >The escaping of characters may not be needed depending on the client in use. Typically, clients getting an input from a command line require escaping, while clients providing a GUI (like Paho) do not.

17. Choose **Publish**.

    A message is sent to the Internet of Things Service.

    You should receive a message with the processing status of the published measurement data. The JSON message on the topic `ack/22334455` should look like

    ```JSON
    { "capabilityAlternateId": "2345", "sensorAlternateId": "5432", "code":202 }
    ```

    The code `202` means the data is "provisionally accepted" by the Internet of Things Gateway (but may still be rejected further upstream), similar to the corresponding HTTP status code.

18. You can check the incoming values using the **Data Visualization** of the device in the Internet of Things Service Cockpit or the Message Processing API. For more information, please refer to the tutorial [Consume Measures](iot-cf-consume-measures).

[DONE]

[ACCORDION-END]
