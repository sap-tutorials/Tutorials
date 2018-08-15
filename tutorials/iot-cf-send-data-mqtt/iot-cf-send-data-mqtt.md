---
title: Send Data with MQTT
description: Send data to the SAP Cloud Platform Internet of Things Service Cloud using MQTT. As a sample client for MQTT the Paho client is used.
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things ]
---

<!-- loiofa221f5ebf8d46cca04a4a875787aacb -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Install the Paho Client] and [Install OpenSSL].

## Details
### You will learn
- How to send data to the SAP Cloud Platform Internet of Things Service Cloud using MQTT.
- How to use the Paho client as a sample client for MQTT.

### Time to Complete
20 min

---

[ACCORDION-BEGIN [Step 1: ](Create a Capability)]

In the following a capability is created. A capability can be reused since it can be assigned to multiple sensor types. Each capability can have one or many properties.

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Capabilities* section of the *Device Management* category.

3.  Choose **+** (Create a capability) above the capabilities list.

4.  In the *General Information* section, enter a *Name*. Optionally enter an *Alternate ID* for the capability.

    *Name*: for example, `MyCapability`

    *Alternate ID*: for example, `2345`

5.  In the *Properties* section, specify additional properties. Choose **+** (Add a property) from the toolbar of the properties table.

6.  Enter a *Name* for the property.

    *Name*: for example, `temperature`

7.  Select a *Data Type* from the dropdown box for the property.

    *Data Type*: for example, `float`

8.  Enter a *Unit of Measure*.

    *Unit of Measure*: for example, `Celsius`

9.  Choose *Create*. Note down the *Alternate ID* of the capability.

    You see a message that the capability was created successfully.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Sensor Type)]

In the following a sensor type is created. The previously created capability is assigned to the sensor type.

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Sensor Types* section of the *Device Management* category.

3.  Choose **+** (Create a sensor type) above the sensor types list.

4.  In the *General Information* section, enter a *Name*. Optionally enter an *Alternate ID* for the sensor type.

    *Name*: for example, `MySensorType`

5.  In the *Capabilities* section, add capabilities for this sensor type. Choose **+** (Add a capability) from the toolbar of the capabilities table.

6.  Select the previously created *Capability* from the dropdown box.

7.  Select the capability *Type* from the dropdown box.

    *Type*: `measure`

8.  Choose *Create*.

    You see a message that the sensor type was created successfully.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create an MQTT Device)]

In the following a device entity is created. The device entity does not have any sensors attached, yet. The device entity will be assigned to the MQTT gateway.

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Devices* section of the *Device Management* category.

3.  Choose **+** (Create a device) above the devices list.

4.  In the *General Information* section, enter a *Name*, and select a *Gateway* from the dropdown box. Optionally enter an *Alternate ID* for the device.

    *Name*: for example, `MyDevice`

    *Gateway*: `IoT Gateway MQTT`

    *Alternate ID*: for example, `22334455`

5.  Choose *Create*. Note down the *Alternate ID* of the device.

    You see a message that the device was created successfully.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Sensor)]

In the following a sensor entity is created. The sensor will be assigned to the previously created device and will be a kind of the previously created sensor type.

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Devices* section of the *Device Management* category.

3.  Search for and select the previously created device.

4.  In the *Sensors* section, choose **+** (Add a sensor) to add a new sensor.

5.  In the *General Information* section, enter a *Name*, and select the previously created *Sensor Type* from the dropdown box. Optionally enter an *Alternate ID* for the sensor.

    *Name*: for example, `MySensor`

    *Alternate ID*: for example, `5432`

6.  Choose *Add*. Note down the *Alternate ID* of the sensor.

    You see a message that the sensor was created successfully.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Generate the Device Certificate)]

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Devices* section of the *Device Management* category.

3.  Choose the previously created device.

4.  On the device details page, choose the *Certificate* tab.

5.  In the *Generate Certificate* dialog, select the type of certificate you want to generate. For this tutorial please use *PEM* and choose *Generate*.

    > Note:
    > Supported types are PEM and P12. Based on the certificate type you choose, the system downloads a `*-device_certificate.pem` or `*-device_certificate.p12` and a dialog opens, which shows the *Secret* key.
    >
    >

6.  Select and copy the displayed *Secret* key before closing the dialog or leaving the page, as it cannot be restored at a later point in time.

7.  Rename `*-device_certificate.pem` to `certificate.pem`.

8.  Open the console in the directory where the previously downloaded `*.pem` file is located. Enter the following command in the terminal:

    ```bash
    openssl pkcs12 -export -in certificate.pem -inkey certificate.pem -out client.ks
    ```

    > Note:
    > Use the copied *Secret* key from the previous step for all password requests (import pass phrase and Export Password).
    >
    >

    A file named `client.ks` is created.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Publish Data Using the Paho Client)]

**Prerequisites:**

-   You have created the device model.

-   You have generated a certificate for your MQTT device and wrote down the secret key.


1.  Open the Paho client.

    > Note:
    > You must be connected to public Internet. Most corporate networks do not work due to port and protocol restrictions.
    >
    >

2.  Choose **+** in the *Connections* tab to create a new connection.

    The system opens a new tab with the connection details.

3.  The system opens a new tab with the connection details.

4.  Enable the checkbox *Enable SSL*.

5.  Choose *Browse* for *Key Store Location* and choose the `client.ks` file downloaded for the device.

    > Note:
    > You might need to change the file type to **.** in the file selection menu.
    >
    >

6.  Choose *Browse* for *Trust Store Location* and choose the `/jre/lib/security/cacerts` of your local Java installation folder if you use a trusted certification authority like VeriSign.

    > Note:
    > You might need to change the file type to **.** in the file selection menu.
    >
    >

7.  Enter the copied or noted *Secret*key into the field for the *Key Store Password*.

8.  Set the *Trust Store Password* to **`changeit`**.

9.  Choose *MQTT* tab of the connection.

10. Add the *Server URI* as follows: `ssl://<HOST_NAME>:8883`

    For example: `ssl://demo.eu10.cp.iot.sap:8883`

11. Add the *Client ID*: The Alternate ID of the device as a string.

    For example: `22334455`

12. Choose *Connect*.

    The status is changed to *Connected*.

13. Enter the *Topic* in the *Publication* section.

    Topic: `measures/<DEVICE_ALTERNATE_ID>`

    Enter the recorded `<DEVICE_ALTERNATE_ID>` as a string. For example: `measures/22334455`.

14. Enter the *Message* in the *Publication* section.

    Two formats are allowed:

    -   One with the measures specified as array of array.

        ```bash
        { "capabilityAlternateId": "2345", "sensorAlternateId": "5432", "measures": [["25"]] }
        ```

    -   Another one, specifying measures as array of JSON objects where the name of each property defined in the capability is the key.

        ```bash
        { "capabilityAlternateId": "2345", "sensorAlternateId": "5432", "measures": [{"temperature": "25"}] }
        ```

    > Note:
    > The escaping of characters may not be needed depending on the client in use. Typically, clients getting an input from a command line require escaping, while clients providing a GUI (like Paho) do not.
    >
    >

15. Choose *Publish*.

    A message is sent to the Internet of Things Service.

16. You can check the incoming values using the *Data Visualization* of the device in the Internet of Things Service Cockpit or the Internet of Things API Service. For more information, please refer to the tutorial [Consuming Measures.](https://help.sap.com/viewer/7e269da75d024ef09bfb7a5986c47517/Cloud/en-US)


[ACCORDION-END]
