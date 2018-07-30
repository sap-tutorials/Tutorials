---
title: Send Data with REST
description: Send data to the SAP Cloud Platform Internet of Things Service Cloud using REST.
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things ]
---

<!-- loioe341f4746979496d8abad6f1e0d8a1dc -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Install curl] and [Install OpenSSL].

## Details
### You will learn
- How to send data to the SAP Cloud Platform Internet of Things Service Cloud using REST.
- How to use cURL as a sample client for REST.

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

    *Alternate ID*: for example, `1234`

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

[ACCORDION-BEGIN [Step 3: ](Create a REST Device)]

In the following a device entity is created. The device entity does not have any sensors attached, yet. The device entity will be assigned to the REST gateway.

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  Use the main menu to navigate to the *Devices* section of the *Device Management* category.

3.  Choose **+** (Create a device) above the devices list.

4.  In the *General Information* section, enter a *Name*, and select a *Gateway* from the dropdown box. Optionally enter an *Alternate ID* for the device.

    *Name*: for example, `MyDevice`

    *Gateway*: `IoT Gateway REST`

    *Alternate ID*: for example, `11223344`

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

    *Alternate ID*: for example, `4321`

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

[ACCORDION-BEGIN [Step 6: ](Publish Data With curl Using REST)]

**Prerequisites:**

-   You have created the device model.

-   You have generated a certificate for your REST device and wrote down the secret key.


Open the terminal (macOS) or command line tool CMD (Windows) and change the directory to the path of the extracted REST device certificate.

> Note:
> You must be connected to public Internet. Most corporate networks do not work due to port and protocol restrictions.
>
>

**Send data using curl to the device (only for curl with OpenSSL on Windows or for curl without SecureTransport using LibreSSL on macOS).**

1.  Enter and send the message string.

    ```bash
    curl -v -k -E client.pem:<SECRET_KEY> -H "Content-Type:application/json" -d "<ENCODED_JSON_MESSAGE>" <REST_ENDPOINT>
    ```

    Two formats are allowed:

    -   One with the measures specified as array of array.

        ```bash
        curl -v -k -E client.pem:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \"4321\", \"measures\": [[\"25\"]] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

    -   Another one, specifying measures as array of JSON objects where the name of each property defined in the capability is the key.

        ```bash
        curl -v -k -E client.pem:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \"4321\", \"measures\": [{\"temperature\": \"25\"}] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

2.  Check the response code in the terminal. The message must contain the following:

    ```bash
    * upload completely sent off: <xx> out of <xx> bytes
    ```

    ```bash
    < HTTP/1.1 200 OK
    ```

3.  You can check the incoming values using the *Data Visualization* of the device in the Internet of Things Service Cockpit or the Internet of Things API Service. For more information, please refer to the tutorial [Consuming Measures.](https://help.sap.com/viewer/7e269da75d024ef09bfb7a5986c47517/Cloud/en-US)


**Send data using curl to the device (only for curl with SecureTransport on macOS).**

1.  Enter and send the message string.

    ```bash
    curl -v -k -E client.p12:<SECRET_KEY> -H "Content-Type:application/json" -d "<ENCODED_JSON_MESSAGE>" <REST_ENDPOINT>
    ```

    Two formats are allowed:

    -   One with the measures specified as array of array.

        ```bash
        curl -v -k -E client.p12:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \4321\", \"measures\": [[\"25\"]] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

    -   Another one, specifying measures as array of JSON objects where the name of each property defined in the capability is the key.

        ```bash
        curl -v -k -E client.p12:jJ3fF7dD0rR2rR3wW0eE2tT -H "Content-Type:application/json" -d "{ \"capabilityAlternateId\": \"1234\", \"sensorAlternateId\": \"4321\", \"measures\": [{\"temperature\": \"25\"}] }" https://demo.eu10.cp.iot.sap/iot/gateway/rest/measures/11223344
        ```

2.  Check the response code in the terminal. The message must contain the following:

    ```bash
    * upload completely sent off: <xx> out of <xx> bytes
    ```

    ```bash
    < HTTP/1.1 200 OK
    ```

3.  You can check the incoming values using the *Data Visualization* of the device in the Internet of Things Service Cockpit or the Internet of Things API Service. For more information, please refer to the tutorial [Consuming Measures.](https://help.sap.com/viewer/7e269da75d024ef09bfb7a5986c47517/Cloud/en-US)


[ACCORDION-END]
