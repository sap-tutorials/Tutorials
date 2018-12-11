---
title: Send Commands with MQTT
description: Send commands to the SAP Cloud Platform Internet of Things Service Cloud using MQTT.
auto_validation: true
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things, topic>cloud ]
---

<!-- loio957975dfd7ad42cfbda0402c5f0e936b -->

## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Install the Paho Client](https://www.sap.com/developer/tutorials/iot-cf-install-paho-client.html) and [Install OpenSSL](https://www.sap.com/developer/tutorials/iot-cf-install-openssl.html).

## Details
### You will learn
- How to send commands to the Internet of Things Service Cloud using MQTT
- How to use Paho as a sample client for MQTT

### Time to Complete
15 min

---

[ACCORDION-BEGIN [Step 1: ](Create a Capability)]

1.  Open the Internet of Things API Service UI.

    ```bash
    http://<HOST_NAME>/iot/core/api/v1/doc/
    ```

    You will see the main page with the categories overview.

2.  Choose *Authorize*.

3.  Log on with your user credentials.

4.  In the *Capabilities* category, choose the `POST /capabilities` request.

5.  Choose *Try it out*.

6.  Copy the *JSON string* and replace `{capabilityName}` with your capability name, replace `{propertyName}` with property name, replace `{dataTypeName}` with the data type for the property, and replace `{unitOfMeasureName}` with the unit of the measure.

    ```bash
    {
      "name": "{capabilityName}",
      "properties": [
       {
          "name" : "{propertyName}",
          "dataType" : "{dataTypeName}",
          "unitOfMeasure" : "{unitOfMeasureName}"
        }
      ]
    }
    ```

    > Note:
    > Supported data types are: integer, long, float, double, boolean, string, binary, and date. For more information, please refer to section [Device Model](https://help.sap.com/viewer/2f1daa938df84fd090fa2a4da6e4bc05/Cloud/en-US/41c5d53fee864f2482b965cf4127a730.html) in the *Introduction*.
    >
    >

7.  Paste the modified *JSON string* in the *Example Value* field for the POST request.

    ```bash
    {
    "name": "MyCapability",
      "properties": [
        {
          "name" : "LED",
          "dataType" : "boolean"
        },
        {
          "name" : "Buzzer",
          "dataType" : "boolean"
        },
        {
          "name" : "Speed",
          "dataType" : "float",
          "unitOfMeasure" : "mph"
        }
      ]
    }
    ```

8.  Choose *Execute*.

9.  Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created capability.

    ```bash
    {
      "id": "3772113f-d8bf-4dc5-9968-847845b8e8ee",
      "name": "MyCapability",
      "alternateId": "e6ae441b8820c91f",
      "properties": [
        {
          "name": "LED",
          "dataType": "boolean"
        },
        {
          "name": "Buzzer",
          "dataType": "boolean"
        },
        {
          "name": "Speed",
          "dataType": "float",
          "unitOfMeasure": "mph",
          "formatter": {
            "scale": 0,
            "shift": 0,
            "swap": false
          }
        }
      ]
    }
    ```

10.  Note down the `id` of the capability.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Sensor Type)]

1.  Navigate to the `SensorTypes` category.

2.  In the `SensorTypes` category, choose the `POST /sensorTypes` request.

3.  Choose *Try it out*.

4.  Copy the *JSON string* and replace `{sensorTypeName}` with your sensor type name and replace `{capabilityId}` with the `id` of the capability noted down before.

    ```bash
    {
      "capabilities": [
        {
          "id": "{capabilityId}",
          "type": "command"
        }
      ],
      "name": "{sensorTypeName}"
    }
    ```

5.  Paste the *JSON string* in the *Example Value* field for POST request.

    ```bash
    {
      "capabilities": [
        {
          "id": "3772113f-d8bf-4dc5-9968-847845b8e8ee",
          "type": "command"
        }
      ],
      "name": "MySensorType"
    }
    ```

6.  Choose *Execute*.

7.  Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created sensor type

    ```bash
    {
      "id": "a0b92e49-45fe-4ae1-805f-5fafcbb65f84",
      "alternateId": "4",
      "name": "MySensorType",
      "capabilities": [
        {
          "id": "3772113f-d8bf-4dc5-9968-847845b8e8ee",
          "type": "command"
        }
      ]
    }
    ```

8.  Note down the `id` of the sensor type.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create an MQTT Device)]

1.  Navigate to the *Gateways* category.

2.  In the *Gateways* category, choose the `GET /gateways` request.

3.  Choose *Try it out*.

4.  Choose *Execute*.

5.  Scroll to the response body and note down the `id` of the gateway with the name `IoT Gateway MQTT` (for example, `2`).

    ```bash
    [
      {
        "id": "2",
        "name": "IoT Gateway MQTT",
        "protocolId": "mqtt",
        "status": "online",
        "type": "cloud",
        "creationTimestamp": 1525261225950
      },
      {
        "id": "3",
        "name": "IoT Gateway REST",
        "protocolId": "rest",
        "status": "online",
        "type": "cloud",
        "creationTimestamp": 1525261230192
      }
    ]
    ```

    > Note:
    > The IDs of the gateways are assigned in order of the appearance.
    >
    >

6.  Navigate to the *Devices* category.

7.  In the *Devices* category, choose the `POST /devices` request.

8.  Choose *Try it out*.

9.  Copy the *JSON string* and replace `{gatewayId}` with the `id` you noted down previously and replace `{deviceName}` with your device name.

    ```bash
    {
      "gatewayId": "{gatewayId}",
      "name": "{deviceName}"
    }

    ```

10. Paste the *JSON string* in the *Example Value* field for POST request.

    ```bash
    {
    	"gatewayId" : "2",
    	"name": "MyDevice"
    }

    ```

11. Choose *Execute*.

12. Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created device.

    ```bash
    {
      "id": "5",
      "gatewayId": "2",
      "name": "MyDevice",
      "alternateId": "cb1bc8935375529e",
      "creationTimestamp": 1525757977416,
      "status": "fullyFunctional",
      "online": true,
      "sensors": [
        {
          "id": "10",
          "deviceId": "5",
          "sensorTypeId": "0",
          "name": "Sensor: 0:0:0:0",
          "alternateId": "0:0:0:0"
        }
      ],
      "authentications": [
        {
          "type": "clientCertificate"
        }
      ]
    }
    ```

13. Note down the `id` and the `alternateId` of the device.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Sensor)]

1.  Navigate to the *Sensors* category.

2.  In the *Sensors* category, choose the `POST /sensors` request.

3.  Choose *Try it out*.

4.  Copy the *JSON string* and replace `{deviceId}` with the `id` of the device noted down before, replace `{sensorName}` with your sensor name, and replace `{sensorTypeId}` with the `id` of the sensor type noted down before.

    ```bash
    {
      "deviceId": "{deviceId}",
      "name": "{sensorName}",
      "sensorTypeId": "{sensorTypeId}"
    }
    ```

5.  Paste the *JSON string* in the *Example Value* field for POST request.

    ```bash
    {
      "deviceId": "5",
      "name": "MySensor",
      "sensorTypeId": "a0b92e49-45fe-4ae1-805f-5fafcbb65f84"
    }
    ```

6.  Choose *Execute*.

7.  Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created sensor.

    ```bash
    {
      "id": "11",
      "deviceId": "5",
      "sensorTypeId": "a0b92e49-45fe-4ae1-805f-5fafcbb65f84",
      "name": "MySensor",
      "alternateId": "07c0a15c0d0baeb7"
    }
    ```

8.  Note down the `id` of the sensor.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Generate the Device Certificate)]

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    ```bash
    https://<HOST_NAME>/iot/cockpit/
    ```

2.  On the home page, select a tenant first and then use the main menu to navigate to the *Devices* section of the *Device Management* category.

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

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Send Commands to the Internet of Things Service)]

**Prerequisites:**

-   You have created the device model in step 1-4.

-   You have generated a certificate for your MQTT device and wrote down the secret key.


**Connecting the Paho Client to the Device**

1.  Open the Paho client.

    > Note:
    > You must be connected to public Internet. Most corporate networks do not work due to port and protocol restrictions.
    >
    >

2.  Choose **+** in the *Connections* tab to create a new connection.

3.  Choose the *Options* tab of the connection.

4.  Enable the checkbox *Enable SSL*.

5.  Choose *Browse* for *Key Store Location* and choose the `client.ks` file downloaded for the device.

    > Note:
    > You might need to change the file type to **`*.*`** in the file selection menu.
    >
    >

6.  Choose *Browse* for *Trust Store Location* and choose the `/jre/lib/security/cacerts` of your local Java installation folder if you use a trusted certification authority like Verisign.

    > Note:
    > You might need to change the file type to **`*.*`** in the file selection menu.
    >
    >

7.  Enter the copied or noted *Secret* key into the field for the *Key Store Password*.

8.  Set the *Trust Store Password* to **`changeit`**.

9.  Choose *MQTT* tab of the connection.

10. Add the *Server URI* as follows: `ssl://<HOST_NAME>:8883` → `ssl://demo.eu10.cp.iot.sap:8883`

11. Add the *Client ID*: The Alternate ID of the device as a string: `cb1bc8935375529e`

12. Choose *Connect*.

    The status is changed to *Connected*.

13. Choose **+** in the *Subscription* section and enter the topic.

    Topic: `commands/<DEVICE_ALTERNATE_ID>`

14. Enter the recorded `<DEVICE_ALTERNATE_ID>` as a string: `commands/cb1bc8935375529e`.

15. Choose *Subscribe*.

    The Paho client is now connected to the device. Incoming commands will appear in the *History* tab.


**Sending Commands Using the Internet of Things API Service**

1.  Switch back to the Internet of Things API Service UI.

    ```bash
    http://<HOST_NAME>/iot/core/api/v1/doc/
    ```

    You will see the main page with the categories overview.

2.  Navigate to the *Devices* category.

3.  In the *Devices* category, choose the `POST /devices/{deviceId}/commands` request.

4.  Choose *Try it out*.

5.  Copy the *JSON string* and replace `{sensorId}` and `{capabilityId}` with the `ids` you noted down previously.

    ```bash
    {
        "sensorId": "{sensorId}",
        "capabilityId": "{capabilityId}",
        "command": {
            "LED": "true",
            "Buzzer": "true",
            "Speed": "50.0"
        }
    }
    ```

6.  Enter the `{deviceId}` you noted down previously as *Unique identifier of a device* and paste the *JSON string* in the *Example Value* field for the POST request.

    ```bash
    {
        "sensorId": "11",
        "capabilityId": "3772113f-d8bf-4dc5-9968-847845b8e8ee",
        "command": {
            "LED": "true",
            "Buzzer": "true",
            "Speed": "50.0"
        }
    }
    ```

7.  Choose *Execute*

8.  Scroll to the *Server response* body and *Code*.

    In case of success the response code is `200` and the *Response body* contains a success message.

    ```bash
    {
      "message": "Command issued successfully."
    }
    ```


**Receiving Commands Using the Paho Client**

1.  Switch back to the Paho client.

    You can find the command received by the device below the *History* tab in the *Event* column.

2.  Double click the *Received* *Event* to open the *Message Viewer* dialog and find the received command *Message*.

    ```bash
    {"sensorAlternateId":"07c0a15c0d0baeb7","capabilityAlternateId":"e6ae441b8820c91f"
    ,"command":{"LED":"true","Buzzer":"true","Speed":50.0}}
    ```
[DONE]

[ACCORDION-END]
