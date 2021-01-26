---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Send Commands with MQTT
description: Send commands to the SAP Cloud Platform Internet of Things Service Cloud using MQTT.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner
 - **Tutorials:** You have completed [Install the Paho Client](iot-cf-install-paho-client) and [Install OpenSSL](iot-cf-install-openssl).

## Details
### You will learn
- How to send commands to the Internet of Things Service Cloud using MQTT
- How to use Paho as a sample client for MQTT

### Time to Complete
15 min

---

You need a tenant and a user with administrator role. For more information, please refer to the tutorial [Create User and Tenant](iot-cf-create-user-tenant).

You have to create a device model for the Internet of Things Service using the Device Management API. For detailed information about the entities of a device model, please refer to section [Device Model](https://help.sap.com/viewer/2f1daa938df84fd090fa2a4da6e4bc05/Cloud/en-US/41c5d53fee864f2482b965cf4127a730.html) in the **Introduction**.

[ACCORDION-BEGIN [Step 1: ](Create a Capability)]

In the following, a capability is created. A capability can be reused since it can be assigned to multiple sensor types. Each capability can have one or many properties.

1.  Open the Device Management API:

    `http://<HOST_NAME>/<INSTANCE_ID>/iot/core/api/v1/doc/`

    You will see the main page with the categories overview.

2.  Choose **Authorize**.

3.  Enter your user credentials.

4.  Choose **Authorize**.

5.  In the **Capabilities** category, choose the `POST /tenant/{tenantId}/capabilities` request.

6.  Choose **Try it out**.

7.  In the `tenantId field`, enter the ID of your tenant.

8.  Copy the **JSON string** and replace `{capabilityName}` with your capability name, replace `{propertyName}` with property name, replace `{dataTypeName}` with the data type for the property, and replace `{unitOfMeasureName}` with the unit of the measure.

    ```JSON
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

    >Supported data types are: integer, long, float, double, boolean, string, binary, and date. For more information, please refer to section [Device Model](https://help.sap.com/viewer/2f1daa938df84fd090fa2a4da6e4bc05/Cloud/en-US/41c5d53fee864f2482b965cf4127a730.html) in the **Introduction**.

9.  Paste the modified **JSON string** in the **Example Value** field for the POST request.

    ```JSON
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

10.  Choose **Execute**.

11.  Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created capability.

    ```JSON
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
         }
        }
      ]
    }
    ```

12.  Note down the `id` of the capability.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Sensor Type)]

1.  Navigate to the `SensorTypes` category.

2.  In the `SensorTypes` category, choose the `POST /tenant/{tenantId}/sensorTypess` request.

3.  Choose **Try it out**.

4.  In the `tenantId field`, enter the ID of your tenant.

5.  Copy the **JSON string** and replace `{sensorTypeName}` with your sensor type name and replace `{capabilityId}` with the `id` of the capability noted down before.

    ```JSON
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

6.  Paste the **JSON string** in the **Example Value** field for POST request.

    ```JSON
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

7.  Choose **Execute**.

8.  Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created sensor type

    ```JSON
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

9.  Note down the `id` of the sensor type.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create an MQTT Device)]

In the following, a device entity is created. The device entity is assigned to one specific gateway.

1.  Navigate to the **Gateways** category.

2.  In the **Gateways** category, choose the `GET /tenant/{tenantId}/gateways` request.

3.  Choose **Try it out**.

4.  In the `tenantId field`, enter the ID of your tenant.

5.  Choose **Execute**.

6.  Scroll to the response body and note down the `id` of the gateway with the name `IoT Gateway MQTT` (for example, `2`).

    ```JSON
    [
  {
    "id": "2",
    "name": "MQTT Gateway",
    "protocolId": "mqtt",
    "status": "online",
    "creationTimestamp": 152526122595,
    "alternateId": "GATEWAY_CLOUD_MQTT",
    "type": "cloud"
  },
  {
    "id": "3",
    "name": "REST Gateway",
    "protocolId": "rest",
    "status": "online",
    "creationTimestamp": 1525261230192,
    "alternateId": "GATEWAY_CLOUD_REST",
    "type": "cloud"
  }
]
    ```

    >The IDs of the gateways are assigned in order of the appearance.

7.  Navigate to the **Devices** category.

8.  In the **Devices** category, choose the `POST /tenant/{tenantId}/devices` request.

9.  Choose **Try it out**.

10. In the `tenantId field`, enter the ID of your tenant.

11.  Copy the **JSON string** and replace `{gatewayId}` with the `id` you noted down previously and replace `{deviceName}` with your device name.

    ```JSON
    {
      "gatewayId": "{gatewayId}",
      "name": "{deviceName}"
    }

    ```

12. Paste the **JSON string** in the **Example Value** field for POST request.

    ```JSON
    {
    	"gatewayId" : "2",
    	"name": "MyDevice"
    }

    ```

13. Choose **Execute**.

14. Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created device.

    ```JSON
    {
      "id": "5",
      "gatewayId": "2",
      "name": "MyDevice",
      "alternateId": "cb1bc8935375529e",
      "creationTimestamp": 1525757977416,
      "online": true,
      "authentications": [
        {
          "type": "clientCertificate"
        }
      ]
    }
    ```

15. Note down the `id` and the `alternateId` of the device.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Sensor)]

1.  Navigate to the **Sensors** category.

2.  In the **Sensors** category, choose the `POST /tenant/{tenantId}/sensors` request.

3.  Choose **Try it out**.

4.  In the `tenantId field`, enter the ID of your tenant.

5.  Copy the **JSON string** and replace `{deviceId}` with the `id` of the device noted down before, replace `{sensorName}` with your sensor name, and replace `{sensorTypeId}` with the `id` of the sensor type noted down before.

    ```JSON
    {
      "deviceId": "{deviceId}",
      "name": "{sensorName}",
      "sensorTypeId": "{sensorTypeId}"
    }
    ```

6.  Paste the **JSON string** in the **Example Value** field for POST request.

    ```JSON
    {
      "deviceId": "5",
      "name": "MySensor",
      "sensorTypeId": "a0b92e49-45fe-4ae1-805f-5fafcbb65f84"
    }
    ```

7.  Choose **Execute**.

8.  Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created sensor.

    ```JSON
    {
      "id": "11",
      "deviceId": "5",
      "sensorTypeId": "a0b92e49-45fe-4ae1-805f-5fafcbb65f84",
      "name": "MySensor",
      "alternateId": "07c0a15c0d0baeb7"
    }
    ```

9.  Note down the `id` of the sensor.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Generate the Device Certificate)]

**Prerequisites:**

You have installed OpenSSL. A description on how to install OpenSSL can be found in the tutorial [Install OpenSSL](iot-cf-install-openssl).

1.  Log on to the Internet of Things Service Cockpit with your user credentials.

    `https://<HOST_NAME>/<INSTANCE_ID>/iot/cockpit/`

2.  On the **My Tenants** page, select a tenant first.

3.  Use the main menu to navigate to the **Devices** section of the **Device Management** category.

    All devices are listed.

4.  Choose the previously created device.

5.  On the device details page, choose the **Certificate** tab.

6.  In the **Generate Certificate** dialog, select the type of certificate you want to generate. For this tutorial, please use `pem`

    >Supported types are PEM and P12. Based on the certificate type you choose, the system downloads a `*-device_certificate.pem` or `*-device_certificate.p12` and a dialog opens, which shows the **Secret** key.

7. Choose **Generate**.

8.  Select and copy the displayed **Secret** key before closing the dialog or leaving the page, as it cannot be restored at a later point in time.

9.  Rename `*-device_certificate.pem` to `certificate.pem`.

10.  Open the console in the directory where the previously downloaded `*.pem` file is located. Enter the following command in the terminal:

    `openssl pkcs12 -export -in certificate.pem -inkey certificate.pem -out client.ks`

    >Use the copied **Secret** key from the previous step for all password requests (import pass phrase and Export Password).

    A file named `client.ks` is created.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Send Commands to the Internet of Things Service)]

**Prerequisites:**

-   You have installed Java SE Runtime Environment 8.

-   You have installed the MQTT client (Paho). A description of how to install the Paho client can be found in the tutorial [Install the Paho Client](iot-cf-install-paho-client).

    >For ease of use, we recommend that you add `JAVA_HOME` to your PATH environment variables to point to where the JDK software is located.

-   You have created the device model in step 1-4.

-   You have generated a certificate for your MQTT device and wrote down the secret key.


**Connecting the Paho Client to the Device**

1.  Open the Paho client.

    >You must be connected to public Internet. Most corporate networks do not work due to port and protocol restrictions.

2.  Choose **+** in the **Connections** tab to create a new connection.

    The system opens a new tab with the connection details.

3.  Choose the **Options** tab of the connection.

4.  Enable the checkbox **Enable SSL**.

5.  Choose **Browse** for **Key Store Location** and choose the `client.ks` file downloaded for the device.

    >You might need to change the file type to **`*.*`** in the file selection menu.

6.  Choose **Browse** for **Trust Store Location** and choose the `/jre/lib/security/cacerts` of your local Java installation folder if you use a trusted certification authority, for example Verisign.

    >You might need to change the file type to **`*.*`** in the file selection menu.

7.  Enter the copied or noted **Secret** key into the field for the **Key Store Password**.

8.  Set the **Trust Store Password** to **`changeit`**.

9.  Choose **MQTT** tab of the connection.

10. Add the **Server URI** as follows: `ssl://<HOST_NAME>:8883` → `ssl://demo.eu10.cp.iot.sap:8883`

11. Add the **Client ID**: The Alternate ID of the device as a string: `cb1bc8935375529e`

12. Choose **Connect**.

    The status is changed to **Connected**.

13. Choose **+** in the **Subscription** section and enter the topic.

    Topic: `commands/<DEVICE_ALTERNATE_ID>`

    Enter the recorded `<DEVICE_ALTERNATE_ID>` as a string: `commands/cb1bc8935375529e`.

14. Choose **Subscribe**.

    The Paho client is now connected to the device. Incoming commands will appear in the **History** tab.


**Sending Commands Using the Internet of Things API Service**

1.  Switch back to the Device Management API:

    `http://<HOST_NAME>/<INSTANCE_ID>/iot/core/api/v1/doc/`

    You will see the main page with the categories overview.

2.  Navigate to the **Devices** category.

3.  In the **Devices** category, choose the `POST /tenant/{tenantId}/devices/{deviceId}/commands` request.

4.  Choose **Try it out**.

5.  In the `tenantId field`, enter the ID of your tenant.

6.  Copy the **JSON string** and replace `{sensorId}` and `{capabilityId}` with the `ids` you noted down previously.

    ```JSON
    {
        "sensorId": "{sensorId}",
        "capabilityId": "{capabilityId}",
        "command": {
            "LED": true,
            "Buzzer": true,
            "Speed": 50.0
        }
    }
    ```

7.  Enter the `{deviceId}` you noted down previously as **Unique identifier of a device** and paste the **JSON string** in the **Example Value** field for the POST request.

    ```JSON
    {
        "sensorId": "11",
        "capabilityId": "3772113f-d8bf-4dc5-9968-847845b8e8ee",
        "command": {
            "LED": true,
            "Buzzer": true,
            "Speed": 50.0
        }
    }
    ```

8.  Choose **Execute**

9.  Scroll to the **Server response** body and **Code**.

    In case of success the response code is `202` and the **Response body** contains a success message.

    ```JSON
    {
      "message": "Command issued successfully."
    }
    ```


**Receiving Commands Using the Paho Client**

1.  Switch back to the Paho client.

    You can find the command received by the device below the **History** tab in the **Event** column.

2.  Double click the **Received** **Event** to open the **Message Viewer** dialog and find the received command **Message**.

    ```JSON
    {"sensorAlternateId":"07c0a15c0d0baeb7","capabilityAlternateId":"e6ae441b8820c91f"
    ,"command":{"LED":true,"Buzzer":true,"Speed":50.0}}
    ```

[DONE]

[ACCORDION-END]
