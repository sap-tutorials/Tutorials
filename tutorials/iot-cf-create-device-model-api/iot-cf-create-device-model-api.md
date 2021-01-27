---
author_name: Supriya Cherukunnathu
author_profile: https://github.com/SupriyaCherukunnathu
title: Create a Device Model Using the API
description: Create a device model for the SAP Cloud Platform Internet of Things Service using the Device Management API.
auto_validation: true
primary_tag: topic>internet-of-things
tags: [ tutorial>beginner, tutorial>license, topic>internet-of-things, topic>cloud, products>sap-leonardo-iot, products>sap-edge-services, products>sap-cloud-platform-internet-of-things, products>sap-cloud-platform ]
---


## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn
- How to create a device model in the Internet of Things Service using the Device Management API

### Time to Complete
20 min

---
You need a tenant and a user with administrator role. For more information, please refer to the tutorial [Create User and Tenant](iot-cf-create-user-tenant).


[ACCORDION-BEGIN [Step 1: ](Create a Capability)]

In the following a capability is created. A capability can be reused since it can be assigned to multiple sensor types. Each capability can have one or many properties.

1.  Open the Device Management API:

    `https://<HOST_NAME>/<INSTANCE_ID>/iot/core/api/v1/doc/`

    You see the main page with the categories overview.

2.  Choose **Authorize**.

3.  Enter your user credentials.

4.  Choose **Authorize**.

5.  In the **Capabilities** category, choose the `POST /tenant/{tenantId}/capabilities` request.

6.  Choose **Try it out**.

7.  In the `tenantId` field, enter the ID of your tenant.

8.  Copy the **JSON string** and replace `{CapabilityName}` with your capability name, replace `{PropertyName}` with property name, replace `{DataTypeName}` with the data type for the property, and replace `{UnitOfMeasureName}` with the unit of the measure.

    ```JSON
    {
      "name": "{CapabilityName}",
      "properties": [
       {
          "name" : "{PropertyName}",
          "dataType" : "{DataTypeName}",
          "unitOfMeasure" : "{UnitOfMeasureName}"
        }
      ]
    }
    ```

    >Supported data types are: integer, long, float, double, boolean, string, binary, and date. For more information, please refer to section [Device Model](https://help.sap.com/viewer/2f1daa938df84fd090fa2a4da6e4bc05/Cloud/en-US/41c5d53fee864f2482b965cf4127a730.html) in the **Introduction**.

9.  Paste the modified **JSON string** in the **Example Value** field for the POST request.

    ```JSON
    {
      "name": "MyData",
      "properties": [
       {
          "name" : "temperature",
          "dataType" : "float",
          "unitOfMeasure" : "Celsius"
        },
        {
          "name" : "humidity",
          "dataType" : "float",
          "unitOfMeasure" : "g/kg"
        }
      ]
    }
    ```

10.  Choose **Execute**.

11.  Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created capability.

    ```JSON
    {
    "id": "2bc72771-595d-483e-be50-3b0d8e372bf1",
    "name": "MyData",
    "alternateId": "ba19f8b6584cf3bd",
    "properties": [
      {
        "name": "temperature",
        "dataType": "float",
        "unitOfMeasure": "Celsius"
      },
      {
        "name": "humidity",
        "dataType": "float",
        "unitOfMeasure": "g/kg"
      }
    ]
  }
    ```

12. Note down the `id` of the capability.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Sensor Type)]

In the following, a sensor type is created. The previously created capability is assigned to the sensor type.

1.  Navigate to the `SensorTypes` category.

2.  In the `SensorTypes` category, choose the `POST /tenant/{tenantId}/sensorTypes` request.

3.  Choose **Try it out**.

4.  In the `tenantId` field, enter the ID of your tenant.

5.  Copy the **JSON string** and replace `{SensorTypeName}` with your sensor type name and replace `{CapabilityId}` with the `id` of the capability noted down before.

    ```JSON
    {
  "capabilities": [
    {
      "id": "{CapabilityId}",
      "type": "{type}"
    }
  ],
  "name": "{SensorTypeName}"
}
    ```

6.  Paste the **JSON string** in the **Example Value** field for POST request.

    ```JSON
    {
      "capabilities": [
        {
          "id": "2bc72771-595d-483e-be50-3b0d8e372bf1",
          "type": "measure"
        }
      ],
      "name": "MySensorType"
    }
    ```

7.  Choose **Execute**.

8.  Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created sensor type.

    ```JSON
    {
      "id": "052bbcf8-a48d-4822-ad5e-fa1765783dfb",
      "alternateId": "1",
      "name": "MySensorType",
      "capabilities": [
        {
          "id": "2bc72771-595d-483e-be50-3b0d8e372bf1",
          "type": "measure"
        }
      ]
    }
    ```

9.  Note down the `id` of the sensor type.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a Device)]

In the following, a device entity is created. The device entity will be assigned to one specific gateway.

1.  Navigate to the **Gateways** category.

2.  In the **Gateways** category, choose the `GET /tenant/{tenantId}/gatewayss` request.

3.  Choose **Try it out**.

4.  In the `tenantId` field, enter the ID of your tenant.

5.  Scroll to the response body and note down the `id` of the gateway with the name `REST Gateway` (for example, `3`). If you want to create a MQTT device, note down the `id` of the gateway with the name `MQTT Gateway` (for example, `2`).

    >The IDs of the gateways are assigned in order of the appearance.

    ```JSON
    [
      {
        "id": "2",
        "name": "MQTT Gateway",
        "protocolId": "mqtt",
        "status": "online",
        "creationTimestamp": 1510043734156,
        "alternateId": "GATEWAY_CLOUD_MQTT",
        "type": "cloud"
      },
      {
        "id": "3",
        "name": "REST Gateway",
        "protocolId": "rest",
        "status": "online",
        "creationTimestamp": 1510043734531,
        "alternateId": "GATEWAY_CLOUD_REST",
        "type": "cloud"
      }
    ]
    ```

6.  Navigate to the **Devices** category.

7.  In the **Devices** category, choose the `POST /tenant/{tenantId}/devices` request.

8.  Choose **Try it out**.

9.  In the `tenantId` field, enter the ID of your tenant.

10.  Copy the **JSON string** and replace `{gatewayId}` with the `id` you noted down previously and replace `{deviceName}` with your device name.

    ```JSON
    {
      "gatewayId": "{gatewayId}",
      "name": "{deviceName}"
    }

    ```

11.  Paste the **JSON string** in the **Example Value** field for POST request.

    ```JSON
    {
    	"gatewayId" : "3",
    	"name": "MyDevice"
    }
    ```

    (Optional) You can configure the device as a router device, which is authorized to communicate on behalf of any other device in the same tenant. For more information, please refer to section [Connection as a Router Device](https://help.sap.com/viewer/643f531cbf50462c8cc45139ba2dd051/Cloud/en-US/455bdaaf40e44ff2981e68a5f6bcc6b2.html#loio7e0ec3a75b08481a97cba01b4e577566) in the **Internet of Things Gateway** documentation. Set the parameter as follows:

    ```JSON
    "authorizations":[{"type":"router"}]
    ```
    >The authorization policy cannot be changed anymore after the device has been created. Update is not supported for this property.

12. Choose **Execute**.

13. Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created device.

    ```JSON
    {
      "id": "5",
      "gatewayId": "3",
      "name": "MyDevice",
      "alternateId": "d6e50978854c0f9b",
      "creationTimestamp": 1510049163054,
      "online": true,    
      "authentications": [
        {
          "type": "clientCertificate"
        }
      ]
    }
    ```

14. Note down the `id` of the device.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Sensor)]

In the following, a sensor entity is created. The sensor will be assigned to the previously created device and is a kind of the previously created sensor type.

1.  Navigate to the **Sensors** category.

2.  In the **Sensors** category, choose the `POST /tenant/{tenantId}/sensors` request.

3.  Choose **Try it out**.

4.  In the `tenantId` field, enter the ID of your tenant.

5.  Copy the **JSON string** and replace `{DeviceId}` with the `id` of the device noted down before, replace `{SensorName}` with your sensor name, and replace `{SensorTypeId}` with the `id` of the sensor type noted down before.

    ```JSON
    {
      "deviceId": "{DeviceId}",
      "name": "{SensorName}",
      "sensorTypeId": "{SensorTypeId}"
    }
    ```

6.  Paste the **JSON string** in the **Example Value** field for POST request.

    ```JSON
    {
      "deviceId": "5",
      "name": "MySensor",
      "sensorTypeId": "052bbcf8-a48d-4822-ad5e-fa1765783dfb"
    }
    ```

7.  Choose **Execute**.

8.  Scroll to the **Server response** body and **Code**. In case of success the response code is `200` and the **Response body** contains all information of the created sensor.

    ```JSON
    {
      "id": "91",
      "alternateId": "716647de52209f61",
      "name": "MySensor",
      "deviceId": "5",
      "sensorTypeId": "052bbcf8-a48d-4822-ad5e-fa1765783dfb"
    }
    ```

8.  Note down the `id` of the sensor.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Send Data to a Specific Sensor)]

In the following, it is described how the payload looks like for sending data to a device with the created device model.

1.  You can address a sensor within the JSON message.

    ```JSON
    {
      "capabilityAlternateId": "{capabilityAlternateId}",
      "sensorAlternateId":"{sensorAlternateId}",
      "measures":[ ["{MEASURE}"] ]
    }
    ```

    -   Add the parameter `capabilityAlternateId` with the Alternate ID of the previously created capability.

    -   Add the parameter `sensorAlternateId` with the Alternate ID of the created sensor.

    -   Add the parameter `measures` with the array of measures (with each measure expressed as an object).

    ```JSON
    {
      "capabilityAlternateId": "ba19f8b6584cf3bd",
      "sensorAlternateId": "716647de52209f61",
      "measures": [{ "temperature":22.4, "humidity":90 }]
    }
    ```

    The message is now sent. For more information on how to publish data, please refer to the tutorials [Send Data with MQTT](iot-cf-send-data-mqtt) and [Send Data with REST](iot-cf-send-data-rest).

2.  You can check the incoming values using the **Data Visualization** of the device in the Internet of Things Service Cockpit or the Device Management API. For more information, please refer to the tutorial [Consume Measures](iot-cf-consume-measures).

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Send Data to an Automatically Created Sensor)]

1.  Alternatively, you can send data using the sensor type ID. In this case the system automatically creates a new sensor with the provided sensor type.

    ```JSON
    {
      "sensorAlternateId":"{sensorAlternateId}",
      "capabilityAlternateId": "{capabilityAlternateId}",
      "sensorTypeAlternateId":"{sensorTypeAlternateId}",
      "measures":[ ["{MEASURE}"] ]
    }
    ```

    -   Add the parameter `sensorAlternateId` with the Alternate ID of the sensor to be created.

    -   Add the parameter `capabilityAlternateId` with the Alternate ID of the previously created capability.

    -   Add the parameter `sensorTypeAlternateId` with the Alternate ID of the created sensor type. (A new sensor associated to the sensor type will be automatically created.)

    -   Add the parameter `measures` with the array of measures (with each measure expressed as an object).

    ```JSON
    {
      "sensorAlternateId":"sensor1",
      "capabilityAlternateId": "ba19f8b6584cf3bd",
      "sensorTypeAlternateId": "1",
      "measures": [{ "temperature":22.4, "humidity":90 }]
    }
    ```

    The message is now sent. For more information on how to publish data, please refer to the tutorials [Send Data with MQTT](iot-cf-send-data-mqtt) and [Send Data with REST](iot-cf-send-data-rest).

2.  You can check the incoming values using the **Data Visualization** of the device in the Internet of Things Service Cockpit or the Device Management API. For more information, please refer to the tutorial [Consume Measures](iot-cf-consume-measures).

[VALIDATE_1]

[ACCORDION-END]
