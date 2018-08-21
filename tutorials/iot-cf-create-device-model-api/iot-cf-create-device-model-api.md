---
title: Create a Device Model Using the API
description: Create a device model for the SAP Cloud Platform Internet of Things Service using the Internet of Things API Service.
auto_validation: true
primary_tag: products>sap-cloud-platform-internet-of-things
tags: [ tutorial>beginner, products>sap-cloud-platform-internet-of-things, topic>internet-of-things, topic>cloud ]
---

<!-- loioc4da75750e5b42488cb5874d3fc623da -->

## Prerequisites
 - **Proficiency:** Beginner

## Details
### You will learn
- How to create a device model in the Internet of Things Service using the Internet of Things API Service

### Time to Complete
20 min

---

[ACCORDION-BEGIN [Step 1: ](Create a Capability Using the API)]

In the following a capability is created. A capability can be reused since it can be assigned to multiple sensor types. Each capability can have one or many properties.

1.  Open the Internet of Things API Service UI.

    ```bash
    https://<HOST_NAME>/iot/core/api/v1/doc/
    ```

    You see the main page with the categories overview.

2.  Choose *Authorize* .

3.  Log on with your user credentials.

4.  In the *Capabilities* category, choose the `POST /capabilities` request.

5.  Choose *Try it out*.

6.  Copy the *JSON string* and replace `{CapabilityName}` with your capability name, replace `{PropertyName}` with property name, replace `{DataTypeName}` with the data type for the property, and replace `{UnitOfMeasureName}` with the unit of the measure.

    ```bash
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

    > Note:
    > Supported data types are: integer, long, float, double, boolean, string, binary, and date. For more information, please refer to section [Device Model](https://help.sap.com/viewer/2f1daa938df84fd090fa2a4da6e4bc05/Cloud/en-US/41c5d53fee864f2482b965cf4127a730.html) in the *Introduction*.
    >
    >

7.  Paste the modified *JSON string* in the *Example Value* field for the POST request.

    ```bash
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

8.  Choose *Execute*.

9.  Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created capability.

    ```bash
    {
      "id": "2bc72771-595d-483e-be50-3b0d8e372bf1",
      "name": "MyData",
      "alternateId": "ba19f8b6584cf3bd",
      "properties": [
        {
          "name": "temperature",
          "dataType": "float",
          "unitOfMeasure": "Celsius",
          "formatter": {
            "scale": 0,
            "shift": 0,
            "swap": false
          }
        },
        {
          "name": "humidity",
          "dataType": "float",
          "unitOfMeasure": "g/kg",
          "formatter": {
            "scale": 0,
            "shift": 0,
            "swap": false
          }
        }
      ]
    }
    ```

10. Note down the `id` of the capability.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create a Sensor Type Using the API)]

In the following a sensor type is created. The previously created capability is assigned to the sensor type.

1.  Navigate to the `SensorTypes` category.

2.  In the `SensorTypes` category, choose the `POST /sensorTypes` request.

3.  Choose *Try it out*.

4.  Copy the *JSON string* and replace `{SensorTypeName}` with your sensor type name and replace `{CapabilityId}` with the `id` of the capability noted down before.

    ```bash
    {
      "capabilities": [
        {
          "id": "{CapabilityId}"
        }
      ],
      "name": "{SensorTypeName}"
    }
    ```

5.  Paste the *JSON string* in the *Example Value* field for POST request.

    ```bash
    {
      "capabilities": [
        {
          "id": "2bc72771-595d-483e-be50-3b0d8e372bf1",
          "type": "MEASURE"
        }
      ],
      "name": "MySensorType"
    }
    ```

6.  Choose *Execute*.

7.  Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created sensor type.

    ```bash
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

8.  Note down the `id` of the sensor type.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a Device Using the API)]

In the following a device entity is created. The device entity does not have any sensors attached, yet. The device entity will be assigned to one specific gateway.

1.  Navigate to the *Gateways* category.

2.  In the *Gateways* category, choose the `GET /gateways` request.

3.  Choose *Try it out*.

4.  Scroll to the response body and note down the `id` of the gateway with the name `IoT Gateway REST` (for example, `3`). If you want to create a MQTT device, note down the `id` of the gateway with the name `IoT Gateway MQTT` (for example, `2`).

    > Note:
    > The IDs of the gateways are assigned in order of the appearance.
    >
    >

    ```bash
    [
      {
        "id": "2",
        "name": "IoT Gateway MQTT",
        "protocolId": "mqtt",
        "status": "online",
        "creationTimestamp": 1510043734156
      },
      {
        "id": "3",
        "name": "IoT Gateway REST",
        "protocolId": "rest",
        "status": "online",
        "creationTimestamp": 1510043734531
      }
    ]
    ```

5.  Navigate to the *Devices* category.

6.  In the *Devices* category, choose the `POST /devices` request.

7.  Choose *Try it out*.

8.  Copy the *JSON string* and replace `{gatewayId}` with the `id` you noted down previously and replace `{deviceName}` with your device name.

    ```bash
    {
      "gatewayId": "{gatewayId}",
      "name": "{deviceName}"
    }

    ```

9.  Paste the *JSON string* in the *Example Value* field for POST request.

    ```bash
    {
    	"gatewayId" : "3",
    	"name": "MyDevice"
    }
    ```

    (Optional) You can set the device to a router device, which is allowed to send data on behalf of another device. Set the parameter as follows:

    ```bash
    "authorizations":[{"type":"router"}]
    ```

    > Note:
    > The authorization policy cannot be changed anymore after the device has been created. Update is not supported for this property.
    >
    >

10. Choose *Execute*.

11. Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created device.

    ```bash
    {
      "id": "5",
      "gatewayId": "3",
      "name": "MyDevice",
      "alternateId": "d6e50978854c0f9b",
      "creationTimestamp": 1510049163054,
      "status": "fullyFunctional",
      "online": true,
      "sensors": [
        {
          "id": "105",
          "deviceId": "5",
          "sensorTypeId": "0",
          "name": "Sensor: 00:00:00:00",
          "alternateId": "00:00:00:00"
        }
      ],
      "authentications": [
        {
          "type": "clientCertificate"
        }
      ]
    }
    ```

12. Note down the `id` of the device.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a Sensor Using the API)]

In the following a sensor entity is created. The sensor will be assigned to the previously created device and will be a kind of the previously created sensor type.

1.  Navigate to the *Sensors* category.

2.  In the *Sensors* category, choose the `POST /sensorss` request.

3.  Choose *Try it out*.

4.  Copy the *JSON string* and replace `{DeviceId}` with the `id` of the device noted down before, replace `{SensorName}` with your sensor name, and replace `{SensorTypeId}` with the `id` of the sensor type noted down before.

    ```bash
    {
      "deviceId": "{DeviceId}",
      "name": "{SensorName}",
      "sensorTypeId": "{SensorTypeId}"
    }
    ```

5.  Paste the *JSON string* in the *Example Value* field for POST request.

    ```bash
    {
      "deviceId": "5",
      "name": "MySensor",
      "sensorTypeId": "052bbcf8-a48d-4822-ad5e-fa1765783dfb"
    }
    ```

6.  Choose *Execute*.

7.  Scroll to the *Server response* body and *Code*. In case of success the response code is `200` and the *Response body* contains all information of the created sensor.

    ```bash
    {
      "id": "91",
      "deviceId": "5",
      "sensorTypeId": "052bbcf8-a48d-4822-ad5e-fa1765783dfb",
      "name": "MySensor",
      "alternateId": "716647de52209f61"
    }
    ```

8.  Note down the `id` of the sensor.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Send Data)]

In the following it is described how the payload looks like for sending data to a device with the created device model.

**Send Data to a Specific Sensor**

1.  You can address a sensor within the JSON message.

    ```bash
    {
    "capabilityAlternateId": "{capabilityAlternateId}",
    "sensorAlternateId":"{sensorAlternateId}",
    "measures":[ ["{MEASURE}"] ]
    }
    ```

    -   Add the parameter `capabilityAlternateId` with the alternate id of the previously created capability.

    -   Add the parameter `sensorAlternateId` with the alternateId of the created sensor.

    -   Add the parameter `measures` with the array of measures (with each measure expressed as an array of values for the number of defined properties).

    ```bash
    {
    "capabilityAlternateId": "ba19f8b6584cf3bd",
    "sensorAlternateId": "716647de52209f61",
    "measures": [{ "temperature":22.4, "humidity":90 }]
    }
    ```

    The message is now sent to the previously created sensor. For more information on how to publish data, please refer to the tutorials [Send Data with MQTT](https://www.sap.com/developer/tutorials/iot-cf-send-data-mqtt.html) and [Send Data with REST](https://www.sap.com/developer/tutorials/iot-cf-send-data-rest.html).

2.  You can check the incoming values using the *Data Visualization* of the device in the Internet of Things Service Cockpit or the Internet of Things API Service. For more information, please refer to the tutorial [Consume Measures](https://www.sap.com/developer/tutorials/iot-cf-consume-measures.html).


**Send Data to an Automatically Created Sensor**

1.  Alternatively, you can send data using the sensor type ID. In this case the system automatically creates a new sensor with the provided sensor type.

    ```bash
    {
    "sensorAlternateId":"{sensorAlternateId}",
    "capabilityAlternateId": "{capabilityAlternateId}",
    "sensorTypeAlternateId":"{sensorTypeAlternateId}",
    "measures":[ ["{MEASURE}"] ]
    }
    ```

    -   Add the parameter `sensorAlternateId` with the alternate id of the sensor to be created.

    -   Add the parameter `capabilityAlternateId` with the alternate id of the previously created capability.

    -   Add the parameter `sensorTypeAlternateId` with the alternate id of the created sensor type. (A new sensor associated to the sensor type will be automatically created.)

    -   Add the parameter `measures` with the array of measures (with each measure expressed as an array of values for the number of defined properties).

    ```bash
    {
    "sensorAlternateId":"sensor1",
    "capabilityAlternateId": "ba19f8b6584cf3bd",
    "sensorTypeAlternateId": "1",
    "measures": [{ "temperature":22.4, "humidity":90 }]
    }
    ```

    The message is now sent to a newly created sensor. For more information on how to publish data, please refer to the tutorials [Send Data with MQTT](https://www.sap.com/developer/tutorials/iot-cf-send-data-mqtt.html) and [Send Data with REST](https://www.sap.com/developer/tutorials/iot-cf-send-data-rest.html).

2.  You can check the incoming values using the *Data Visualization* of the device in the Internet of Things Service Cockpit or the Internet of Things API Service. For more information, please refer to the tutorial [Consume Measures](https://www.sap.com/developer/tutorials/iot-cf-consume-measures.html).

[VALIDATE_1]

[ACCORDION-END]
