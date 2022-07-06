---
author_name: Marco Porru
author_profile: https://github.com/marco-porru
title: Onboard a New Device and Thing
description: Onboard a new Device and a Thing, complete the digital twin mapping  and test the ingestion services for an IoT Edge node using the Edge Gateway Service.
auto_validation: true
time: 15
tags: [ tutorial>beginner, software-product-function>sap-btp-cockpit, software-product>sap-business-technology-platform, software-product>sap-btp--cloud-foundry-environment, tutorial>license]
primary_tag: topic>internet-of-things
---

## Prerequisites
 -   You have licensed SAP Internet of Things (with the new capacity unit based licensing introduced in August 2020, your company has a Cloud Platform Enterprise Agreement or Pay-As-You-Go for SAP BTP and you have subscribed to the `oneproduct` service plan)
 -   You have setup the subscription for SAP IoT in your global account in a tenant (e.g. in the DEV tenant, the guide for the basic setup is at [Get Started with Your SAP IoT Account](https://help.sap.com/viewer/195126f4601945cba0886cbbcbf3d364/latest/en-US/bfe6a46a13d14222949072bf330ff2f4.html) ).
 - You have knowledge how to [manage users](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/a3bc7e863ac54c23ab856863b681c9f8.html) and [role collections](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/9e1bf57130ef466e8017eab298b40e5e.html) in the SAP Business Technology Platform
 - Your SAP User has at a minimum the `iot_role_collection` created during onboarding of your tenant and the associated roles (see [SAP Help on Providing Authorizations](https://help.sap.com/viewer/195126f4601945cba0886cbbcbf3d364/latest/en-US/2810dd61e0a8446d839c936f341ec46d.html) ) and all the required roles for the SAP Internet of Things Edge feature, see [Configure Role Collections for Users](https://help.sap.com/viewer/247022ddd1744053af376344471c0821/2109b/en-US/7e0ddf3d1ef24a42b68cd75fc526302c.html#5f0427eab54d467bb18871ce0d41e862.html)
 -   You have already completed the [initial setup for the Identity Authentication Service](https://help.sap.com/viewer/6d6d63354d1242d185ab4830fc04feb1/Cloud/en-US/31af7da133874e199a7df1d42905241b.html)
 -   You have already configured the [Install Edge Gateway Service and Persistence Service](iot-edge-install-gateway-persistence)

## Details
### You will learn
-   How to create Device Model by using Device Connectivity APIs in Postman collection
-   How to create Thing Model and link it to the Device Model by using APIs in Postman collection
-   How to simulate physical Device Data ingestion
-   How to consume ingested measurements with the SAP IoT APIs

---

[ACCORDION-BEGIN [Step 1:](Get OAuth token and tenant details)]

You can access the SAP IoT APIs using an OAuth Token. After you subscribe your subaccount to SAP IoT, you can retrieve the client secret from the service keys you created for your subaccount.

1.  Retrieve client credentials

     Refer to this document to [retrieve Client ID and Client Secret](https://help.sap.com/viewer/fffd6ca18e374c2e80688dab5c31527f/2009a/en-US/a41c28db0cf449059d48c23fa5f7b24b.html). You can use the client credentials to retrieve the OAuth token. You can access all the APIs and data within the subaccount using this OAuth token.

2.  Set up Postman collection

     Postman is a popular and easy to use REST client which you can use to access the SAP IoT APIs. Please refer to the introduction guide for understanding the usage of Postman.

       We have provided a set of sample Postman collections on GitHub for you to download. Please clone or download/unzip the repository [sap-iot-samples](https://github.com/SAP-samples/sap-iot-samples). The Postman collection and environment (in folder `/tools/Postman collections/SAP IoT Edge - Create Device and Thing`) includes all the API calls used in these tutorials and the payload structures for the same. Import both into your Postman.

     Review and if required update the URL for the following endpoints in the Postman environment:


    |  Postman variable     | Value
    |  :------------- | :-------------
    |  `deviceService`          | You can find it in the service key (use the value of **`iot-device-connectivity`** removing initial **`https://`**) you generated in your subaccount, for example **`iot-device-connectivity-noah-live.cfapps.eu10.hana.ondemand.com`**
    |  `packageApi`           | You can find it in the service key (use the value of **`config-package-sap`** removing initial **`https://`**) you generated in your subaccount, for example **`config-package-sap.cfapps.eu10.hana.ondemand.com`**
    |  `configThingApi`    | You can find it in the service key (use the value of **`config-thing-sap`** removing initial **`https://`**) you generated in your subaccount, for example **`config-thing-sap.cfapps.eu10.hana.ondemand.com`**
    |  `dataMapping`          | You can find it in the service key (use the value of **`tm-data-mapping`** removing initial **`https://`**) you generated in your subaccount, for example **`tm-data-mapping.cfapps.eu10.hana.ondemand.com`**
    |  `appiot`   | You can find it in the service key (use the value of **`appiot-mds`** removing initial **`https://`**) you generated in your subaccount, for example **`appiot-mds.cfapps.eu10.hana.ondemand.com`**
    |  `uaadomain` | You can find it in the service key (use the value of **`uaadomain`** in the **`uaa`** part of the keys, removing initial **`https://`**) you generated in your subaccount, for example **`authentication.eu10.hana.ondemand.com`**


3.  Generate OAuth token

    >You can refer to the [SAP Help Portal](https://help.sap.com/viewer/fffd6ca18e374c2e80688dab5c31527f/latest/en-US/7b3a94e68be9460680a915138a160c67.html) to create an OAuth Token if you would like to understand the steps for generating the token in detail.

     To retrieve the OAuth token, you must do the following:

     -   Select the environment you have imported as active and edit the variable values

     -   Update other Postman environment variables:


    |  Postman variable     | Obtain the value from          | Operation
    |  :------------- | :------------- | :-------------
    |  `identityzone`           | The `identityzone` value in the `uaa` part from your SAP IoT Service Keys | Replace the placeholder **`<specify here the tenant name, it's the uaa identityzone from your SAP IoT Service Keys>`**
    |  `client_id`           | The `clientid` value in the `uaa` part from your SAP IoT Service Keys | Replace the placeholder **`<specify here the uaa clientid from your SAP IoT Service Keys>`**
    |  `client_secret`    | The `clientsecret` value in the `uaa` part from your SAP IoT Service Keys | Replace the placeholder **`<specify here the uaa clientsecret from your SAP IoT Service Keys>`**
    |  `package`          | Any unique alphanumeric value of your choice (i.e.: **`test`**) | Replace the placeholder **`<specify here a name for your package>`**
    |  `gatewayAlternateId`   | The alternate id of your gateway specified in during the installation of the **Edge Gateway Service** in the tutorial [Install Edge Gateway Service and Persistence Service](iot-edge-install-gateway-persistence) | Replace the placeholder **`<specify here a alternate Id of your gateway specified during the deployment>`**
    |  `local-ip` | A network IP address or hostname of your edge node reachable from your Postman client | Replace the placeholder **`<specify here a reachable hostname or ip address of your Edge Node>`**

     -   You can retrieve the `OAuth` token by using the Postman collection (`SAP IoT Edge - Create Device and Thing`) and triggering the request **Step 0: GET OAuth Token**.

     -   The answer provided by the API is the `Oauth` token. You don't need to do anything; in the **Tests** section of this API invocation the `LoginTokenT1` Postman environment variable is set appending 'Bearer ' at the beginning. It will be used as authorization header to invoke all the subsequent SAP IoT APIs.


    >While invoking a subsequent API, you can receive the following message:
     !![tokenexpired](tokenexpired.png)
     It means the token is expired and you have to execute the **Step 0: GET OAuth Token** again to refresh the token.

4.  Get the tenant details and create your application package in SAP IoT

     In order to onboard correctly the Device and the Thing models you need to execute some step and to create a new package in SAP IoT to host your Thing Type and you Thing.

     Note: The following process will set up several Postman environmental variables, keep the order as follows.

     1.  Trigger the request **Step 1.1: Get Object Group** to get the correct object-based access authorization group. See [Object Groups](https://help.sap.com/viewer/fffd6ca18e374c2e80688dab5c31527f/latest/en-US/94c98d6a5fbe4aa0ad80d000667e4755.html) for more details.

     2.  Trigger the request **Step 1.2: Get Tenant** to get the current tenant details.

     3.  Trigger the request **Step 1.3: Post a new package** to create a new package in SAP IoT.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create device and thing model types)]

In order to create a digital twin of a physical asset in the SAP Internet of Things system, the Device entity needs to be onboarded. This is done by creating a Device Model, which includes four entities: Capability, Sensor Type, Device with a Sensor. To complete the creation of the digital twin, the Device Model needs then to be mapped to a created [Thing Type](https://help.sap.com/viewer/e057ad687acc4d2d8f2893609aff248b/2110b/en-US/b08a694fc6da45e8a6b63404517536c8.html). The following steps will guide you through this setup. For more information on the Device Model see [SAP Help Portal on IoT Device Connectivity](https://help.sap.com/viewer/104567de4c2547a4ac32ba9b362e60ec/latest/en-US/2b5ac1ae2bf843d691ed763a96b10712.html)

1.  Trigger the request **Step 2.1 Create Capability for IoT Device**.

2.  Trigger the request **Step 2.2 Create Sensor Type for IoT Device**.

3.  Trigger the request **Step 2.3: Create Property Set Type**.

4.  Trigger the request **Step 2.4: Create Thing Type**.

5.  Trigger the request **Step 2.5: Map Thing Type**.

    >If, for any reason, an existing Thing Type mapping details is lost or removed from the environmental variables, you can always fetch it again by using the request **Step 2.6: Get Sensor Type Mapping - Only if required**.

6.  This step permits you to obtain your specific **Edge Gateway Service** identifier, to be able to subsequently onboard a device connected and ingesting data into your edge node. Trigger the request **Step 3.1: Get gateway Id**.

7.  Trigger the request **Step 3.2: Create Device**

     A device, used in the next step to simulate data ingestion using Postman, is now onboarded and connected to your edge node. To be able to consume any ingested data in the cloud you need to map his Sensor to a Thing in SAP IoT.

8.  Trigger the request **Step 3.3: Onboard Thing**.

9.  Since the `ThingId` is not returned in the previous step, you need to execute a separate API to get it; trigger the request **Step 3.4: Get Thing Id**.

10. Trigger the request **Step 3.5: Map Thing**.

With this step the definition of both the Device Model and the Thing Model have been completed. A correct mapping between **Sensor Type - Thing Type** and **Sensor - Thing** is set to connect correctly your device to a digital thing.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Ingest data)]

Since the Device Model and the Thing Model are now created you can ingest some data at the edge, simulating the created device with the Postman client, and consume it in the cloud with the usage of the SAP IoT APIs.

>If you have installed an **Edge Gateway Service** of protocol **MQTT** you can use any MQTT client simulate the data ingestion instead of Postman, using the same provided sample payload.

1.  Trigger the request **Step 5: Send data and wait 1 min** and wait a while to be sure the data has been processed correctly by the cloud services and stored in the SAP IoT Big Data Storage.

    This is the sample payload provided in the collection; if you have changed the definition of the device and thing model objects, you need to change it accordingly. You can also execute it multiple times to ingest more then one message, maybe changing the sent value of the `temperature`.

    ```JSON
    {
      "capabilityAlternateId": "{{pstTemperature}}",
      "sensorAlternateId": "fridgeThermometer",
      "measures": [{
        "temperature": "23"
      }]
    }
    ```


2.  Trigger the request **Step 6.1: Get `Timeseries` Data for a Thing**. Check the values of the `temperature` are returned from the hot storage in the response for this invocation:

    ```JSON
    {
      "value": [
          {
              "_time": "2021-10-29T10:25:38.297Z",
              "temperature": 23.0
          },
          {
              "_time": "2021-10-29T10:43:20.946Z",
              "temperature": 23.0
          },
          {
              "_time": "2021-10-29T10:25:38.297Z",
              "temperature": 23.0
          },
          {
              "_time": "2021-10-29T10:25:19.410Z",
              "temperature": 24.0
          },
          {
              "_time": "2021-10-29T10:25:13.535Z",
              "temperature": 24.0
          }
      ]
    }
    ```


3.  Trigger the request **Step 6.2: Get Snapshot Data for a Thing**; it will return the last known value of the `temperature`.

    ```JSON
    {
        "_id": "DCC87007E4B148E19906E0A3B810E1C0",
        "_thingType": [
            "iot.xxxxxxx.test:thermometer"
        ],
        "value": [
            {
                "iot.xxxxxxx.test:thermometer": [
                    {
                        "/temperature": [
                            {
                                "temperature": {
                                    "_unitOfMeasure": "",
                                    "description": "temperature",
                                    "_value": 23.0,
                                    "_qualityCode": null,
                                    "_time": "2021-10-29T10:25:38.297Z"
                                }
                            }
                        ],
                        "description": "temperature",
                        "_propertySetType": {
                            "name": "iot.xxxxxxx.test:temperature",
                            "description": "temperature"
                        }
                    }
                ]
            }
        ]
    }

    ```


[VALIDATE_2]
[ACCORDION-END]

---
