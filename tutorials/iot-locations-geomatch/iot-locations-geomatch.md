---
title: Create Geolocations and trigger Geomatch
description: Create geolocations and trigger geomatch for a thing using SAP IoT
auto_validation: true
time: 40
tags: [ tutorial>beginner, topic>cloud, products>sap-leonardo-iot, tutorial>license]
primary_tag: topic>internet-of-things
---

## Prerequisites
 - Basic knowledge of Rest-APIs.
 - Basic knowledge of [Postman](https://www.postman.com/) (Version 7.29.1 and above).
 - Your SAP user from <accounts.sap.com> has been added to the Cloud Foundry space in this tenant as a Space Developer so you can retrieve the required credentials for accessing the APIs.
 - Your user has at a minimum the `iot_role_collection` created during onboarding of your tenant and the associated roles.
 - Have a working thing with continuous data ingestion according to the [Onboard Virtual Devices Sending Data from Your Computer](iot-onboard-device) tutorial.
 - Basic understanding of SAP IoT rules and actions services.


## Details
### You will learn
  - How to define a space of geometry type point
  - How to define a point of interest geolocation
  - How to define radius configurations for geomatch
  - How to trigger a manual geomatch request
  - How to read geomatch associations
  - How to configure rules and actions to trigger geomatch automatically, if relevant

SAP IoT Location services enables you to achieve business outcomes for real-time things -- like vehicles equipped with GPS sensors. It allows you to define geolocations -– point of interest and area of interest, associating a thing to a business location.

This tutorial helps you with the first steps of setting up geolocations, configuring geomatch, associating a thing to a business location via a geomatch request and reading the associations for the thing.

---

[ACCORDION-BEGIN [Step 1: ](Configure destination for geomatch request)]


You can create the destination for the geomatch request API endpoint by going to your Cloud Foundry subaccount at [https://hana.ondemand.com/](https://hana.ondemand.com/), drilling down to **Destinations** (under connectivity on the left panel) and creating / importing a destination.

Provide the 'geomatch-service' API endpoint (Ex:`sap-iot-noah-live-geomatch-service.cfapps.eu10.hana.ondemand.com/api/v1/geomatch/request`) from the service key you generated for your subaccount as the URL in the destination.

You can retrieve the Client ID, Client Secret and the Token Service URL (authentication URL) from the service key and provide the same in the destination.

Also, add 'sap.iot.header.Content-Type' as 'application/json' for the additional properties.

!![geomatch Request Destination](GeomatchRequestDestination.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Retrieve OAuth token)]

In this case, let us assume that the University Medical Center Heidelberg has a vending machine with some food items in it. Since the location of the vending machine and University Medical Center Heidelberg is very close, the vending machine will get assigned to it. You can create the University Medical Centre Heidelberg as a point of interest.

To create a point of interest, a space of geometry type point should exist.

+ **Retrieve client credentials**

You can access the SAP IoT APIs using an OAuth token. After you subscribe your subaccount to SAP IoT, you can retrieve the client secret from the service keys you created for your subaccount. Refer to [Retrieving Client ID and Client Secret](https://help.sap.com/viewer/fffd6ca18e374c2e80688dab5c31527f/2009a/en-US/a41c28db0cf449059d48c23fa5f7b24b.html) document for more details. You can use the client secret to retrieve the OAuth token. The client secret is unique for a subaccount and you can access all APIs and data within the subaccount using this OAuth token. This kind of authentication is used when you build backend applications, where you don't have a user context.

+ **Install Postman**

Postman is a popular and easy to use REST client using which you can access the SAP IoT APIs. Refer to the [introduction guide](https://learning.postman.com/docs/getting-started/introduction/) for understanding the usage of Postman. We have provided a set of sample Postman collections on GitHub for you to download. Please clone or download/unzip the repository **`sap-iot-samples`** at [https://github.com/SAP-samples/sap-iot-samples](https://github.com/SAP-samples/sap-iot-samples). The Postman collection (iot-location-services-samples > Geomatch) includes all the API calls used in this tutorial and the payload structures for the same. However, if you would like to try out the APIs on your own, we have provided the details in this tutorial on the consumption of the APIs.

+ **Generate OAuth token**

Before retrieving the OAuth token, you must update the following in the [Postman environment variables](https://learning.postman.com/docs/sending-requests/managing-environments/)

1. Update the tenant name (your subaccount name - Ex: iot.trainingf954d) to the  `tenant_name` Postman environment variable.
2. Update the `uaadomain` from the service key to `uaadomain` Postman environment variable.
3. Update the Client ID and Client Secret from the service key you generated for your subaccount to `client_id` and `client_secret` Postman environment variable respectively.


You can generate the OAuth token by using the Postman collection and triggering the request **'Step 1: GET OAuth Token'**. You can refer to the below Developer Guide to create an OAuth Token if you would like to understand the steps for generating the token.

[Generate OAuth Token](https://help.sap.com/viewer/fffd6ca18e374c2e80688dab5c31527f/2005b/en-US/7b3a94e68be9460680a915138a160c67.html)

Update the generated token to `LoginToken1` Postman environment variable appending 'Bearer' at the beginning. For accessing the SAP IoT APIs, enter this OAuth token as an Authorization Header in the Postman.

!![OAuth Token in header](OAuthHeader.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Define space of geometry type point)]

A space must exist before creating a geolocation. To be able to create a space, you can use the UIs or APIs. In this case, let us use the APIs to define a space of geometry type **point**. You can refer to the [Create Geolocations and configure Geofence](iot-locations-geofence) tutorial for creating geolocations using the UI.

You can get the API endpoints for the services from the service key that you generated for your subaccount. For creating a space, you must use the 'geolocation' endpoint from the service key. In the  Postman collection (the Geomatch collection you downloaded from the GitHub link), you can execute the **'Step 3.1: Create a point space'** request to create a space of geometry type point. However, if you want to try out, below is the sample API endpoint and payload. You must replace 'sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com' with the 'geolocation' endpoint from the service key you generated for your subaccount in the below URL.

```
https://sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com/geolocation/v1/Spaces
```

```JSON
[{
	"SpaceName": "Healthcare 0001",
	"Geometry": {
		"Type": "Point"
	},
	"Description": "Healthcare Point Space",
	"ParentSpaceId": null
}]
```
In the above example, we have defined the geometry type as **point**. Provide a **unique name** for the space to avoid conflicts (Ex: **Healthcare 0001**)

You can now get the `SpaceId` for the above space by checking the **Locations** key in the **response headers**. You must select the alphanumeric string after '/Spaces' for getting the ID. Update this `SpaceId` to `point_space_id` in the Postman environment variables for future use.

!![Get SpaceID](SpaceIDFetch.png)

Try to read the above space with a GET request to the same endpoint, but with the `SpaceId` as the parameter. You can use the Postman collection (Geomatch collection) and execute **'Step 3.2: Read point space'**

However, if you want to try out, below is the sample API endpoint. You must replace 'sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com' with the 'geolocation' endpoint from the service key you generated for your subaccount in the below URL. Use the `SpaceId` you retrieved above in place of `ee2002d6-57d6-4553-b9b5-eff164yuw3d`.

```
https://sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com/geolocation/v1/Spaces/ee2002d6-57d6-4553-b9b5-eff164yuw3d
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Define business location -- point of interest)]

A business location must exist for associating a thing to a location using the manual geomatch request. A business location is a point of interest geolocation. Before triggering the **'Step 4.1: Create Business Location'** request from the Postman collection (Geomatch collection), you must do the following changes in the payload (Body tab in Postman):

+ **Generate a GUID for the Business Object**

Manual geomatch works for geolocations with `BusinessObjectGUID`. For the `BusinessObjectGuid`, generate a 32 character GUID using the [GUID Generator](https://www.guidgenerator.com/online-guid-generator.aspx). Update the generated GUID to the `BusinessObjectGuid` Postman environment variable.

+ **Update the coordinates**

The format of the coordinates in the payload is as below:

```JavaScript
   "coordinates":[
        longitude,
        latitude
        ]
```

You can get the coordinates for an address using [https://www.gps-coordinates.net/](https://www.gps-coordinates.net/). You must update the latitude and longitude values you are using in the `latitude` and `longitude` Postman environment variables for future use.

+ **Create business location**

You must have updated the point space ID you retrieved in the previous step to the `point_space_id` Postman environment variable. Provide a **unique name** for `GeoLocationName` and `BusinessObjectDisplayName`(Ex: **University Medical Center Heidelberg POI 0001**) to avoid conflicts. You can now trigger the **'Step 4.1: Create Business Location'** request from the Postman collection (Geomatch collection).

However, if you would like to try out, below is the sample API endpoint and payload. You must replace 'sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com' with the 'geolocation' API endpoint from the service key you generated for your subaccount in the below URL and make a POST request.

```
https://sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com/geolocation/v1/GeoLocations
```

```JavaScript
[{
      "Type":"Feature",
      "Properties":{
         "ParentGeoLocationId":null,
         "ExternalId":null,
         "SpaceId":"ee2002d6-57d6-4553-b9b5-eff164yuw3d",
         "GeoLocationName":"University Medical Center Heidelberg POI 0001",
         "BusinessObjectType":"Hospital",
         "BusinessObjectGuid":"b3f24275-c351-4f1e-8389-a94ed431f097",
         "BusinessObjectDisplayName":"University Medical Center Heidelberg 0001",
         "IsGeoFence":false,
         "HasChild":false,
         "Descriptions":[
            {
               "Locale":"en",
               "Label":"University Medical Center Heidelberg"
            }
         ],
         "Addresses":[
            {
               "Locale":"en",
               "Address":{
                  "House":"672",
                  "Street":"Im Neuenheimer Feld",
                  "CityDistrict":null,
                  "City":"Heidelberg",
                  "District":null,
                  "RegionCode":"BW",
                  "Region":"Baden-Wurttemberg",
                  "CountryCode":"DE",
                  "Country":"Germany",
                  "PostalCode":"69120"
               }
            }
         ]
      },
      "Geometry":{
         "type":"Point",
         "coordinates":[
            8.671045303344727,
            49.41686867456883
         ]
      },
      "SensitivityCategory":null
   }]
```

As a user, you can choose to define a geolocation as personal information, personal sensitive information or none. You can define the `SensitivityCategory` as `com.sap.appiot.security:spi` for personal sensitive information, `com.sap.appiot.security:pii` for personal information or `null`. This is with respect to the Global Data Protection Regulation (GDPR).

You can now get the `GeoLocationID` for the above geolocation by checking the **Locations** key in the **response headers**. You must select the alphanumeric string after `/GeoLocations` for getting the ID. Update this `GeoLocationID` to `point_geolocation_id` in the Postman environment variables for future use.

You can also read the above business location. Use the Postman collection (Geomatch collection) and trigger **'Step 4.2: Read Business Location'**. However, if you want to try out the API, below is the sample API endpoint. You must replace 'sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com' with the 'geolocation' API endpoint from the service key you generated for your `subaccount` in the below URL, replace `0bc4d7b4-bef9-4e0f-8f60-354673cc43ea` with the `GeoLocationID` retrieved earlier and trigger a GET request.

```
https://sap-iot-noah-live-geolocation-runtime.cfapps.eu10.hana.ondemand.com/geolocation/v1/GeoLocations/0bc4d7b4-bef9-4e0f-8f60-354673cc43ea
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Configure geomatch radius settings)]

Geomatch is the process of comparing the location of the thing against the location of business objects and assigning the thing to the business location based on certain criteria like the distance between the thing and business location. Geomatch can be used to automatically detect the movement of a thing and accordingly do the assignment/un-assignment with a business location. Further, notifications can be generated to notify the person responsible if there is any unplanned movement of the thing and they can take further actions.

Geomatch functionality is based on the GPS data that a thing is generating. To configure the geomatch service to a business case, the radius values for calculations should be configured conforming to certain constraints. There can be only **One Configuration** per subaccount for geomatch. You can always modify the configurations if required.

To create the geomatch radius settings configuration, use the Postman collection (Geomatch collection) and execute **'Step 5.1: Configure GeoMatch Radius Settings'**. However, if you would like to try out, below is the sample API endpoint and the payload. You must replace 'sap-iot-noah-live-geomatch-service.cfapps.eu10.hana.ondemand.com' with the 'geomatch-service' API endpoint from the service key you generated for your subaccount in the below URL.

```
https://sap-iot-noah-live-geomatch-service.cfapps.eu10.hana.ondemand.com/api/v1/geomatch/Configuration/geomatchRadiusSettings
```

```JavaScript
{
  "InitialRadius": {
    "Radius": 50,
    "Status": "Assigned",
    "CreateBOAssociation": true,
    "TreatMultipleMatchesWithSameStatus": false
  },
  "IntermediateRadius": [
    {
      "Radius": 100,
      "Status": "Nearby"
    },
    {
      "Radius": 200,
      "Status": "In 200m range"
    }
  ],
  "RestAllStatus": "Unassigned",
  "ManualMappedState": "Manually assigned",
  "ManualUnmappedState": "Manually unassigned",
  "AllowedDistanceError": 10
}
```
In the above payload, we have defined the radius configurations for a geomatch request with three statuses for different ranges of radius in meters.

You can use the Postman collection (Geomatch collection that you downloaded from the GitHub link) for retrieving the configurations and triggering **'Step 5.2: Read GeoMatch Radius Settings Configuration'** request. If you want to try out, below is the sample API endpoint. You must replace 'sap-iot-noah-live-geomatch-service.cfapps.eu10.hana.ondemand.com' with the 'geomatch-service' API endpoint from the service key you generated for your subaccount in the below URL.

```
https://sap-iot-noah-live-geomatch-service.cfapps.eu10.hana.ondemand.com/api/v1/geomatch/Configuration/GeoMatchRadiusSettings
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Trigger manual geomatch request)]

Now that the radius configurations are setup for the subaccount, you can trigger a geomatch request.

The geomatch request API is a REST API, which allows you to request one of the supported actions like 'Automatic geomatch', 'Manual geomatch', 'Manual Un-mapping' and 'Clearing of Manual Mappings' to be performed on a single thing. For triggering the geomatch request for a thing, it's location must be known.

+ **Retrieve the `ThingID`**

First, update the thing type name in the Postman environment variable `ThingType` with the thing type (Ex: `iot.trainingf954d.com.sap.silo10:silo_type_1`) you created for your thing model (Refer to [Onboard Virtual Devices Sending Data from Your Computer](iot-onboard-device) tutorial to create a thing model and ingest data).

Next, trigger the **'Step 6.1: GET `ThingID`'** request to get the `ThingID`. However, if you would like to try out, you can use the below endpoint.

```
https://appiot-mds.cfapps.eu10.hana.ondemand.com/Things?$filter=_thingType eq 'iot.trainingf954d.com.sap.silo00:silo_type_1'
```
In the above API, you must replace 'appiot-mds.cfapps.eu10.hana.ondemand.com' with the 'appiot-mds' endpoint from the service key and replace `iot.trainingf954d.com.sap.silo00:silo_type_1` with the thing type for your thing model.

The above API will return all the **things** for the thing type provided in the request. You can select the `ThingID`(`_id` in the response) you want to use. Update the `ThingID`(`_id` in the response) and the `ThingDisplayName`(`_name` in the response) you retrieved here, in the Postman environment variables `thing_id` and `thing_displayName` respectively for future use.

!![GET ThingID](ThingID.png)

+ **Trigger manual geomatch request**

You can trigger the **'Step 6.2: Manual Geomatch Assignment'** for creating a geomatch request. However, if you would like to try out, below is the sample API endpoint and payload. You must replace 'sap-iot-noah-live-geomatch-service.cfapps.eu10.hana.ondemand.com' with the 'geomatch-service' API endpoint from the service key you generated for your subaccount in the below URL. In the below sample payload, use the `ThingID`, `ThingDisplayName` (from the thing details),`BusinessObjectGUID` and `BusinessObjectType` (from the geolocation details) you retrieved above.

```
https://sap-iot-noah-live-geomatch-service.cfapps.eu10.hana.ondemand.com/api/v1/geomatch/request
```

```JavaScript
{
   "RequestType": "MANUAL_ASSIGN",
   "ThingID": "{{thing_id}}",
   "ThingDisplayName": "{{thing_displayName}}",
   "IgnoreStatuses": "",
   "BusinessObjectType": "<Geolocation-BusinessObjectType>",
   "BusinessObjectGUID": "<Geolocation-BusinessObjectGUID>",
   "ThingLatitude": {{thing_latitude}},
   "ThingLongitude": {{thing_longitude}},
   "RequestTime": {{requestutctimestamp}},
   "SensitivityCategory": "NONE"
}
```
Manual geomatch requires that the geolocation is a business location, i.e, it has a `BusinessObjectGUID` for associating a thing to a geolocation. This will be the Business Partner information. The `BusinessObjectType` and `BusinessObjectGUID` are the type and ID of the Business Object to which the thing shall be manually assigned.

For the manual geomatch request, the `ThingLatitude` and `ThingLongitude` can be the same as the business location (point of interest) coordinates values which you created earlier.

As a user, you can choose to define a geomatch request as personal information, personal sensitive information or none. You can define the `SensitivityCategory` as `com.sap.appiot.security:spi` for personal sensitive information, `com.sap.appiot.security:pii` for personal information or `NONE`. This is with respect to the Global Data Protection Regulation (GDPR).

Use the Postman collection (Geomatch collection) and trigger **'Step 6.2: Manual Geomatch Assignment'** request.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Read geomatch association for the thing)]

Now that a geomatch request is triggered for a thing, the status of the thing will be based on the request type, radius configurations and thing's location. The results of the geomatch requests will be persisted as a Geo association data'. The same is exposed as an OData API.

You can trigger the **'Step 7: Latest Geo-Associations for one Thing'** request from the  Postman collection (Geomatch collection). However, if you'd like to try out, below is the sample API Endpoint. You must replace 'sap-iot-noah-live-geomatch-association-odata.cfapps.eu10.hana.ondemand.com' with the 'geoassociation-odata' API endpoint from the service key you generated for your subaccount in the below URL.

```
https://sap-iot-noah-live-geomatch-association-odata.cfapps.eu10.hana.ondemand.com/api/v1/geomatch/odata/LatestgeomatchAssociationBythings?thingId={{thing_id}}
```

In the above API endpoint, the `thing_id` is the `ThingID` you used for triggering the geomatch request. You can read the latest geomatch associations or all the geomatch associations for the thing. The sample response for `LatestGeoMatchAssociationByThings` is as below:

!![Geomatch Association Response](GeoMatchAssociationResponse.png)

The `Matching_Status` of the thing's association depends on the request type, radius configurations and the thing's location.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Define rules for automatic geomatch)]

You can define rules to specify the conditions to generate events which can be consumed in actions to trigger a geomatch request for the things that are ingesting data continuously. To make this cost effective, you can define a **Scheduled rule** to trigger an action in case of any change in the thing's location for a certain time window. A rule context must exist before creating a rule. You could use the UIs to create rule contexts and rules. You must navigate to the **IoT Rules and Actions** tab and then to **Rule Contexts** tile in the Fiori Launchpad to create a rule context and the **Rules** tile to create a rule respectively.

If you know the tenant name (in Cloud Foundry developer lingo called 'subdomain' and in cloud platform lingo called 'Cloud Foundry subaccount') you can also invoke the Fiori Launchpad by replacing `iot-training-f954-d` with your tenant name in the following:

[https://iot-training-f954-d.leonardo-iot.cfapps.eu10.hana.ondemand.com/](https://iot-training-f954-d.leonardo-iot.cfapps.eu10.hana.ondemand.com/)

+ **Create a rule context**

In the rule context, provide a **unique name** for the name and description fields (Ex: **Geomatch rule context 0001**) to avoid conflicts. Add the measured value (EX: `silo_status`) of your thing model (refer to [Onboard Virtual Devices Sending Data from Your Computer](iot-onboard-device) tutorial to create a thing model and ingest data) as the 'Thing Data - Property Sets'. An example is provided below for creating a rule context:

!![Geomatch rule context](GeoMatchRuleContext.png)

+ **Create a rule**

Now that you have a rule context, you can create a rule. Navigate to the **IoT Rules and Actions** tab and then to **Rules** tile in the Fiori launchpad and create a **Scheduled rule**.

1. Provide a **unique name** for the name and description fields (Ex: **Geomatch request rule 0001**).
2. In the **Definition** tab, select the rule context (Ex: **GeoMatch Rule Context 0001**) you created earlier for **Rule Context** field.
3. Provide **4 minutes** for the **Windowing** and **2 minutes** for the **Scheduling**.
4. Sample rule condition is provided below (you have to type the condition as the tool doesn't allow you to copy-paste the condition. You could also define your own condition based on your scenario).

```
( silo_status_aggregate.latitude_LAST != silo_status_aggregate.latitude_FIRST ) OR ( silo_status_aggregate.longitude_LAST != silo_status_aggregate.longitude_FIRST )
```
Once all the details are filled, **Activate** the rule.

The above condition implies to trigger the rule action if there is a change in the thing's location.

The above timings implies that the rule will be executed every two minutes (scheduling) and checks for any change in the thing's location in the past four minutes (windowing). You may define the scheduling and windowing according to your scenario.

!![Geomatch rule information](GeomatchRuleInfo.png)

!![geomatch Request rule](GeomatchRequestRule.png)

!![geomatch Request Time Window](GeomatchRequestTimeWindow.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Define actions for geomatch)]

Before defining an action, you must configure a **Destination** for the geomatch request API Endpoint for your subaccount. Please refer to the **'Step 1: Configure destination for geomatch request'** part of this tutorial.

+ **Create an action to trigger geomatch request**

If the above rule condition is satisfied, the rule will trigger an action. You can define an action to trigger the geomatch request. You can use the UIs to create an action. You must navigate to the **IoT Rules and Actions** tab and then to **Actions** tile in the Fiori launchpad.

1. Once you are in the create screen, provide a **unique name** for the name and description fields (Ex: **Geomatch Request Action 0001**).
2. Select **Event from rule** for **Triggered by** field
3. Select the rule you created (Ex: **Geomatch request rule 0001**) in the previous step for the **Rule** field
4. Select the **Action Type** as **Service Integration** and define the sample payload for the geomatch request. We have provided the sample payload below:

```JavaScript
{         
  "RequestType": "AUTOMATIC",     
  "ThingID": "${thing.id}",     
  "ThingDisplayName": "${thing.description}",     
  "IgnoreStatuses": "",     
  "BusinessObjectType": null,     
  "BusinessObjectGUID": null,     
  "ThingLatitude": ${silo_status.latitude},
  "ThingLongitude": ${silo_status.longitude},
  "SensitivityCategory": "NONE"
}
```
In the above payload, the `RequestType` of the geomatch request will be **AUTOMATIC**. The `ThingLatitude` and `ThingLongitude` values are the coordinates of the thing which comes from the thing model.

Once you have a destination configured, you can use it in the action for triggering a geomatch request. An example of the action is below:

!![Action for geomatch request](GeoMatchRequestAction.png)

+ **Create an action for FLP (Fiori launchpad) notification**

As a follow-up, you can define another action to be triggered with the **Action Type** as **In-App Notification** after the completion of the previous action.

1. Provide a **unique name** for the name and description fields (Ex: **Geomatch request FLP notification 0001**). This will trigger an FLP notification for the user.
2. Provide the email you are logged on with (SAP user) for the **Recipients** to receive notifications.
3. For **Text** field, you can use the below sample text or any other text of your choice.

```
Geomatch was called for thing ${thing.name}
```
 An example of an action is provided below:

!![geomatch Request FLP action](GeomatchRequestFLPAction.png)

Once the geomatch request is triggered by the action, the user will get a Fiori launchpad notification for the same.

!![geomatch Request FLP Notification](GeomatchRequestNotification.png)

You can now retrieve the latest geomatch associations for the thing by using the APIs (like shown above in **'Read geomatch association for the thing'**) or create more actions for the same. A destination (similar to the geomatch request destination) must be configured for the associations API for consuming the API in the actions.

In conclusion, the vending machine will now be assigned to the University Medical Center Heidelberg business location, which we did using manual geomatch. For the other things, geomatch request will be triggered again as the data gets ingested with different locations. Based on the distance between thing and business location, assignment/un-assignment will happen.

[VALIDATE_7]
[ACCORDION-END]

---
