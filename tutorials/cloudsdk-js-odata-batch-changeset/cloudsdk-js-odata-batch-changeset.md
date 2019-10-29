---
title: Build OData Batch Requests for Writing Data with the SAP Cloud SDK's Virtual Data Model
description: Build OData Batch Requests for writing data with the SAP Cloud SDK's Virtual Data Model in your Address Manager application.
auto_validation: true
time: 20
tags: [ tutorial>advanced, topic>javascript, products>sap-cloud-platform, topic>odata]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites).
 - Have access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html).
 - Read the [tutorials](https://developers.sap.com/group.cloudsdk-js-vdm.html) of building basic OData queries with the SAP Cloud SDK's Virtual Data Model.
 - Basic knowledge of OData is recommended, but not required.

## Details
### You will learn
  - How to use the Virtual Data Model to create multiple entities in one request
  - How to trigger a batch request consisting of multiple change sets from an API endpoint exposed by your application

The goal of this tutorial group is to continue implementing the TypeScript/JavaScript web application you built from a previous tutorial with the OData Batch request feature of the SAP Cloud SDK.

In this tutorial, we use the `batch changeset` for creating multiple business partner addresses via an API endpoint.

---

[ACCORDION-BEGIN [Step 1: ](What is OData batch?)]
In the previous [tutorials](https://developers.sap.com/group.cloudsdk-js-vdm.html), we learned how to leverage the SAP Cloud SDK to build queries for [basic OData operations](https://www.odata.org/getting-started/basic-tutorial/). The basic OData operations allow you to

- query entities (e.g., `BusinessPartnerAddress`) with the same type
- create an entity
- update an entity
- delete an entity

as one operation in one HTTP request.

However, imagine that you may want to update 100 entities (e.g., business partner address). With the basic OData operations, you have to send 100 HTTP requests, which is slow and not atomic. Essentially, the OData protocol also supports batch processing, which is so-called `OData batch request`. With the power of the batch processing, you can avoid network overhead \( _1_ request V.S. _N_ requests \) and benefit from its atomic behavior.


Changeset consists of a collection of write operations (`create`, `update` or `delete`) and behaves like a database transaction. This means, that either all operations are executed successfully, or none of them. We will focus on the `batch changesets` in this tutorial.
`Retrieve requests`, i.e., the read operation, is also supported and will be introduced in the next tutorial.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Prepare your application.)]
We assume you have read the [tutorials](https://developers.sap.com/group.cloudsdk-js-vdm.html) as one of the prerequisites, so you should have a running application with a connection to an SAP S/4HANA Cloud system by using the SAP Cloud SDK's Virtual Data Model.

Now open the `package.json` and update the dependency versions like below, since the `OData batch request` feature is released in the version `1.11.2`.

```JSON
"dependencies": {
  "@sap/cloud-sdk-core": "^1.11.2",
  "@sap/cloud-sdk-vdm-business-partner-service": "^1.11.2",
  ...
}
```

Then run `npm install` in your root folder to install/update the dependencies.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Add an API endpoint)]
[OPTION BEGIN [TypeScript]]

Create a file with the name `batch-changeset-route.ts` in the root folder and then copy the code below to the file:
```JavaScript
import { Request, Response } from 'express';
import {
  batch,
  BusinessPartnerAddress,
  changeset
} from "@sap/cloud-sdk-vdm-business-partner-service";
import { BatchResponse, WriteResponses } from "@sap/cloud-sdk-core";

export function batchChangesetRoute(req: Request, res: Response) {
  const addresses = req.body.addresses as any[];
  batch(
      changeset(
        ...addresses.map(address =>
          BusinessPartnerAddress.requestBuilder().create(
            BusinessPartnerAddress.builder().fromJson(address)
          )
        )
      )
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    })
    .then(batchResult => {
      const success = 'success';
      res.status(200).send(success);
    })
    .catch(error => {
      res.status(500).send(error.message);
    });
}
```
Replace the `url` with your system info.

Then open the `application.ts`, add the router definition and the missing import.

```JavaScript
...
import { batchChangeSetRoute } from "./batch-changeset-route";
...
private routes(): void {
  const router = express.Router();
...
  // please add this line starts
  router.post("/batch-changeset/create", batchChangesetRoute);
  // please add this line ends
  this.app.use("/", router);
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]

Create a file with the name `batch-changeset-route.js` in the root folder and then copy the code below to the file:
```JavaScript
const { batch, BusinessPartnerAddress, changeset } = require('@sap/cloud-sdk-vdm-business-partner-service');

export function batchChangesetRoute(req, res) {
  const addresses = req.body.addresses;
  batch(
      changeset(
        ...addresses.map(address =>
          BusinessPartnerAddress.requestBuilder().create(BusinessPartnerAddress.builder().fromJson(address))
        )
      )
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    })
    .then(batchResult => {
      const success = 'success';
      res.status(200).send(success);
    })
    .catch(error => {
      res.status(500).send(error.message);
    });
}
```
Replace the `url` with your system info.

Then open the `application.js`, add the router definition and the missing import.

```JavaScript
...
const { batchChangeSetRoute } require ('./batch-changeset-route');
...
private routes() {
  const router = express.Router();
...
  // please add this line starts
  router.post("/batch-changeset/create", batchChangesetRoute);
  // please add this line ends
  this.app.use("/", router);
}
```
[OPTION END]

Please note, we use `post` as the method of the `batch-changeset` endpoint, aligned with the OData Batch. Now you can start the application locally by executing the command `npm run start:local` or `npm run serve-debug` as debug mode.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test: create business partner addresses in a batch request)]
After starting your application you can make a request with `post` as the `method`, `localhost:8080/batch-changeset` as the `url` and the json `body`:
```JSON
{
	"addresses" :[
		{
			"businessPartner": "1000000","country": "DE","postalCode": "14469","cityName": "potsdam","streetName": "Konrad-Zuse-Ring","houseNumber": "10"
		},
		{
			"businessPartner": "1000000","country": "DE","postalCode": "14469","cityName": "potsdam","streetName": "Konrad-Zuse-Ring","houseNumber": "11"
		}
	]
}
```
Make sure you use the proper business partner id in your cloud system.

You should see the `success` as the response body, meaning that two business partner addresses are created.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Build entities from response)]


[OPTION BEGIN [TypeScript]]
Now I'll show you how to build type-safe `BusinessPartnerAddress` entities by using the SDK.

Copy the function below to your `batch-changeset-route.ts` file.

```JavaScript
function convertBatchResponse(batchResponse: BatchResponse): BusinessPartnerAddress[]{
  return (batchResponse as WriteResponses).responses.map(r=> r.as!(r.type!) as BusinessPartnerAddress);
}
```

First of all, based on the changeset in the batch requests we sent, we know the batch responses are `WriteResponses` instead of `ReadResponse`. Secondly, by calling `as(Constructable<Entity>)`, the responses are cast to `Entity`. Then, the `Entity` is eventually cast to `BusinessPartnerAddress` by using `as` keyword. This is essential, because during compile time, we don't know the type of the entity. Only you as the caller can provide the entity type based on the order of the changesets.  

Now you can comment out the old code and replace it by using the one below in the `batch-changeset-route.ts` file:
```JavaScript
...
        .then(batchResult =>{
          // const success = 'success';
          // res.status(200).send(success);
          const entities = batchResult.map(convertBatchResponse);
          res.status(200).send(entities);
        })
...
```

[OPTION END]

[OPTION BEGIN [JavaScript]]
Now I'll show you how to build `BusinessPartnerAddress` entities by using the SDK.

Copy the function below to your `batch-changeset-route.js` file.

```JavaScript
function convertBatchResponse(batchResponse) {
  return batchResponse.responses;
}
```

Now you can comment out the old code and replace it by using the one below in the `batch-changeset-route.js` file:
```JavaScript
...
        .then(batchResult =>{
          // const success = 'success';
          // res.status(200).send(success);
          const entities = batchResult.map(convertBatchResponse);
          res.status(200).send(entities);
        })
...
```
[OPTION END]

Restart your server and call the request example again in the Step 4.

You create 2 business partner addresses as shown in the response example:
```JSON
[
    [
        {
            "businessPartner": "1000000",
            "addressId": "40160",
            "addressUuid": "fa163e1c-91b4-1ed9-bec5-2b469753c6e6",
            ...
            "cityName": "potsdam",
            "country": "DE",
            "houseNumber": "10",
            "postalCode": "14469",
            "streetName": "Konrad-Zuse-Ring",
            ...
        },
        {
            "businessPartner": "1000000",
            "addressId": "40160",
            "addressUuid": "fa163e1c-91b4-1ed9-bec5-2b469753c6e6",
            ...
            "cityName": "potsdam",
            "country": "DE",
            "houseNumber": "11",
            "postalCode": "14469",
            "streetName": "Konrad-Zuse-Ring",
            ...
        }
    ]
]
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Wrap up)]
In this tutorial, learned how to make a OData Batch request with the SAP Cloud SDK's Virtual Data Model for writing data to your cloud system and you built your own application that can send multiple changesets in one batch request.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]
---
