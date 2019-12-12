---
title: Build OData Batch Requests for Reading Data with SAP Cloud SDK's Virtual Data Model
description: Build OData batch requests for reading data with the SAP Cloud SDK's Virtual Data Model in your Address Manager application.
auto_validation: true
time: 20
tags: [ tutorial>advanced, topic>javascript, products>sap-cloud-platform, topic>odata]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites).
 - Have access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap-samples.github.io/cloud-s4-sdk-book/pages/mock-odata.html).
 - Read the [tutorials](group.cloudsdk-js-vdm) of building basic OData queries with the SAP Cloud SDK's Virtual Data Model (VDM).
 - Read the [tutorial](cloudsdk-js-odata-batch-changeset) of building OData batch requests for writing data with the SAP Cloud SDK's VDM.
 - Basic knowledge of OData is recommended, but not required.

## Details
### You will learn
  - How to use the Virtual Data Model to read multiple entities in one request
  - How to trigger a batch request consisting of multiple retrieve requests from an API endpoint exposed by your application

The goal of this tutorial group is to continue implementing the TypeScript/JavaScript web application you built in the [Build an Address Manager with the SAP Cloud SDK's OData Virtual Data Model](group.cloudsdk-js-vdm) tutorials by using the OData Batch request feature of the SAP Cloud SDK.

In this tutorial, you use the `batch retrieve request` for reading business partner addresses and business partner banks via an API endpoint.

---

[ACCORDION-BEGIN [Step 1: ](Add and API endpoint)]


[OPTION BEGIN [TypeScript]]
Create a file called `batch-retrieve-route.ts` and the copy the code below into it:

```JavaScript
import { Request, Response } from 'express';
import {
  batch,
  BusinessPartnerAddress,
  BusinessPartnerBank
} from "@sap/cloud-sdk-vdm-business-partner-service";
import {
  GetByKeyRequestBuilder,
  ODataBatchRequestBuilder,
  ReadResponse
} from "@sap/cloud-sdk-core";

export function batchRetrieveRoute(req: Request, res: Response) {
  toBatchRequestBuilder(req.body).execute({
      url: 'https://my.s4hana.ondemand.com/'
    })
    .then(batchResult => {
      const streetName = (batchResult[0] as ReadResponse).as(BusinessPartnerAddress).map(bpa => bpa.streetName);
      const banks = (batchResult[1] as ReadResponse).as(BusinessPartnerBank).map(bpb => bpb.bankName);
      res.status(200).send([streetName, banks]);
    })
}

function toBatchRequestBuilder(body: any): ODataBatchRequestBuilder {
  return batch(...toGetByKeyRequestBuilder(body.retrieve));
}

function toGetByKeyRequestBuilder(retrieveRequest: any): Array<GetByKeyRequestBuilder<BusinessPartnerAddress>|GetByKeyRequestBuilder<BusinessPartnerBank>> {
  return [getBusinessPartnerAddress(retrieveRequest.businessPartnerId, retrieveRequest.businessPartnerAddressId), getBusinessPartnerBank(retrieveRequest.businessPartnerId, retrieveRequest.businessPartnerBankId)];
}

function getBusinessPartnerAddress(businessPartnerId: string, businessPartnerAddressId: string): GetByKeyRequestBuilder<BusinessPartnerAddress> {
  return BusinessPartnerAddress.requestBuilder()
    .getByKey(businessPartnerId, businessPartnerAddressId)
    .select(
      BusinessPartnerAddress.BUSINESS_PARTNER,
      BusinessPartnerAddress.ADDRESS_ID,
      BusinessPartnerAddress.STREET_NAME
    )
}

function getBusinessPartnerBank(businessPartnerId: string, businessPartnerBankId: string): GetByKeyRequestBuilder<BusinessPartnerBank> {
  return BusinessPartnerBank.requestBuilder()
    .getByKey(businessPartnerId, businessPartnerBankId)
    .select(
      BusinessPartnerBank.BUSINESS_PARTNER,
      BusinessPartnerBank.BANK_IDENTIFICATION,
      BusinessPartnerBank.BANK_NAME
    );
}
```
Replace the `url` with your system info.

The batch request consists of two retrieve requests, being `GET BusinessPartnerAddress` and `GET BusinessPartnerBank`. When handling the batch response, you know they are `ReadResponse` instead of `WriteResponses`. Then `as(Constructable<Entity>)` is called to cast the `ReadResonse` to specific entities (`BusinessPartnerAddress` and `BusinessPartnerBank`), which are reflected by the order of the retrieve requests in the batch. Finally, you are able to use the type safe function to get the `streetName` (as an attribute of the `BusinessPartnerAddress`) and the `bankName` (as an attribute of the `BusinessPartnerBank`).

Open the `application.ts`, add the router definition and the missing import.

```JavaScript
...
import {batchRetrieveRoute} from "./batch-retrieve-route";
...
private routes(): void {
  const router = express.Router();
...
  // please add this line starts
  router.post("/batch-retrieve", batchRetrieveRoute);
  // please add this line ends
  this.app.use("/", router);
}
```

[OPTION END]

[OPTION BEGIN [JavaScript]]
Create a file called `batch-retrieve-route.js` and the copy the code below into it:

```JavaScript
const {
  batch,
  BusinessPartnerAddress,
  BusinessPartnerBank
} = require('@sap/cloud-sdk-vdm-business-partner-service');

export function batchRetrieveRoute(req, res) {
  toBatchRequestBuilder(req.body).execute({
    url: 'https://my.s4hana.ondemand.com/'
  }))
.then(batchResult => {
  const streetName = batchResult[0].as(BusinessPartnerAddress).map(bpa => bpa.streetName);
  const banks = batchResult[1].as(BusinessPartnerBank).map(bpb => bpb.bankName);
  res.status(200).send([streetName, banks]);
});
}

function toBatchRequestBuilder(body) {
  return batch(...toGetByKeyRequestBuilder(body.retrieve));
}

function toGetByKeyRequestBuilder(retrieveRequest) {
  return [getBusinessPartnerAddress(retrieveRequest.businessPartnerId, retrieveRequest.businessPartnerAddressId), getBusinessPartnerBank(retrieveRequest.businessPartnerId, retrieveRequest.businessPartnerBankId)];
}

function getBusinessPartnerAddress(businessPartnerId, businessPartnerAddressId) {
  return BusinessPartnerAddress.requestBuilder()
    .getByKey(businessPartnerId, businessPartnerAddressId)
    .select(BusinessPartnerAddress.BUSINESS_PARTNER, BusinessPartnerAddress.ADDRESS_ID, BusinessPartnerAddress.STREET_NAME);
}

function getBusinessPartnerBank(businessPartnerId, businessPartnerBankId) {
  return BusinessPartnerBank.requestBuilder()
    .getByKey(businessPartnerId, businessPartnerBankId)
    .select(BusinessPartnerBank.BUSINESS_PARTNER, BusinessPartnerBank.BANK_IDENTIFICATION, BusinessPartnerBank.BANK_NAME);
}
```
Replace the `url` with your system info.

Open the `application.js`, add the router definition and the missing import.

```JavaScript
...
const { batchRetrieveRoute } require ('./batch-retrieve-route');
...
private routes() {
  const router = express.Router();
...
  // please add this line starts
  router.post("/batch-retrieve", batchRetrieveRoute);
  // please add this line ends
  this.app.use("/", router);
}
```
[OPTION END]

Start the application locally by executing the command `npm run start:local` or `npm run serve-debug` as debug mode.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test by retrieving addresses and bank information in batch request)]
When the application is up and running, you can make a `POST` request to the `URL`: `localhost:8080/batch-retrieve`.

You should see similar json response like the example below:

```JSON
[
    [
        "Lindenstraße"
    ],
    [
        "Bank 3 - SAMPLE BANK"
    ]
]
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]
---
