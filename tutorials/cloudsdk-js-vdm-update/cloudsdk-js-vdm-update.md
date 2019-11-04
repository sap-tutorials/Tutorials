---
title: Update OData Entities with the SAP Cloud SDK's Virtual Data Model
description: Update OData entities with the SAP Cloud SDK's virtual data model to build an address manager application.
auto_validation: true
time: 15
tags: [ tutorial>intermediate, products>sap-cloud-platform, topic>javascript, topic>odata]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites)
 - Access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html)
 - Basic knowledge of OData is recommended, but not required

## Details
### You will learn
  - How to use the Virtual Data Model to update an existing entity
  - How to trigger an update request from an API endpoint exposed by your application

The goal of this tutorial group is to show you how to implement a JavaScript application that allows you to manage the addresses of business partners. This application will be using `Express.js` and the SAP Cloud SDK for JavaScript. In this tutorial, we use the SAP Cloud SDK's OData Virtual Data Model to update an existing address and make this functionality available via an API endpoint.

---

[ACCORDION-BEGIN [Step 1: ](Add an API endpoint)]

[OPTION BEGIN [TypeScript]]
Create a file called `update-business-partner-address-route.ts` in the `src` folder of your application. Then, copy the following code into it:

```JavaScript / TypeScript
import { Request, Response } from 'express';
import { BusinessPartnerAddress } from '@sap/cloud-sdk-vdm-business-partner-service';

export function updateBusinessPartnerAddressRoute(req: Request, res: Response) {
  updateBusinessPartnerAddress(buildAddress(req.body, req.params.id, req.params.addressId))
    .then(businessPartner => {
      res.status(200).send(businessPartner);
    })
    .catch(error => {
      res.status(500).send(error.message);
    });
}

function updateBusinessPartnerAddress(address: BusinessPartnerAddress): Promise<BusinessPartnerAddress> {
  return Promise.resolve(BusinessPartnerAddress.builder().build());
}

function buildAddress(body: any, businessPartnerId: string, addressId: string): BusinessPartnerAddress {
  const address = BusinessPartnerAddress.builder().fromJson(body) as BusinessPartnerAddress;
  address.businessPartner = businessPartnerId;
  address.addressId = addressId;
  return address;
}
```

This follows the implementation in the previous tutorials. `updateBusinessPartnerAddress` does not do anything useful yet, but you will implement it in the next step. Now open `application.ts`, import the function and add the following route definition:

```JavaScript / TypeScript
import { businessPartnerRoute } from './business-partner-route';
import { singleBusinessPartnerRoute } from './single-business-partner-route';
import { createBusinessPartnerAddressRoute } from './create-business-partner-address-route';
import { updateBusinessPartnerAddressRoute } from './update-business-partner-address-route';

// ...

private routes(): void {
  const router = express.Router();

  router.get("/", indexRoute);
  router.get("/hello", helloWorld);
  router.get("/business-partners", businessPartnerRoute);
  router.get("/business-partners/:id", singleBusinessPartnerRoute);
  router.post("/business-partners/:id/address", createBusinessPartnerAddressRoute);
  // add the following line
  router.put("/business-partners/:id/address/:addressId", updateBusinessPartnerAddressRoute)
  this.app.use("/", router);
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
Create a file called `update-business-partner-address-route.js` in the `src` folder of your application. Then, copy the following code into it:

```JavaScript
const { BusinessPartnerAddress } = require('@sap/cloud-sdk-vdm-business-partner-service');

function updateBusinessPartnerAddressRoute(req, res) {
  updateBusinessPartnerAddress(buildAddress(req.body, req.params.id, req.params.addressId))
    .then(businessPartner => {
      res.status(200).send(businessPartner);
    })
    .catch(error => {
      res.status(500).send(error.message);
    });
}

module.exports.updateBusinessPartnerAddressRoute = updateBusinessPartnerAddressRoute;

function updateBusinessPartnerAddress(address) {
  return Promise.resolve(BusinessPartnerAddress.builder().build());
}

function buildAddress(body, businessPartnerId, addressId) {
  const address = BusinessPartnerAddress.builder().fromJson(body);
  address.businessPartner = businessPartnerId;
  address.addressId = addressId;
  return address;
}
```

This follows the implementation in the previous tutorials. `updateBusinessPartnerAddress` does not do anything useful yet, but you will implement it in the next step. Now open `application.js`, import the function and add the following route definition:

```JavaScript
const { businessPartnerRoute } = require('./business-partner-route');
const { singleBusinessPartnerRoute } = require('./single-business-partner-route');
const { createBusinessPartnerAddressRoute } = require('./create-business-partner-address-route');
const { updateBusinessPartnerAddressRoute } = require('./update-business-partner-address-route');

// ...

private routes() {
  const router = express.Router();

  router.get("/", indexRoute);
  router.get("/hello", helloWorld);
  router.get("/business-partners", businessPartnerRoute);
  router.get("/business-partners/:id", singleBusinessPartnerRoute);
  router.post("/business-partners/:id/address", createBusinessPartnerAddressRoute);
  // add the following line
  router.put("/business-partners/:id/address/:addressId", updateBusinessPartnerAddressRoute)
  this.app.use("/", router);
}
```
[OPTION END]

Note, that we used `router.put` for this route, so we need to send a `PUT` request. Restart your server and send a `PUT` request to `http://localhost:8080/business-partners/1/address/2`. Since the update request isn't implemented yet, the server responds with an empty address.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Update a business partner address)]

[OPTION BEGIN [TypeScript]]
Next, we use the VDM to update an existing business partner address. Open `update-business-partner-address-route.ts` and overwrite the `updateBusinessPartnerAddress` function as shown below:

```JavaScript / TypeScript
function updateBusinessPartnerAddress(address: BusinessPartnerAddress): Promise<BusinessPartnerAddress> {
  return BusinessPartnerAddress.requestBuilder()
    .update(address)
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
Next, we use the VDM to update an existing business partner address. Open `update-business-partner-address-route.js` and overwrite the `updateBusinessPartnerAddress` function as shown below:

```JavaScript
function updateBusinessPartnerAddress(address) {
  return BusinessPartnerAddress.requestBuilder()
    .update(address)
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

The `update` function takes as parameter the entity that should be updated. When creating a new entity, the service will automatically generate things like key fields, the creation date, etc. and return it. The VDM makes this available to you by returning a `Promise<BusinessPartnerAddress>`.

Restart your server and send a `PUT` request to `http://localhost:8080/business-partners/1/address/1` with the following body (or replace the IDs with the ones you used in the previous tutorial):

```JSON
{
	"cityName": "Berlin"
}
```

This will update the business partner address and set its `CityName` to `"Berlin"`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Optional: Concurrency and version identifiers)]

In the example presented here, we don't do anything with the `BusinessPartnerAddress` returned by the update request. In a real-world application, it is recommend to store the entity somewhere. Whenever an entity is received from a service, the VDM will keep track of this state (we call it remote state). If you change the entity locally and then send an update request, the VDM will only send the difference between the current local state and the last known remote state.

You might now ask: "But what if the remote state changes in the meantime?" To protect from this, OData uses so-called version identifiers (in the form of an [entity tag or `eTag`](https://en.wikipedia.org/wiki/HTTP_ETag)). Every entity will be assigned a version identifier for a given state by the service. If the state of the entity changes, the version identifier changes as well. When a request to update or delete an entity is sent to the service, it can compare the version identifier sent with the request to the one it has currently assigned to that entity. If they match, the request will be handled. Otherwise it will be rejected, which prevents clients from making changes to an entity based on an outdated state. This concept is also known as "optimistic concurrency".

The version identifier of an entity is sent via the [`If-Match` HTTP header](http://www.rfc-editor.org/rfc/rfc2616.txt) (section 14.24 "If-Match"). Instead of sending a version identifier, it is also possible so send `*`, meaning that any version identifier should be matched. This is semantically equivalent to forcing a request to be handled, ignoring differences between the remote state and the local state.

For update requests, the VDM will handle version identifiers for you. Whenever an entity is received from a service, the VDM will store the version identifier and automatically send it on subsequent requests. Should you need to force a request to be handled, you can ignore the version identifier like this:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

// pre-existing businessPartner

BusinessPartner.requestBuilder()
  .update(businessPartner)
  .ignoreVersionIdentifier()
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

// pre-existing businessPartner

BusinessPartner.requestBuilder()
  .update(businessPartner)
  .ignoreVersionIdentifier()
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
```
[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]
---
