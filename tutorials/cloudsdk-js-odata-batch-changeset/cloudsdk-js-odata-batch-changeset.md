---
title: Send Multiple OData Requests in batch mode with SAP Cloud SDK's Virtual Data Model
description: Build and execute OData batch requests for writing and retrieving data with the SAP Cloud SDK's Virtual Data Model in your Address Manager application.
auto_validation: true
time: 20
tags: [tutorial>advanced, topic>javascript, products>sap-cloud-platform, topic>odata]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites).
 - Have access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap-samples.github.io/cloud-s4-sdk-book/pages/mock-odata.html).
 - Learn how to use the SAP Cloud SDK's Virtual Data Model by going through [these tutorials](https://developers.sap.com/group.cloudsdk-js-vdm.html).
 - Basic knowledge of OData is recommended, but not required.

## Details
### You will learn
  - How to combine multiple write requests like requests to create, update and delete into changesets
  - How to combine read requests and changesets into batch requests and execute them
  - How to handle the response of a batch request using the SAP Cloud SDK and it's Virtual Data Model

The goal of this tutorial is to create and execute OData Batch requests using the SAP Cloud SDK. It is recommended to go through the [previous tutorials on the SAP Cloud SDK's Virtual Data Model](group.cloudsdk-js-vdm) as you will extend the TypeScript / JavaScript address manager application that was built there.

---

[ACCORDION-BEGIN [Step 1: ](What is an OData batch request?)]
OData supports some basic operations to query, create, update and delete entities. Those operations are mapped to the `GET`, `POST`, `PUT`, `PATCH` and `DELETE` HTTP methods. OData batch requests combine multiple of those operations into one `POST` operation, allowing you to execute multiple requests with just one network call. This can significantly reduce the network overhead you have to deal with, when you want to execute a large number of requests.

An OData batch request can consist of a number of "retrieve requests" and "changesets", all of which will be executed in order. A retrieve request is any HTTP `GET` request - in terms of the SAP Cloud SDK this includes all requests built by a `GetAllRequestBuilder` and `GetByKeyRequestBuilder`.

A changeset is a collection of HTTP `POST`, `PUT`, `PATCH` and `DELETE` operations - requests built by any `CreateRequestBuilder`, `UpdateRequestBuilder` and `DeleteRequestBuilder` in terms of the SAP Cloud SDK. The order of execution within a changeset is not defined as opposed to the whole batch request itself. Therefore the requests within a changeset should not depend on each other. If the execution of any of the requests within a changeset fails, the whole changeset will be reflected as an error in the response and will not be applied, much like a database transaction.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Prepare your application)]
You will extend your `update-business-partner-address-route` and create an additional route that allows to update a list of addresses independently of business partners and returns the complete addresses after they were updated. Make sure that you have the address manager application in place, that supports address deletion as described in [this tutorial](https://developers.sap.com/tutorials/cloudsdk-js-vdm-update.html).

[OPTION BEGIN [TypeScript]]
Your `update-business-partner-address-route.ts` should roughly look as follows, while the given URL refers to your SAP S/4HANA or mock server:

```JavaScript / TypeScript
import { BusinessPartnerAddress } from '@sap/cloud-sdk-vdm-business-partner-service';
import { Request, Response } from 'express';

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
  return BusinessPartnerAddress.requestBuilder()
    .update(address)
    .execute({
      url: 'https://my.s4hana.ondemand.com/',
    });
}

function buildAddress(body: any, businessPartnerId: string, addressId: string): BusinessPartnerAddress {
  const address = BusinessPartnerAddress.builder().fromJson(body);
  address.businessPartner = businessPartnerId;
  address.addressId = addressId;
  return address;
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
Your `update-business-partner-address-route.js` should roughly look as follows, while the given URL refers to your SAP S/4HANA or mock server:

```JavaScript
const { BusinessPartnerAddress } = require('@sap/cloud-sdk-vdm-business-partner-service');
const { Request, Response } = require('express');

export function updateBusinessPartnerAddressRoute(req, res) {
  updateBusinessPartnerAddress(buildAddress(req.body, req.params.id, req.params.addressId))
    .then(businessPartner => {
      res.status(200).send(businessPartner);
    })
    .catch(error => {
      res.status(500).send(error.message);
    });
}

function updateBusinessPartnerAddress(address) {
  return BusinessPartnerAddress.requestBuilder()
    .update(address)
    .execute({
      url: 'https://my.s4hana.ondemand.com/',
    });
}

function buildAddress(body, businessPartnerId, addressId) {
  const address = BusinessPartnerAddress.builder().fromJson(body);
  address.businessPartner = businessPartnerId;
  address.addressId = addressId;
  return address;
}
```
[OPTION END]

In case you have worked on the previous tutorials before the release of version `1.11.2` of the SAP Cloud SDK make sure to update your dependencies to the latest version (any version >= `1.11.2`). Just install the used libraries again:

```Shell
npm install @sap/cloud-sdk-core @sap/cloud-sdk-vdm-business-partner-service
```
[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add an API endpoint for creating entities)]
[OPTION BEGIN [TypeScript]]
Create an additional route function `updateAndReadAddressListRoute` in your `update-business-partner-address-route.ts` that does nothing for now.

```JavaScript / TypeScript
export function updateAndReadAddressListRoute(req: Request, res: Response) {
  res.status(200).send('Not yet implemented.');
}
```

Add that route in the `application.ts`:
```JavaScript / TypeScript
// import updateAndReadAddressListRoute
import { updateBusinessPartnerAddressRoute, updateAndReadAddressListRoute } from './update-business-partner-address-route';

// ...

private routes(): void {
  // ...

  router.delete('/business-partners/:id/address/:addressId', deleteBusinessPartnerAddressRoute);
  // add the route
  router.post('/business-partners/:id/update-address-list', updateAndReadAddressListRoute);
  this.app.use('/', router);
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
Create and additional route function `updateAndReadAddressListRoute` in your `update-business-partner-address-route.js` that does nothing for now.

```JavaScript
export function updateAndReadAddressListRoute(req, res) {
  res.status(200).send('Not yet implemented.');
}
```

Add that route in the `application.js`:
```JavaScript
// require updateAndReadAddressListRoute
const { updateBusinessPartnerAddressRoute, updateAndReadAddressListRoute } = require('./update-business-partner-address-route');

// ...

private routes() {
  // ...

  router.delete('/business-partners/:id/address/:addressId', deleteBusinessPartnerAddressRoute);
  // add the route
  router.post('/business-partners/:id/update-address-list', updateAndReadAddressListRoute);
  this.app.use('/', router);
}
```
[OPTION END]

Note, that the route we added is registered to an HTTP `POST` operation, as internally we will execute both `PATCH` and `GET` operations.

Now, let's implement a batch request.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a batch request with a changeset)]
[OPTION BEGIN [TypeScript]]
In your `update-business-partner-address-route.ts` add another function `updateAndReadAddressList` that takes a list of addresses as input and returns `Promise<BusinessPartnerAddress[]>`. Create an `updateRequest` for each of the addresses. For now, let's just return an empty list.

```JavaScript / TypeScript
async function updateAndReadAddressList(addressList: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  const updateRequests = addressList.map(address =>
    BusinessPartnerAddress
      .requestBuilder()
      .update(address)
  );

  return [];
}
```

Wrap these requests in a `changeset` and pass the `chageset` to the `batch` function of the `@sap/cloud-sdk-vdm-business-partner-service`. This creates a batch request that can be executed the same way all other requests provided by the SAP Cloud SDK can be executed, by calling the `execute` function and passing your destination - a URL in this example:

```JavaScript / TypeScript
// import the batch and changeset functions
import { batch, BusinessPartnerAddress, changeset } from '@sap/cloud-sdk-vdm-business-partner-service';

// ...

async function updateAndReadAddressList(addressList: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  // ...

  batch(
    changeset(...updateRequests)
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  return [];
}
```

> The syntax used to create the changeset is called [spread syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax). It allows to pass the `updateRequests` array as single parameters, equivalent to the older syntax: `changeset.apply(null, updateRequests)`.

The expected input for your `update-address-list` route is a list of addresses in JSON format. Each of those must provide the key fields `businessPartner` and `addressId` otherwise you won't be able to find the correct address to update. Parse this input and call your `updateAndReadAddressList` function:

```JavaScript / TypeScript
export function updateAndReadAddressListRoute(req: Request, res: Response) {
  const addressDiffs = req.body.addressList.map((addressPayload: any) =>
    buildAddress(addressPayload, addressPayload.businessPartner, addressPayload.addressId));

  updateAndReadAddressList(addressDiffs)
    .then((addressList) => {
      res.status(200).send(addressList);
    })
    .catch((error) => {
      res.status(500).send(error.message);
    });
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
In your `update-business-partner-address-route.js` add another function `updateAndReadAddressList` that takes a list of addresses as input and returns a promise with a list of addresses. Create an `updateRequest` for each of the addresses. For now, let's just return an empty list.

```JavaScript
async function updateAndReadAddressList(addressList){
  const updateRequests = addressList.map(address =>
    BusinessPartnerAddress
      .requestBuilder()
      .update(address)
  );

  return [];
}
```

Wrap these requests in a `changeset` and pass the `chageset` to the `batch` function of the `@sap/cloud-sdk-vdm-business-partner-service`. This creates a batch request that can be executed the same way all other requests provided by the SAP Cloud SDK can be executed, by calling the `execute` function and passing your destination - a URL in this example:

```JavaScript
// require the batch and changeset functions
const { batch, changeset } = require('@sap/cloud-sdk-vdm-business-partner-service');

// ...

async function updateAndReadAddressList(addressList) {
  // ...

  batch(
    changeset(...updateRequests)
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  return [];
}
```

> The syntax used to create the changeset is called [spread syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Spread_syntax). It allows to pass the `updateRequests` array as single parameters, equivalent to the older syntax: `changeset.apply(null, updateRequests)`.

The expected input for your `update-address-list` route is a list of addresses in JSON format. Each of those must provide the key fields `businessPartner` and `addressId` otherwise you won't be able to find the correct address to update. Parse this input and call your `updateAndReadAddressList` function:

```JavaScript
export function updateAndReadAddressListRoute(req, res) {
  const addressDiffs = req.body.addressList.map(addressPayload =>
    buildAddress(addressPayload, addressPayload.businessPartner, addressPayload.addressId));

  updateAndReadAddressList(addressDiffs)
    .then((addressList) => {
      res.status(200).send(addressList);
    })
    .catch((error) => {
      res.status(500).send(error.message);
    });
}
```
[OPTION END]

Congratulations! You created and executed a batch request using the SAP Cloud SDK.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Optional: Convert the changeset response to a list of entities)]
An SAP Cloud SDK batch request will return a list of `BatchResponse`s. It is the developer's responsibility to parse those responses and convert them to the expected entities.

Assign the batch response to a local variable. We only executed one changeset, therefore the the response for the changeset will be the first element in the response.

[OPTION BEGIN [TypeScript]]
Use a type assertion (`as`) to inform the compiler that you are handling a response of type `WriteResponses`. This response provides all responses of the requests within the changeset as `responses`. Transform those to instances of `BusinessPartnerAddress` using the `.as` function of each `WriteResponse`. As a `WriteResponse` can belong to either create, update or delete requests, it is possible that there is no response to transform. Therefore you have to make sure to call `as!` with an exclamation mark, to inform the compiler, that we know that the write request we put into our changeset responds with data.

Your code should look like this:
```JavaScript / TypeScript
// add this import
import { WriteResponses } from '@sap/cloud-sdk-core';
// ...

async function updateAndReadAddressList(addressList: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  // ...

  // assign the response to a local variable
  const [updateChangesetResponse] = await batch(
    changeset(...updateRequests)
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  // convert the response
  return (updateChangesetResponse as WriteResponses).responses.map(response => response.as!(BusinessPartnerAddress));
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
This response provides all responses of the requests within the changeset as `responses`. Transform those to instances of `BusinessPartnerAddress` using the `.as` function of each `WriteResponse`.

Your code should look like this:
```JavaScript
async function updateAndReadAddressList(addressList) {
  // ...

  // assign the response to a local variable
  const [updateChangesetResponse] = await batch(
    changeset(...updateRequests)
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  // convert the response
  return (updateChangesetResponse).responses.map(response => response.as(BusinessPartnerAddress));
}
```
[OPTION END]

> The syntax used to assign the response to a variable is called [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#Array_destructuring). It allows retrieve the response from an array without accessing it by its index. It is equivalent to the older syntax: `const updateChangesetResponse = await batch(...)[0];`.

The update request of the SAP Cloud SDK only responds with the local diff that was sent to update the entity. In order to get the complete remote state of the entity, you will have to retrieve each entity from your destination as described in the following.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Retrieve an address list in batch mode)]

In order to retrieve the complete updated addresses, you will execute a number of requests using the `GetByKeyRequestBuilder` of `BusinessPartnerAddress`. Create a list of requests based on the given `addressList` using the key properties `businessPartner` and `addressId` for each address:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
async function updateAndReadAddressList(addressList: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  // ...

  const retrieveRequests = addressList.map(address =>
    BusinessPartnerAddress
      .requestBuilder()
      .getByKey(address.businessPartner, address.addressId)
  );

  // ...
}
```
Add the retrieve requests to the batch request:

```JavaScript / TypeScript
async function updateAndReadAddressList(addressList: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  // ...

  batch(
    changeset(...updateRequests),
    ...retrieveRequests
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  // ...
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
```JavaScript
async function updateAndReadAddressList(addressList) {
  // ...

  const retrieveRequests = addressList.map(address =>
    BusinessPartnerAddress
      .requestBuilder()
      .getByKey(address.businessPartner, address.addressId)
  );

  // ...
}
```

Add the retrieve requests to the batch request:

```JavaScript / TypeScript
async function updateAndReadAddressList(addressList) {
  // ...

  batch(
    changeset(...updateRequests),
    ...retrieveRequests
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  // ...
}
```
[OPTION END]

Congratulations! You created a batch request, that executes a number of updates and retrieve. Note, that the change set will be executed first and the retrieve requests will be executed sequentially afterwards. The order of execution within the changeset is not defined.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Convert the retrieve response to a list of entities)]
Currently you are either returning an empty list as response or, if you followed the optional step 5, a list of partial addresses, containing only the changes sent to the remote destination. In the previous step you executed a batch request, that contains a changeset and retrieves all the data necessary to provide complete addresses. A batch request with the SAP Cloud SDK responds with a list of `BatchResponse`. The first element in this list will be the response to the changeset. The rest will be the responses for your retrieve requests. Assign those to a local variable and convert each of the `ReadResonse`s to a `BusinessPartnerAddress`.

[OPTION BEGIN [TypeScript]]
For each response use a type assertion (`as`) to inform the compiler that you are handling a response of type `ReadResponse`. `ReadResonse`s can either be responses to a request built by a `GetAllRequestBuilder` or a `GetByKeyRequestBuilder`, therefore you will have to transform your response to a list of addresses and return the first element in this list using the `as` method. Return the converted entities:

```JavaScript / TypeScript
import { ReadResponse } from '@sap/cloud-sdk-core';
// ...

async function updateAndReadAddressList(addressList: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  // ...
  const [updateChangesetResponse, ...retrieveResponses] = await batch(
    changeset(...updateRequests),
    ...retrieveRequests
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  return retrieveResponses.map(response =>
    (response as ReadResponse).as(BusinessPartnerAddress)[0]
  );
}  
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
All responses for retrieve requests are `ReadResonse`s. `ReadResonse`s can either be responses to a request built by a `GetAllRequestBuilder` or a `GetByKeyRequestBuilder`, therefore you will have to transform your response to a list of addresses and return the first element in this list using the `as` method. Return the converted entities:

```JavaScript
async function updateAndReadAddressList(addressList) {
  // ...
  const [updateChangesetResponse, ...retrieveResponses] = await batch(
    changeset(...updateRequests),
    ...retrieveRequests
  )
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });

  return retrieveResponses.map(response =>
    (response).as(BusinessPartnerAddress)[0]
  );
}  
```
[OPTION END]

> The syntax used to assign the retrieve responses to a variable is called rest pattern and is part of [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#Array_destructuring). It allows to assign all but the first response from the list of batch responses to a variable without slicing the array. It is equivalent to the older syntax: `const retrieveResponses = await batch(...).slice(1);`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Try out your batch API)]

You can check whether your `update-address-list` route works by executing a `POST` request against this URL using [Postman](https://www.getpostman.com/), curl or any other tool of your choice. Set the `Content-Type` header to `application/json` and use the following as your request body:
```JSON
{
	"addressList":[{
		"businessPartner": "1003764", "addressId": "28238", "region": "Karlsruhe"
	},{
		"businessPartner": "1003765", "addressId": "28241", "county": "Santa Clara"
	}]
}
```

Note, that the ids might not exist on your SAP S/4HANA or mock server. Feel free to adjust this to your data. The response should contain full address data for all of the requests that were sent - two in the example above.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]
---
