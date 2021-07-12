---
title: Send Multiple OData Requests in Batch Mode with SAP Cloud SDK's Virtual Data Model
description: Build and execute OData batch requests for writing and retrieving data with the SAP Cloud SDK's Virtual Data Model in your Address Manager application.
auto_validation: true
time: 20
tags: [tutorial>advanced, topic>javascript, products>sap-business-technology-platform, topic>odata]
primary_tag: products>sap-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites).
 - Have access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html).
 - Recommended to explore the more basic CRUD operations by going through [these tutorials](group.cloudsdk-js-vdm), but not required.
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
You will create an additional service that allows to update a list of addresses independently of business partners and returns the complete addresses after they were updated. We assume you start from the [address manager application](group.cloudsdk-js-vdm) from the previous tutorial series. As a first step we extend the `business-partner.service.ts` to contain a method responsible for the update with an empty implementation:

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';
import { BusinessPartnerAddress } from '@sap/cloud-sdk-vdm-business-partner-service';

@Injectable()
export class BusinessPartnerService {
  updateAddreses(businessPartnerAddresses: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
    return;
  }
  buildAddressList(body: any): BusinessPartnerAddress[] {
    return body.map(address => BusinessPartnerAddress.builder().fromJson(address));
  }
}
```
Then add a new endpoint to trigger an update for a list of addresses:

```JavaScript / TypeScript
import { Controller, Body, Post } from '@nestjs/common';
import { BusinessPartnerService } from './business-partner.service';
import { BusinessPartnerAddress } from '@sap/cloud-sdk-vdm-business-partner-service';

@Controller('business-partners')
export class BusinessPartnerController {
  constructor(private readonly businessPartnerService: BusinessPartnerService) {}

  @Post('/update-address-list')
  updateAddressList(@Body() body): Promise<BusinessPartnerAddress[]> {
    const addresses = this.businessPartnerService.buildAddressList(body);
    return this.businessPartnerService.updateAddreses(addresses);
  }
}
```

Note, that we have left any type declaration on the body object to keep the sample codes short. In a realistic TypeScript project you would of course define an interface for the data structures to enforce a proper API contract.

In case you have worked on the previous tutorials before the release of version `1.11.2` of the SAP Cloud SDK make sure to update your dependencies to the latest version (any version >= `1.11.2`). Just install the used libraries again:

```Shell
npm install @sap-cloud-sdk/core @sap/cloud-sdk-vdm-business-partner-service
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Create a batch request with a changeset)]

In your `business-partner.service.ts` we can now start to implement the `updateAddreses()` method:

```JavaScript / TypeScript
updateAddreses(businessPartnerAddresses: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  const updateRequests = businessPartnerAddresses.map(address => BusinessPartnerAddress.requestBuilder().update(address));

  return Promise.resolve([]);
}
```

Wrap these update requests in a `changeset` and pass the `chageset` to the `batch` function of the `@sap/cloud-sdk-vdm-business-partner-service`. This creates a batch request that can be executed the same way all other requests provided by the SAP Cloud SDK can be executed, by calling the `execute` function and passing your destination - a URL in this example:

```JavaScript / TypeScript
import { batch, BusinessPartnerAddress, changeset } from '@sap/cloud-sdk-vdm-business-partner-service';

updateAddreses(businessPartnerAddresses: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  const updateRequests = businessPartnerAddresses.map(address => BusinessPartnerAddress.requestBuilder().update(address));

  batch(changeset(...updateRequests)).execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
  return Promise.resolve([]);
}
```

Congratulations! You created a batch request using the SAP Cloud SDK.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Convert the changeset response to a list of entities)]

An SAP Cloud SDK batch request will return a list of `BatchResponse`s. It is the developer's responsibility to parse those responses and convert them to the expected entities.

Assign the batch response to a local variable. We only executed one changeset, therefore the response for the changeset will be the first element in the response.

Use a type assertion (`as`) to inform the compiler that you are handling a response of type `WriteResponses`. This response provides all responses of the requests within the changeset as `responses`. Transform those to instances of `BusinessPartnerAddress` using the `.as` function of each `WriteResponse`. As a `WriteResponse` can belong to either create, update or delete requests, it is possible that there is no response to transform. Therefore you have to make sure to call `as!` with an exclamation mark, to inform the compiler, that we know that the write request we put into our changeset responds with data.

Your code should look like this:
```JavaScript / TypeScript
import { WriteResponses } from '@sap-cloud-sdk/core';

async updateAddreses(businessPartnerAddresses: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  const updateRequests = businessPartnerAddresses.map(address => BusinessPartnerAddress.requestBuilder().update(address));

  const [updateChangesetResponse] = await batch(changeset(...updateRequests)).execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
  return (updateChangesetResponse as WriteResponses).responses.map(response => response.as!(BusinessPartnerAddress));
}
```

> The syntax used to assign the response to a variable is called [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#Array_destructuring). It allows retrieve the response from an array without accessing it by its index. It is equivalent to the older syntax: `const updateChangesetResponse = await batch(...)[0];`.

The update request of the SAP Cloud SDK only responds with the local diff that was sent to update the entity. In order to get the complete remote state of the entity, you will have to retrieve each entity from your destination as described in the following.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Retrieve an address list in batch mode)]

In order to retrieve the complete updated addresses, you will add more requests to your batch request. The first set of requests will trigger the update, the second set will trigger the data retrieval. The retrieve requests are based on a number of requests using the `GetByKeyRequestBuilder` of `BusinessPartnerAddress`. Create a list of requests based on the given `addressList` using the key properties `businessPartner` and `addressId` for each address:

```JavaScript / TypeScript
async updateAddreses(businessPartnerAddresses: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  const updateRequests = businessPartnerAddresses.map(address => BusinessPartnerAddress.requestBuilder().update(address));
  const retrieveRequests = businessPartnerAddresses.map(address =>
    BusinessPartnerAddress.requestBuilder().getByKey(address.businessPartner, address.addressId)
  );

  await batch(changeset(...updateRequests), ...retrieveRequests).execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
  //evaluation of response in next step.
}
```

You created a batch request, that executes a number of updates and retrievals. Note, that the change set will be executed first and the retrieve requests will be executed sequentially afterwards. The order of execution within the changeset is not defined.

What is left to do is to take the response from the retrieve set and return it. A batch request with the SAP Cloud SDK responds with a list of `BatchResponse`. The first element in this list will be the response to the changeset. The rest will be the responses for your retrieve requests. Assign those to a local variable and convert each of the `ReadResonse`s to a `BusinessPartnerAddress`.

The response type of retrieval request is `ReadResponse`. `ReadResonse`s can either be responses to a request built by a `GetAllRequestBuilder` or a `GetByKeyRequestBuilder`, therefore the transformation method `as` on the `ReadResponse` returns a list of objects. In our case each list has length 1, since we used a `byKey` request. A reduce() collects all addresses in the end:

```JavaScript / TypeScript
import { ReadResponse } from '@sap-cloud-sdk/core';

async updateAddreses(businessPartnerAddresses: BusinessPartnerAddress[]): Promise<BusinessPartnerAddress[]> {
  const updateRequests = businessPartnerAddresses.map(address => BusinessPartnerAddress.requestBuilder().update(address));
  const retrieveRequests = businessPartnerAddresses.map(address =>
    BusinessPartnerAddress.requestBuilder().getByKey(address.businessPartner, address.addressId)
  );

  const [updateChangesetResponse, ...retrieveResponses] = await batch(changeset(...updateRequests), ...retrieveRequests).execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
  return retrieveResponses.reduce((addresses, response: ReadResponse) => [...addresses, ...response.as(BusinessPartnerAddress)], []);
}

buildAddressList(body: any): BusinessPartnerAddress[] {
  return body.map(address => BusinessPartnerAddress.builder().fromJson(address));
}
```

> The syntax used to assign the retrieve responses to a variable is called rest pattern and is part of [destructuring](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Destructuring_assignment#Array_destructuring). It allows to assign all but the first response from the list of batch responses to a variable without slicing the array. It is equivalent to the older syntax: `const retrieveResponses = await batch(...).slice(1);`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Try out your batch API)]

The expected input for your `update-address-list` endpoint is a list of addresses in JSON format. Each of those must provide the key fields `businessPartner` and `addressId` otherwise you won't be able to find the correct address to update. You can check whether your endpoint works by executing a `POST` request against `http://localhost:3000/business-partners/update-address-list` using [Postman](https://www.getpostman.com/), curl or any other tool of your choice. Set the `Content-Type` header to `application/json` and use the following as your request body:
```JSON
[
  {"businessPartner": "1003764", "addressId": "28238", "region": "Karlsruhe"},
	{"businessPartner": "1003765", "addressId": "28241", "county": "Santa Clara"}
]
```
As a response you should see a list containing two full address objects. In the first object the "region" should be set to "Karlsruhe" and the "country" should be "Santa Clara" for the second one.

Note, that the ids might not exist on your SAP S/4HANA or mock server. Feel free to adjust this to your data. The response should contain full address data for all of the requests that were sent - two in the example above.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]
---
