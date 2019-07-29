---
title: Build OData Queries with the SAP Cloud SDK's Virtual Data Model
description: Build OData queries with the SAP Cloud SDK's virtual data model to build an address manager application.
auto_validation: true
time: 30
tags: [ tutorial>intermediate, topic>javascript, products>sap-cloud-platform, topic>odata]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites).
 - Have access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html).
 - Basic knowledge of OData is recommended, but not required.

## Details
### You will learn
  - What the OData Virtual Data Model for SAP S/4HANA Cloud is
  - How to use the Virtual Data Model to query the business partner service
  - How to expose the business partners in an `Express.js` application

The goal of this tutorial group is to show you how to implement a JavaScript application that allows you to manage the addresses of business partners. This application will be using `Express.js` and the SAP Cloud SDK for JavaScript. In this tutorial, we introduce the SAP Cloud SDK's OData Virtual Data Model and teach you how to use it to query business partners from an SAP S/4HANA Cloud system.

---

[ACCORDION-BEGIN [Step 1: ](What is the OData virtual data model?)]

Most of the services exposed by SAP S/4HANA Cloud and On-Premise are OData services. OData is a [RESTful API protocol](https://www.odata.org/) that has two key features: First, each services is described by a metadata document that lists all entities, their properties and relations, and which operations can be executed on them. Second, OData defines a set of SQL-like operators that allow constructing powerful queries.

However, building requests by hand can be tedious and error-prone. It's easy to mistype the name of an entity or a property, which will make the request fail. Furthermore, you have to continuously cross-check with the service's metadata to look up the spelling and types of entities and properties.

The Virtual Data Model is a set of API clients that are generated from a service's metadata. Every entity and their properties are represented by concrete objects. This allows you to build requests in a fluent, type-safe and discoverable manner.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up your application)]

This tutorial and the following ones in this group are based on the SAP Cloud SDK's [TypeScript project scaffolding](https://github.com/SAP/cloud-s4-sdk-examples/tree/scaffolding-ts). We have described how to set it up in a [previous tutorial](s4sdkjs-getting-started). Go ahead and download the project scaffolding as starting point for these tutorials.

For each OData service in SAP S/4HANA Cloud, there is a corresponding npm package in the SAP Cloud SDK. You can find an overview of [all packages here](https://help.sap.com/doc/9dbcab0600b346c2b359a8c8978a45ba/1.0/en-US/globals.html). All of the VDM packages are prefixed with `@sap/cloud-sdk-vdm`. The package for the business partner service can be installed by executing the following command on your command line:

```Shell
npm install @sap/cloud-sdk-vdm-business-partner-service
```

Starting with version `1.4.0` of the SAP Cloud SDK for JavaScript, you can also use our generator for your own custom OData services.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add an API endpoint)]

[OPTION BEGIN [TypeScript]]

Start by creating a file called `business-partners-route.ts` in the `src` folder of the project. Copy the following code into the file:

```JavaScript / TypeScript
import { Request, Response } from 'express';

export function businessPartnerRoute(req: Request, res: Response) {
  res.status(200).send('Nothing to see yet...');
}
```

The function `businessPartnerRoute` is the function that will handle requests to the API endpoint that we create. Every function in `Express.js` that handles a request takes two parameters: the `Request` object and the `Response` object (though we don't need the `Request` for now).

Now you need to instruct the `Express.js` router to delegate incoming requests to `/business-partners` to the `businessPartnerRoute` function. To do so, open the `application.ts` file, import your function and add a route to the `router`:

```JavaScript / TypeScript
import { businessPartnerRoute } from './business-partner-route';

// ...

private routes(): void {
  const router = express.Router();

  router.get("/", indexRoute);
  router.get("/hello", helloWorld);
  // add the following line
  router.get("/business-partners", businessPartnerRoute);
  this.app.use("/", router);
}
```

Now you can start your app using `npm run start:local` or `npm run serve-debug`. When the server is running, you should see `Express server listening on port 8080` on your command line. Open `http://localhost:8080/business-partners` and you should see `'Nothing to see yet...'`.

[OPTION END]

[OPTION BEGIN [JavaScript]]

Start by creating a file called `business-partners-route.js` in the `src` folder of the project. Copy the following code into the file:

```JavaScript
function businessPartnerRoute(req, res) {
  res.status(200).send('Nothing to see yet...');
}

module.exports.businessPartnerRoute = businessPartnerRoute;
```

The function `businessPartnerRoute` is the function that will handle requests to the API endpoint that we create. Every function in `Express.js` that handles a request takes two parameters: the `Request` object and the `Response` object (though we don't need the `Request` for now).

Now you need to instruct the `Express.js` router to delegate incoming requests to `/business-partners` to the `businessPartnerRoute` function. To do so, open the `application.js` file, import your function and add a route to the `router`:

```JavaScript
const { businessPartnerRoute } = require('./business-partner-route');

// ...

private routes() {
  const router = express.Router();

  router.get("/", indexRoute);
  router.get("/hello", helloWorld);
  // add the following line
  router.get("/business-partners", businessPartnerRoute);
  this.app.use("/", router);
}
```

Now you can start your app using `npm run start:local` or `npm run serve-debug`. When the server is running, you should see `Express server listening on port 8080` on your command line. Open `http://localhost:8080/business-partners` and you should see `'Nothing to see yet...'`.

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Query the business partner service)]

Every request you can do with the VDM follows a common pattern. You start with the entity that you want to perform a request on, in this case `BusinessPartner`. To build a request, call the `requestBuilder` function. Next, you can select which request to build. OData, as RESTful API protocol, follows the CRUD model: Create, Read, Update, Delete. For reading, we differentiate between querying the service, where you get all available entities if you don't specifically restrict the result set, and retrieving a specific entity by its key. The respective functions are called `getAll` and `getByKey`. For the remaining requests, the functions are simply called `create`, `update` and `delete`. When you type `BusinessPartner.requestBuilder().`, your IDE should show which operations are available on the respective entity. For example, it is not possible to delete a business partner via the business partner service. Therefore, the VDM will not offer a `delete` function.

[OPTION BEGIN [TypeScript]]

Following this pattern, update your code as shown below (mind the updated `import`):

```JavaScript / TypeScript
import { Request, Response } from 'express';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

export function businessPartnerRoute(req: Request, res: Response) {
  res.status(200).send('Nothing to see yet...');
}

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll();
}
```

Requests can be executed with the `execute` function. To do this, the request builder needs to know where to send this request, in the form of a `Destination`. In this tutorial, we will provide this information directly to the `execute` function. We have described how to integrate with SAP Cloud Platform's Destination Service in [this tutorial](s4sdkjs-deploy-application-cloud-foundry). Every destination requires a URL. Note, that we do not need the full URL to the service, but only the host of the system. Suppose, you have an SAP S/4HANA Cloud system running under `https://my.s4hana.ondemand.com/`, you can pass that information directly to the request builder like this:

```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```

[OPTION END]

[OPTION BEGIN [JavaScript]]

Following this pattern, update your code as shown below (mind the updated `require`):

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function businessPartnerRoute(req, res) {
  res.status(200).send('Nothing to see yet...');
}

module.exports.businessPartnerRoute = businessPartnerRoute;

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll();
}
```

Requests can be executed with the `execute` function. To do this, the request builder needs to know where to send this request, in the form of a `Destination`. In this tutorial, we will provide this information directly to the `execute` function. We have described how to integrate with SAP Cloud Platform's Destination Service in [this tutorial](s4sdkjs-deploy-application-cloud-foundry). Every destination requires a URL. Note, that we do not need the full URL to the service, but only the host of the system. Suppose, you have an SAP S/4HANA Cloud system running under `https://my.s4hana.ondemand.com/`, you can pass that information directly to the request builder like this:

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add authentication to the request)]

[OPTION BEGIN [TypeScript]]

Typically, you will need to authenticate yourself against a system in order to successfully execute a request. If you have a technical user for your system, you can pass the credentials like this:

```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      url: 'https://my.s4hana.ondemand.com/',
      username: "USERNAME",
      password: "PASSWORD"
    });
}
```

Alternatively, if you want to use the sandbox of the [SAP API Business Hub](https://api.sap.com), you will need to provide an `APIKey`. `withCustomHeaders` allows you to add custom HTTP headers to the request. You can pass your API key like this:

```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .withCustomHeaders({
      APIKey: 'YOUR-API-KEY'
    })
    .execute({
      url: "https://sandbox.api.sap.com/s4hanacloud/"
    });
}
```

[OPTION END]

[OPTION BEGIN [JavaScript]]

Typically, you will need to authenticate yourself against a system in order to successfully execute a request. If you have a technical user for your system, you can pass the credentials like this:

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      url: 'https://my.s4hana.ondemand.com/',
      username: "USERNAME",
      password: "PASSWORD"
    });
}
```

Alternatively, if you want to use the sandbox of the [SAP API Business Hub](https://api.sap.com), you will need to provide an `APIKey`. `withCustomHeaders` allows you to add custom HTTP headers to the request. You can pass your API key like this:

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .withCustomHeaders({
      APIKey: 'YOUR-API-KEY'
    })
    .execute({
      url: "https://sandbox.api.sap.com/s4hanacloud/"
    });
}
```

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add select to the request)]

Like SQL, OData allows to only select specific properties of an entity. For our address manager, we only want to know the ID, the first name and the last name of a business partner. Add a select statement to your request like this:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

As you can see, each property we select is represented by an object on the `BusinessPartner` entity. If you type `BusinessPartner.` in your IDE, you will see all the properties the can be selected on that entity. This saves you from having to look up the properties in the metadata, and prevents errors due to mistyping.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add a filter to the request)]

Business partners can either be natural persons or legal persons (e.g. organizations or companies). For the address manager, we only want the addresses of natural persons. Therefore, we need to a filter to our request. Modify your code like this:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1')
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1')
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

As for `select`, you can use the properties of the `BusinessPartner` entity directly for filtering. Each property offers a set of functions for constructing filters. Every property has the `equals` and `nonEquals` function. Depending on that type of the property, there can be additional functions like `greaterThan` or `greaterOrEqual`. Also, since we know the type of the property, the VDM will prevent your from passing values of the wrong type. For example, `BusinessPartner.FIRST_NAME.equals(1)` would not compile (in pure JavaScript the code would only fail at runtime, but most editors will still raise a warning for the type mismatch).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Optional: Complex filters)]

While this not required for the address manager, it's a good time to introduce complex filters. By default, multiple filter statements passed to the `filter` function will be combined with a logical `AND`. For example, the following code will only retrieve business partners that are natural persons and whose first name is not "John":

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1'),
      BusinessPartner.FIRST_NAME.notEquals('John')
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```

However, suppose we also want to retrieve business partners if they have been created in 2019 or later. For such use cases, you can use the `and` and `or` functions.

```JavaScript / TypeScript
import { and, or } from '@sap/cloud-sdk-core';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';
import * as moment from 'moment';

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      or(
        BusinessPartner.CREATION_DATE.greaterThan(moment('2019-01-01 00:00:00')),
        and(
          BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1'),
          BusinessPartner.FIRST_NAME.notEquals('Joe')
        )
      )
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1'),
      BusinessPartner.FIRST_NAME.notEquals('John')
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```

However, suppose we also want to retrieve business partners if they have been created in 2019 or later. For such use cases, you can use the `and` and `or` functions.

```JavaScript
const { and, or } = require('@sap/cloud-sdk-core');
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');
const moment = require('moment');


function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      or(
        BusinessPartner.CREATION_DATE.greaterThan(moment('2019-01-01 00:00:00')),
        and(
          BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1'),
          BusinessPartner.FIRST_NAME.notEquals('Joe')
        )
      )
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Optional: Custom fields)]

The OData services in SAP S/4HANA Cloud can be extended by so-called custom fields. A custom field is any property that is not present in the default metadata of a service. Therefore, custom fields are of not "known" to the VDM. However, we still offer the same level of convenience for `select` and `filter`.

Suppose you have extended your business partner entity by a field that stores which user has last checked the addresses. You could use this field for selecting and filtering like this:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

const ADDR_LAST_CHECKED_BY: CustomField<BusinessPartner> = BusinessPartner.customField('YY1_AddrLastCheckedBy_bus');

BusinessPartner.requestBuilder()
  .getAll()
  .select(
    BusinessPartner.FIRST_NAME,
    BusinessPartner.LAST_NAME,
    ADDR_LAST_CHECKED_BY
  )
  .filter(ADDR_LAST_CHECKED_BY.notEquals('John_Doe'))
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
```

Every entity offers a set of functions to interact with custom fields. You can get the value of a specific field using the `getCustomField` function. Alternatively, you can also get an object holding all fields using `getCustomFields`.

```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

// pre-existing businessPartner

const fieldValue: any = businessPartner.getCustomField('YY1_AddrLastCheckedBy_bus');
const allCustomFields: { [key: string]: any } = businessPartner.getCustomFields();
```

Note, that the values will always be typed with `any`. The type of a custom field can be any JSON primitive, i.e. `boolean`, `string`, `number` and `null`. If there is no custom field with the given name, `getCustomField` will return `undefined`. You can check whether a custom field with a given name exists using the `hasCustomField` function. You can set the value for a custom field using the `setCustomField` function:

```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

// pre-existing businessPartner

businessPartner.setCustomField('YY1_AddrLastCheckedBy_bus', 'John_Doe');
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

const ADDR_LAST_CHECKED_BY = BusinessPartner.customField('YY1_AddrLastCheckedBy_bus');

BusinessPartner.requestBuilder()
  .getAll()
  .select(
    BusinessPartner.FIRST_NAME,
    BusinessPartner.LAST_NAME,
    ADDR_LAST_CHECKED_BY
  )
  .filter(ADDR_LAST_CHECKED_BY.notEquals('John_Doe'))
  .execute({
    url: 'https://my.s4hana.ondemand.com/'
  });
```

Every entity offers a set of functions to interact with custom fields. You can get the value of a specific field using the `getCustomField` function. Alternatively, you can also get an object holding all fields using `getCustomFields`.

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

// pre-existing businessPartner

const fieldValue = businessPartner.getCustomField('YY1_AddrLastCheckedBy_bus');
const allCustomFields = businessPartner.getCustomFields();
```

Note, that the values will always be typed with `any`. The type of a custom field can be any JSON primitive, i.e. `boolean`, `string`, `number` and `null`. If there is no custom field with the given name, `getCustomField` will return `undefined`. You can check whether a custom field with a given name exists using the `hasCustomField` function. You can set the value for a custom field using the `setCustomField` function:

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

// pre-existing businessPartner

businessPartner.setCustomField('YY1_AddrLastCheckedBy_bus', 'John_Doe');
```
[OPTION END]

Note, that you cannot use a field name that already exists. For example, if you would try to use `setCustomField('FirstName', 'Joe')`, an error would be thrown, because the field `FirstName` already exists on the business partner entity.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Wire everything up)]

[OPTION BEGIN [TypeScript]]
You've finished the implementation of `getAllBusinessPartners`! To make the result available to clients, you need to call the function in `businessPartnersRoute` and send the result to clients. Update `business-partners-route.ts` so that it looks like this:

```JavaScript / TypeScript
import { Request, Response } from 'express';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

export function businessPartnerRoute(req: Request, res: Response) {
  getAllBusinessPartners()
    .then(businessPartners => {
      res.status(200).send(businessPartners);
    })
    .catch(error => {
      res.status(500).send(error.message);
    })
}

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1')
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
You've finished the implementation of `getAllBusinessPartners`! To make the result available to clients, you need to call the function in `businessPartnersRoute` and send the result to clients. Update `business-partners-route.js` so that it looks like this:

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function businessPartnerRoute(req, res) {
  getAllBusinessPartners()
    .then(businessPartners => {
      res.status(200).send(businessPartners);
    })
    .catch(error => {
      res.status(500).send(error.message);
    })
}

module.exports.businessPartnerRoute = businessPartnerRoute;

function getAllBusinessPartners() {
  return BusinessPartner.requestBuilder()
    .getAll()
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.LAST_NAME
    )
    .filter(
      BusinessPartner.BUSINESS_PARTNER_CATEGORY.equals('1')
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

`businessPartnerRoute` now calls the `getAllBusinessPartners`, which returns a `Promise`. A `Promise` can have two result states. Either the execution of the wrapped function succeeded, or it failed.

The success case can be handled with the `then` function. This gives you access to the return value of the wrapped function, which is an array of business partners. `res.status(200).send(businessPartners)` sends a response to the client with HTTP status code `200` and the business partner array as body.

Similarly, the error case is handled with the `catch` function. Should an error by thrown, we respond with status code `500` and send the error message to the client.

Restart your server and open `http://localhost:8080/business-partners`. You should see a list of business partners!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]
[VALIDATE_2]
[VALIDATE_3]

[ACCORDION-END]
---
