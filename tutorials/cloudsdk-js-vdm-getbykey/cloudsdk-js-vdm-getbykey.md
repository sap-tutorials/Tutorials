---
title: Use OData Navigation Properties with the SAP Cloud SDK's Virtual Data Model
description: Use OData navigation properties with the SAP Cloud SDK's virtual data model to duild an address manager application.
auto_validation: true
time: 20
tags: [ tutorial>intermediate, topic>javascript, products>sap-cloud-platform, topic>odata]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites)
 - Access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html)
 - Basic knowledge of OData is recommended, but not required

## Details
### You will learn
  - How to use the Virtual Data Model to get a single entity by its key
  - How to use the Virtual Data Model with OData navigation properties
  - How to expose this data in your `Express.js` application

The goal of this tutorial group is to show you how to implement a JavaScript application that allows you to manage the addresses of business partners. This application will be using `Express.js` and the SAP Cloud SDK for JavaScript. In this tutorial, we use the SAP Cloud SDK's OData Virtual Data Model to get a single business partner entity and all related addresses.

---

[ACCORDION-BEGIN [Step 1: ](Add an API endpoint)]

[OPTION BEGIN [TypeScript]]
Start by creating a new file called `single-business-partner-route.ts` in the `src` folder of your project. Then, copy the following code into it:

```JavaScript / TypeScript
import { Request, Response } from 'express';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

export function singleBusinessPartnerRoute(req: Request, res: Response) {
  res.status(200).send(req.params.id);
}
```

Then open `application.ts`, import the function and add a route definition:

```JavaScript / TypeScript
import { businessPartnerRoute } from './business-partner-route';
import { singleBusinessPartnerRoute } from './single-business-partner-route';

// ...

private routes(): void {
  const router = express.Router();

  router.get("/", indexRoute);
  router.get("/hello", helloWorld);
  router.get("/business-partners", businessPartnerRoute);
  // add the following line
  router.get("/business-partners/:id", singleBusinessPartnerRoute);
  this.app.use("/", router);
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
Start by creating a new file called `single-business-partner-route.js` in the `src` folder of your project. Then, copy the following code into it:

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function singleBusinessPartnerRoute(req, res) {
  res.status(200).send(req.params.id);
}

module.exports.singleBusinessPartnerRoute = singleBusinessPartnerRoute;
```

Then open `application.js`, import the function and add a route definition:

```JavaScript
const { businessPartnerRoute } = require('./business-partner-route');
const { singleBusinessPartnerRoute } = require('./single-business-partner-route');

// ...

private routes() {
  const router = express.Router();

  router.get("/", indexRoute);
  router.get("/hello", helloWorld);
  router.get("/business-partners", businessPartnerRoute);
  // add the following line
  router.get("/business-partners/:id", singleBusinessPartnerRoute);
  this.app.use("/", router);
}
```
[OPTION END]

Now you can start your app again with `npm run start:local` and open `http://localhost:8080/business-partners/1`. `Express.js` will automatically substitute `:id` from the route definition with the string you used and make it available in `req.params`. Since the route currently sends `req.params.id`, the server should respond with the exact string you used in the URL.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get a single business partner)]

[OPTION BEGIN [TypeScript]]
Next, you will implement a function to get a single business partner by its ID. Open `single-business-partner-route.ts` and add the following function to your code:

```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getBusinessPartner(businessPartnerId: string): Promise<BusinessPartner> {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[OPTION BEGIN [JavaScript]]
Next, you will implement a function to get a single business partner by its ID. Open `single-business-partner-route.js` and add the following function to your code:

```JavaScript
const { BusinessPartner } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getBusinessPartner(businessPartnerId) {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

As before, we call `BusinessPartner.requestBuilder()` to select the type of request we want to build. Since we only want to get a single business partner, we use the `getByKey` function and then call `execute` to execute the request. Add credentials as needed as shown in the [previous tutorial](cloudsdk-js-vdm-getall).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add select to the request)]

As in the `getAll` case, we can again select the specific properties of the entity that we're interested in. Add a select statement to your code as shown below:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getBusinessPartner(businessPartnerId: string): Promise<BusinessPartner> {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE
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

function getBusinessPartner(businessPartnerId) {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Expand to the business partner address)]

So far, we have only retrieved business partners. Since the application should be an address manager, we also need to get the addresses of a business partners. In the metadata, the `BusinessPartnerAddress` entity is modelled as a so-called "navigation property" of `BusinessPartner`. This means that there is a relation between a business partner and their addresses. In this case, one business partner can have multiple addresses.

To include navigation properties, OData offers the expand operation. The VDM allows you to use navigation properties in the `select` function just like normal properties. In SAP S/4HANA, navigation properties typically start with `TO_`. Add `BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS` to your code as shown below:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

function getBusinessPartner(businessPartnerId: string): Promise<BusinessPartner> {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE,
      BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```

As before, we're only interested in a subset of the properties. If the property you pass to the `select` function is a navigation property, you can again call `select` on it and choose the properties you're interested in. Modify your code as shown in the example below:

```JavaScript / TypeScript
import { BusinessPartner, BusinessPartnerAddress } from '@sap/cloud-sdk-vdm-business-partner-service';

function getBusinessPartner(businessPartnerId: string): Promise<BusinessPartner> {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE,
      BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS.select(
        BusinessPartnerAddress.BUSINESS_PARTNER,
        BusinessPartnerAddress.ADDRESS_ID,
        BusinessPartnerAddress.COUNTRY,
        BusinessPartnerAddress.POSTAL_CODE,
        BusinessPartnerAddress.CITY_NAME,
        BusinessPartnerAddress.STREET_NAME,
        BusinessPartnerAddress.HOUSE_NUMBER
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

function getBusinessPartner(businessPartnerId) {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE,
      BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```

As before, we're only interested in a subset of the properties. If the property you pass to the `select` function is a navigation property, you can again call `select` on it and choose the properties you're interested in. Modify your code as shown in the example below:

```JavaScript
const { BusinessPartner, BusinessPartnerAddress } = require('@sap/cloud-sdk-vdm-business-partner-service');

function getBusinessPartner(businessPartnerId) {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE,
      BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS.select(
        BusinessPartnerAddress.BUSINESS_PARTNER,
        BusinessPartnerAddress.ADDRESS_ID,
        BusinessPartnerAddress.COUNTRY,
        BusinessPartnerAddress.POSTAL_CODE,
        BusinessPartnerAddress.CITY_NAME,
        BusinessPartnerAddress.STREET_NAME,
        BusinessPartnerAddress.HOUSE_NUMBER
      )
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

Note, that the properties of the business partner address entity are offered by the corresponding `BusinessPartnerAddress` class, so don't forget to import it.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Wire everything up)]

Now that you've finished implementing the function, you only need to call it in `singleBusinessPartnerRoute`. Your final code should look like this:

[OPTION BEGIN [TypeScript]]
```JavaScript / TypeScript
import { Request, Response } from "express";
import { BusinessPartner, BusinessPartnerAddress } from '@sap/cloud-sdk-vdm-business-partner-service';

export function singleBusinessPartnerRoute(req: Request, res: Response) {
  getBusinessPartner(req.params.id)
    .then(businessPartner => {
      res.status(200).send(businessPartner);
    })
    .catch(error => {
      res.status(500).send(error.message);
    });
}

function getBusinessPartner(businessPartnerId: string): Promise<BusinessPartner> {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE,
      BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS.select(
        BusinessPartnerAddress.BUSINESS_PARTNER,
        BusinessPartnerAddress.ADDRESS_ID,
        BusinessPartnerAddress.COUNTRY,
        BusinessPartnerAddress.POSTAL_CODE,
        BusinessPartnerAddress.CITY_NAME,
        BusinessPartnerAddress.STREET_NAME,
        BusinessPartnerAddress.HOUSE_NUMBER
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
const { BusinessPartner, BusinessPartnerAddress } = require('@sap/cloud-sdk-vdm-business-partner-service');

function singleBusinessPartnerRoute(req, res) {
  getBusinessPartner(req.params.id)
    .then(businessPartner => {
      res.status(200).send(businessPartner);
    })
    .catch(error => {
      res.status(500).send(error.message);
    });
}

module.exports.singleBusinessPartnerRoute = singleBusinessPartnerRoute;

function getBusinessPartner(businessPartnerId) {
  return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .select(
      BusinessPartner.BUSINESS_PARTNER,
      BusinessPartner.LAST_NAME,
      BusinessPartner.FIRST_NAME,
      BusinessPartner.IS_MALE,
      BusinessPartner.IS_FEMALE,
      BusinessPartner.CREATION_DATE,
      BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS.select(
        BusinessPartnerAddress.BUSINESS_PARTNER,
        BusinessPartnerAddress.ADDRESS_ID,
        BusinessPartnerAddress.COUNTRY,
        BusinessPartnerAddress.POSTAL_CODE,
        BusinessPartnerAddress.CITY_NAME,
        BusinessPartnerAddress.STREET_NAME,
        BusinessPartnerAddress.HOUSE_NUMBER
      )
    )
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```
[OPTION END]

The function takes the ID from the URL and passes it to `getBusinessPartner`. If a business partner with the provided ID exists in the target system, it will be sent to the client. Otherwise, an error message is sent. Restart your server and open `http://localhost:8080/business-partners/1` to test it out! Of course you're free to use any other ID.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---
