---
title: Build OData Queries with the SAP Cloud SDK's Virtual Data Model
description: Build OData queries with the SAP Cloud SDK's virtual data model to build an address manager application.
auto_validation: true
time: 30
tags: [ tutorial>intermediate, topic>javascript, products>sap-business-technology-platform, topic>odata]
primary_tag: products>sap-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites).
 - Have access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html).
 - Basic knowledge of OData is recommended, but not required.

## Details
### You will learn
  - What the OData Virtual Data Model for SAP S/4HANA Cloud is
  - How to use the Virtual Data Model to query the business partner service
  - How to expose the business partners in a `NestJS` application

The goal of this tutorial group is to show you how to implement a JavaScript application that allows you to manage the addresses of business partners. This application will be using `NestJS` and the SAP Cloud SDK for JavaScript. In this tutorial, we introduce the SAP Cloud SDK's OData Virtual Data Model and teach you how to use it to query business partners from an SAP S/4HANA Cloud system.

---

[ACCORDION-BEGIN [Step 1: ](What is the SAP Cloud SDK's Virtual Data Model?)]

Most of the services exposed by SAP S/4HANA Cloud and On-Premise are OData services. OData is a [RESTful API protocol](https://www.odata.org/) that has two key features:

- Each service is described by a metadata document that lists all entities, their properties and relations, and which operations can be executed on them.
- OData defines a set of SQL-like operators that allow constructing powerful queries.

However, building requests by hand can be tedious and error-prone. It's easy to mistype the name of an entity or a property, which will make the request fail. Furthermore, you have to continuously cross-check with the service's metadata to look up the spelling and types of entities and properties.

The Virtual Data Model is a set of API clients that are generated from a service's metadata. Every entity and their properties are represented by concrete objects. This allows you to build requests in a fluent, type-safe and discoverable manner.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up your application)]

This tutorial and the following ones in this group are based on the SAP Cloud SDK's project scaffolding. We have described how to set it up in a [previous tutorial](s4sdkjs-getting-started). Go ahead and generate a fresh `NestJS` project as a starting point as described in this tutorial.

For each OData service in SAP S/4HANA Cloud, there is a corresponding npm package in the SAP Cloud SDK. You can find an overview of [all packages here](https://help.sap.com/doc/9dbcab0600b346c2b359a8c8978a45ba/1.0/en-US/globals.html). All of the VDM packages are prefixed with `@sap/cloud-sdk-vdm`. The package for the business partner service can be installed by executing the following command on your command line:

```Shell
npm install @sap/cloud-sdk-vdm-business-partner-service
```

Starting with version `1.4.0` of the SAP Cloud SDK for JavaScript, you can also use our generator for your own custom OData services.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add an API endpoint)]

A `NestJS` application is based on three main entities: `Service`, `Controller` and `Module`.

In the scaffold a set of these entities has already been created: `app.service.ts`, `app.controller.ts` and `app.module.ts`. We will create a new set in order to query business partners from a S/4HANA Cloud system. For details on the concepts have a look at the [Nest tutorials](https://docs.nestjs.com/first-steps). Start with a `business-partner.service.ts` file, which will contain the implementation of the query:

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';

@Injectable()
export class BusinessPartnerService {
  public dummyText = 'Not yet implemented.';

  getAllBusinessPartners(): string {
    return this.dummyText;
  }
}
```

Then create a `business-partner.controller.ts` which takes care of directing incoming requests to the needed implementation:

```JavaScript / TypeScript
import { Controller, Get } from '@nestjs/common';
import { BusinessPartnerService } from './business-partner.service';

@Controller('business-partners')
export class BusinessPartnerController {
  constructor(private readonly businessPartnerService: BusinessPartnerService) {}

  @Get()
  getAllBusinessPartners(): string {
    return this.businessPartnerService.dummyText;
  }
}
```

As a last step we have to register the controller and service. In order to keep the example brief, just add the controller and service to the `app.module.ts` which is the root module of the Nest application:

```JavaScript / TypeScript
import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { BusinessPartnerController } from './business-partner.controller';
import { BusinessPartnerService } from './business-partner.service';

@Module({
  imports: [],
  controllers: [AppController,BusinessPartnerController],
  providers: [AppService,BusinessPartnerService],
})
export class AppModule {}
```

Now you can start your application using:
```Shell
npm start
```
When the server is running, you should see a message like: `[NestApplication] Nest application successfully started` and some mappings related to the listed controllers. Open the URL `http://localhost:3000/business-partners` and you should see `Not yet implemented.`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Query the business partner service)]

Every request you can do with the VDM follows a common pattern. You start with the entity that you want to perform a request on, in this case `BusinessPartner`. To build a request, call the `requestBuilder` function.
Next, you can select which request to build. OData, as RESTful API protocol, follows the CRUD model: Create, Read, Update, Delete. For reading, we differentiate between querying the service, where you get all available entities if you don't specifically restrict the result set, and retrieving a specific entity by its key.
The respective functions are called `getAll` and `getByKey`. For the remaining requests, the functions are simply called `create`, `update` and `delete`. When you type `BusinessPartner.requestBuilder().`, your IDE should show which operations are available on the respective entity. For example, it is not possible to delete a business partner via the business partner service. Therefore, the VDM will not offer a `delete` function.

Following this pattern, update your service as shown below (mind the updated `import`):

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

@Injectable()
export class BusinessPartnerService {
  public dummyText = 'Not yet implemented.';

  getAllBusinessPartners(): any {
    return BusinessPartner.requestBuilder()
    .getAll();
  }
}
```

Requests can be executed with the `execute` function. To do this, the request builder needs to know where to send this request, in the form of a `Destination`. In this tutorial, we will provide this information directly to the `execute` function. We have described how to integrate with SAP Cloud Platform's Destination Service in [this tutorial](s4sdkjs-deploy-application-cloud-foundry). Every destination requires a URL.
Note, that we do not need the full URL to the service, but only the host of the system. Suppose, you have an SAP S/4HANA Cloud system running under `https://my.s4hana.ondemand.com/`, you can pass that information directly to the request builder like this (for simplicity we omit the imports and class definition from the previous code snippets):

```JavaScript / TypeScript
getAllBusinessPartners(): Promise<BusinessPartner[]> {
    return BusinessPartner.requestBuilder()
      .getAll()
      .execute({ url: 'https://my.s4hana.ondemand.com/' });
  }
}
```
Note that the response is properly typed as a promise containing a list of business-partners.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Add authentication to the request)]

Typically, you will need to authenticate yourself against a system in order to successfully execute a request. If you have a technical user for your system, you can pass the credentials like this:

```JavaScript / TypeScript
getAllBusinessPartners(): Promise<BusinessPartner[]> {
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
getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .withCustomHeaders({
      APIKey: 'YOUR-API-KEY'
    })
    .execute({
      url: 'https://sandbox.api.sap.com/s4hanacloud/'
    });
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Add select to the request)]

Like SQL, OData allows to only select specific properties of an entity. For our address manager, we only want to know the ID, the first name and the last name of a business partner. Add a select statement to your request like this:

```JavaScript / TypeScript
getAllBusinessPartners(): Promise<BusinessPartner[]> {
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

As you can see, each property we select is represented by an object on the `BusinessPartner` entity. If you type `BusinessPartner.` in your IDE, you will see all the properties the can be selected on that entity. This saves you from having to look up the properties in the metadata, and prevents errors due to mistyping.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Add a filter to the request)]

Business partners can either be natural persons or legal persons (e.g. organizations or companies). For the address manager, we only want the addresses of natural persons. Therefore, we need to a filter to our request. Modify your code like this:

```JavaScript / TypeScript
getAllBusinessPartners(): Promise<BusinessPartner[]> {
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

As for `select`, you can use the properties of the `BusinessPartner` entity directly for filtering. Each property offers a set of functions for constructing filters. Every property has the `equals` and `notEquals` function. Depending on that type of the property, there can be additional functions like `greaterThan` or `greaterOrEqual`. Also, since we know the type of the property, the VDM will prevent you from passing values of the wrong type. For example, `BusinessPartner.FIRST_NAME.equals(1)` would not compile (in pure JavaScript the code would only fail at runtime, but most editors will still raise a warning for the type mismatch).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Optional: Complex filters)]

While this not required for the address manager, it's a good time to introduce complex filters. By default, multiple filter statements passed to the `filter` function will be combined with a logical `AND`. For example, the following code will only retrieve business partners that are natural persons and whose first name is not "John":

```JavaScript / TypeScript
getAllBusinessPartners(): Promise<BusinessPartner[]> {
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

However, suppose we also want to retrieve business partners if they have been created in 2019 or later. For such use cases, you can use the `and` and `or` functions from the cloud-sdk-core package.

```JavaScript / TypeScript
import { and, or } from '@sap-cloud-sdk/core';
import * as moment from 'moment';

getAllBusinessPartners(): Promise<BusinessPartner[]> {
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

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Optional: Custom fields)]

The OData services in SAP S/4HANA Cloud can be extended by so-called custom fields. A custom field is any property that is not present in the default metadata of a service. Therefore, custom fields are not "known" to the VDM. However, we still offer the same level of convenience for `select` and `filter`.

Suppose you have extended your business partner entity by a field that stores which user has last checked the addresses. You could use this field for selecting and filtering like this:

```JavaScript / TypeScript
const ADDR_LAST_CHECKED_BY: CustomField<BusinessPartner> = BusinessPartner.customField('YY1_AddrLastCheckedBy_bus');

getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
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
}
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

Note, that you cannot use a field name that already exists. For example, if you would try to use `setCustomField('FirstName', 'Joe')`, an error would be thrown, because the field `FirstName` already exists on the business partner entity.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Wire everything up)]

You've finished the implementation of `getBusinessPartners`! To make the result available to clients, you need to call the function in the controller and send the result to clients. Update `business-partners.controller.ts` so that it looks like this:

```JavaScript / TypeScript
import { Controller, Get } from '@nestjs/common';
import { BusinessPartnerService } from './business-partner.service';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

@Controller('business-partners')
export class BusinessPartnerController {
  constructor(private readonly businessPartnerService: BusinessPartnerService) {}

  @Get()
  getAllBusinessPartners(): Promise<BusinessPartner[]> {
    return this.businessPartnerService.getAllBusinessPartners();
  }
}
```
Restart your server and open `http://localhost:3000/business-partners`. You should see a list of business partners!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself (1))]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself (2))]

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself (3))]

[VALIDATE_3]

[ACCORDION-END]
---
