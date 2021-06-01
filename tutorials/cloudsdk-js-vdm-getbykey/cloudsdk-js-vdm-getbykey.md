---
title: Use OData Navigation Properties with the SAP Cloud SDK's Virtual Data Model
description: Use OData navigation properties with the SAP Cloud SDK's virtual data model to duild an address manager application.
auto_validation: true
time: 20
tags: [ tutorial>intermediate, topic>javascript, products>sap-business-technology-platform, topic>odata]
primary_tag: products>sap-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites)
 - Access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html)
 - Basic knowledge of OData is recommended, but not required

## Details
### You will learn
  - How to use the Virtual Data Model to get a single entity by its key
  - How to use the Virtual Data Model with OData navigation properties
  - How to expose this data in your `NestJS` application

The goal of this tutorial group is to show you how to implement a JavaScript application that allows you to manage the addresses of business partners. This application will be using `NestJS` and the SAP Cloud SDK for JavaScript. In this tutorial, we use the SAP Cloud SDK's OData Virtual Data Model to get a single business partner entity and all related addresses.

---

[ACCORDION-BEGIN [Step 1: ](Add an API endpoint)]

In a [previous tutorial](cloudsdk-js-vdm-getall) we explained a bit the basics about `controller`, `service` and `module` of `NestJS` applications. Note: If you have already controller and service classes from the previous tutorial you can of course keep the existing files and just extend the classes by the new methods. Since we want to expose a new endpoint, start by creating  a controller file called `business-partner.controller.ts` in the `src` folder of your project. Then, copy the following code into it:

```JavaScript / TypeScript
import { Controller, Get, Param } from '@nestjs/common';

@Controller('business-partners')
export class BusinessPartnerController {

  @Get('/:businessPartnerId')
  getBusinessPartnerByID(@Param('businessPartnerId') businessPartnerId): string {
    return `My id for query is: ${businessPartnerId}`;
  }
}
```

Then prepare a service file called `business-partner.service.ts` and add this dummy implementation:

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

@Injectable()
export class BusinessPartnerService {
  getBusinessPartnerById(businessPartnerId: string): Promise<BusinessPartner> {
    return;
  }
}
```

Finally, register the `controller` and `service` in the root application module `app.module.ts`:

```JavaScript / TypeScript
import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { BusinessPartnerController } from './business-partner.controller';
import { BusinessPartnerService } from './business-partner.service';

@Module({
  imports: [],
  controllers: [AppController, BusinessPartnerController],
  providers: [AppService, BusinessPartnerService]
})
export class AppModule {}
```

Now you can restart your app with:
```Shell
npm start
```
And open `http://localhost:3000/business-partners/TestID`. You should see the response: `My id for query is: TestID`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get a single business partner)]

Next, you will implement the `getBusinessPartnerById` function to get a single business partner by its ID. Open `business-partner.service.ts` and add the following function to your code:

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

@Injectable()
export class BusinessPartnerService {
  getBusinessPartnerById(businessPartnerId: string): Promise<BusinessPartner> {
    return BusinessPartner.requestBuilder()
    .getByKey(businessPartnerId)
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
  }
}
```

As before, we call `BusinessPartner.requestBuilder()` to select the type of request we want to build. Since we only want to get a single business partner, we use the `getByKey` function and then call `execute` to execute the request. Add credentials as needed as shown in the [previous tutorial](cloudsdk-js-vdm-getall).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add select to the request)]

As in the `getAll` case, we can again select the specific properties of the entity that we're interested in. Add a select statement to your code as shown below:
(for simplicity we omit the import and class declaration and focus on the method implementation)

```JavaScript / TypeScript
getBusinessPartnerById(businessPartnerId: string): Promise<BusinessPartner> {
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

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Expand to the business partner address)]

So far, we have only retrieved business partners. Since the application should be an address manager, we also need to get the addresses of a business partners. In the metadata, the `BusinessPartnerAddress` entity is modelled as a so-called "navigation property" of `BusinessPartner`. This means that there is a relation between a business partner and their addresses. In this case, one business partner can have multiple addresses.

To include navigation properties, OData offers the expand operation. The VDM allows you to use navigation properties in the `select` function just like normal properties. In SAP S/4HANA, navigation properties typically start with `TO_`. Add `BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS` to your code as shown below:

```JavaScript / TypeScript

getBusinessPartnerById(businessPartnerId: string): Promise<BusinessPartner> {
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

getBusinessPartnerById(businessPartnerId: string): Promise<BusinessPartner> {
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

Note, that the properties of the business partner address entity are offered by the corresponding `BusinessPartnerAddress` class, so don't forget to import it.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Wire everything up)]

Now that you've finished implementing the function, you only need to call it in `business-partner.controller.ts`. Your controller should look like this:

```JavaScript / TypeScript
import { Controller, Get, Param } from '@nestjs/common';
import { BusinessPartnerService } from './business-partner.service';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

@Controller('business-partners')
export class BusinessPartnerController {
  constructor(private readonly businessPartnerService: BusinessPartnerService) {}

  @Get('/:businessPartnerId')
  getBusinessPartnerByID(@Param('businessPartnerId') businessPartnerId): Promise<BusinessPartner> {
    return this.businessPartnerService.getBusinessPartnerById(businessPartnerId)
  }
}
}
```

The function takes the ID from the URL and passes it to `getBusinessPartnerById` in the service class. If a business partner with the provided ID exists in the target system, it will be sent to the client. Otherwise, an error message is sent. Restart your server and open `http://localhost:3000/business-partners/1003764` to test it out! Of course you're free to use any other ID. Note that Nest application have a build in [exception filter](https://docs.nestjs.com/exception-filters), which maps exceptions to http messages shown to the client. By default the exceptions from the SDK are mapped to a server side error. So if you provide a non existing ID you will receive a 500 response and not a 404.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---
