---
title: Delete OData Entities with the SAP Cloud SDK's Virtual Data Model
description: Delete OData entities with the SAP Cloud SDK's virtual data model to duild an address manager application.
auto_validation: true
time: 15
tags: [ tutorial>intermediate, products>sap-business-technology-platform, topic>javascript, topic>odata]
primary_tag: products>sap-cloud-sdk
---

## Prerequisites
 - Have `Node.js` and `npm` [installed on your machine](s4sdkjs-prerequisites).
 - Have access to an SAP S/4HANA Cloud system or the [SAP API Business Hub Sandbox](https://api.sap.com/getting-started), or use the [Business Partner Mock Service](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html).
 - Basic knowledge of OData is recommended, but not required.

## Details
### You will learn
  - How to use the Virtual Data Model to delete an entity
  - How to trigger a delete request from an API endpoint exposed by your application

The goal of this tutorial group is to show you how to implement a JavaScript application that allows you to manage the addresses of business partners. This application will be using `NestJS` and the SAP Cloud SDK for JavaScript. In this tutorial, we use the SAP Cloud SDK's OData Virtual Data Model to delete an address and make this functionality available via an API endpoint.

---

[ACCORDION-BEGIN [Step 1: ](Add an API endpoint)]

As in the [previous tutorial](cloudsdk-js-vdm-update) we have to create a controller to expose the endpoint for deleting an address. Note: If you have already controller and service classes from the previous tutorials you can of course keep the existing files and just extend the classes by the new methods. Create a `business-partner.controller.ts` and add the following implementation:

```JavaScript / TypeScript
import { Controller, Param, HttpCode, Delete } from '@nestjs/common';

@Controller('business-partners')
export class BusinessPartnerController {
  @Delete('/:businessPartnerId/address/:addressId')
  @HttpCode(204)
  deleteBusinessPartnerAddress(@Param('businessPartnerId') businessPartnerId, @Param('addressId') addressId){
    console.log(`Your request parameters are businessPartnerId:${businessPartnerId} and addressId:${addressId}.`);
  }
}
```
The `deleteBusinessPartnerAddress` does not do anything useful yet, but you will implement it in the next chapter. We will do the implementation in a separate `business-partner.service.ts` file, which we will prepare already now:

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';

@Injectable()
export class BusinessPartnerService {
  deleteBusinessPartnerAddress(businessPartnerId: string, addressId: string): Promise<void> {
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

Note, that we used the `DELETE` method in the controller, so we need to send a `DELETE` request to trigger this controller method. Restart your server and send a `DELETE` request to `http://localhost:3000/business-partners/1/address/2`. The server should respond with  status code `204 - No content` and in the console of your local server you should see:
`Your request parameters are businessPartnerId:1 and addressId:2.`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Delete a business partner address)]

Next, we use the VDM to delete a business partner address. Open `business-partner.service.ts` and overwrite `deleteBusinessPartnerAddress` as shown below:

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';
import {BusinessPartnerAddress} from '@sap/cloud-sdk-vdm-business-partner-service';

@Injectable()
export class BusinessPartnerService {
  deleteBusinessPartnerAddress(businessPartnerId: string, addressId: string): Promise<void> {
    return BusinessPartnerAddress.requestBuilder()
    .delete(businessPartnerId, addressId)
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
  }
}
```

As of version `1.7.0` of the SAP Cloud SDK, you can alternatively pass an entity to the `delete` method instead of the entity's key fields. This has the advantage that the version identifier of the entity is set automatically. When going with the first approach, you would have to manually supply the version identifier using the `setVersionIdentifier` method. Therefore, if you already have an instance of the entity you want to delete, passing it directly is the easier approach.

Be aware that executing a delete request will return a `Promise<void>`. In order to expose the delete method to the client, we need to adjust the controller:

```JavaScript / TypeScript
import { Controller, Param, HttpCode, Delete } from '@nestjs/common';
import { BusinessPartnerService } from './business-partner.service';

@Controller('business-partners')
export class BusinessPartnerController {
  constructor(private readonly businessPartnerService: BusinessPartnerService) {}

  @Delete('/:businessPartnerId/address/:addressId')
  @HttpCode(204)
  deleteBusinessPartnerAddress(@Param('businessPartnerId') businessPartnerId, @Param('addressId') addressId): Promise<void> {
    return this.businessPartnerService.deleteBusinessPartnerAddress(businessPartnerId, addressId);
  }
}
```

Restart your server and send a delete request to `http://localhost:3000/business-partners/1003764/address/28238` (or the respective IDs you used in the previous tutorials). If everything works, you should see an empty response with status code 204 and your address should disappear from the backend.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---
