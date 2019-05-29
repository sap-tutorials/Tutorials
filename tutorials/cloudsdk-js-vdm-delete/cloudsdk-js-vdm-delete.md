---
title: Delete OData Entities with the SAP Cloud SDK's Virtual Data Model
description: Delete OData Entities with the SAP Cloud SDK's Virtual Data Model to Build an Address Manager Application
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
  - How to use the Virtual Data Model to delete an entity
  - How to trigger a delete request from an API endpoint exposed by your application

The goal of this tutorial group is to show you how to implement a JavaScript application that allows you to manage the addresses of business partners. This application will be using `Express.js` and the SAP Cloud SDK for JavaScript. In this tutorial, we use the SAP Cloud SDK's OData Virtual Data Model to delete an address and make this functionality available via an API endpoint.

---

[ACCORDION-BEGIN [Step 1: ](Add an API Endpoint)]

Start by creating a new file called `delete-business-partner-address-route.ts` and copy the following code into it:

```JavaScript / TypeScript
import { Request, Response } from 'express';
import { BusinessPartnerAddress } from '@sap/cloud-sdk-vdm-business-partner-service';

export function deleteBusinessPartnerAddressRoute(req: Request, res: Response) {
  deleteBusinessPartnerAddress(req.params.id, req.params.addressId)
    .then(() => {
      res.status(200).send('Entity successfully deleted!');
    })
    .catch(error => {
      res.status(500).send(error.message);
    })
}

function deleteBusinessPartnerAddress(businessPartnerId: string, addressId: string): Promise<void> {
  return Promise.resolve();
}
```

This follows the implementation in the previous tutorials. `deleteBusinessPartnerAddress` does not do anything useful yet, but you will implement it in the next step. Now open `application.ts`, import the function and add the following route definition:

```JavaScript / TypeScript
import { businessPartnerRoute } from './business-partner-route';
import { singleBusinessPartnerRoute } from './single-business-partner-route';
import { createBusinessPartnerAddressRoute } from './create-business-partner-address-route';
import { updateBusinessPartnerAddressRoute } from './update-business-partner-address-route';
import { deleteBusinessPartnerAddressRoute } from './delete-business-partner-address-route';

// ...

private routes(): void {
  const router = express.Router();

  router.get("/", indexRoute);
  router.get("/hello", helloWorld);
  router.get("/business-partners", businessPartnerRoute);
  router.get("/business-partners/:id", singleBusinessPartnerRoute);
  router.post("/business-partners/:id/address", createBusinessPartnerAddressRoute);
  router.put("/business-partners/:id/address/:addressId", updateBusinessPartnerAddressRoute)
  // add the following line
  router.delete("/business-partners/:id/address/:addressId", deleteBusinessPartnerAddressRoute)
  this.app.use("/", router);
}
```

Note, that we used `router.delete` for this route, so we need to send a `DELETE` request. Restart your server and send a `DELETE` request to `http://localhost:8080/business-partners/1/address/2`. The server should respond with `"Entity successfully deleted!"`, since `deleteBusinessPartnerAddress` returns `Promise.resolve()` and will therefore never fail.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Delete a Business Partner Address)]

Next, we use the VDM to delete a business partner address. Open `delete-business-partner-address-route.ts` and overwrite `delete-business-partner-address` as shown below:

```JavaScript / TypeScript
function deleteBusinessPartnerAddress(businessPartnerId: string, addressId: string): Promise<void> {
  return BusinessPartnerAddress.requestBuilder()
    .delete(businessPartnerId, addressId)
    .execute({
      url: 'https://my.s4hana.ondemand.com/'
    });
}
```

Compared to the other requests, there are some differences. First, the `delete` function does not take an instance of the entity, but directly expects the entity's key fields as parameters. Secondly, the `delete` function is the only one that does not return a promise of the respective entity, but instead a `Promise<void>`. Be aware that should the service require a version identifier for the delete request, you will have to explicitly set it using the `setVersionIdentifier` function.

Restart your server and send a delete request to `http://localhost:8080/business-partners/1/address/1` (or the respective IDs you used in the previous tutorials). If everything works, you should see "Entity successfully deleted!" in the response body!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---
