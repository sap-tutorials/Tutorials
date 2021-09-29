---
title: Create Your First Application with SAP Cloud SDK for JavaScript
description: Learn the fundamentals of the SAP Cloud SDK for JavaScript and integrate with an SAP S/4HANA Cloud system.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-sdk, topic>javascript ]
primary_tag: products>sap-cloud-sdk
---

## Details
### You will learn
 - How to extend a scaffolded application by another route
 - How to call the Business Partner Service of SAP S/4HANA Cloud using SAP Cloud SDK for JavaScript

---

[ACCORDION-BEGIN [Step 1: ](Set up the API Server)]

>**Note:** If you have access to an `SAP S/4HANA Cloud` system with a technical user, you can skip this part.

There are multiple ways of setting up an API Server, you can either setup your own Mock Server or you can use the Sandbox API.

In order to make a call to an `OData` service, there needs to be a service to call. You can setup a local mock server that mimics the business partner and a custom service by following the instructions [here](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html). This mock server does not support all the features of the actual `OData` services, but it suffices to try it out locally.

Once it is up and running you should see the list of services at `http://localhost:3000/`.

Alternatively, many APIs can also be tested using the sandbox of the SAP API Business Hub. To use the sandbox, you need an an API key. Go to [https://api.sap.com](https://api.sap.com) and click "Log On" in the top right corner. If you do not have an account, you will need to register first. When you are logged in, click on "Hi <your name>" in the top right corner and then click on "Preferences" in the dropdown menu that just opened. On the preferences page, click **Show API Key**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Add a custom route)]

Initially, the app only contains the `index` and `hello-world` routes. We will add another route for `business-parters` that will list all available business partners.

First, create a new file `business-partner.controller.ts` in the `src/` directory and add an implementation for this route, like so:

```JavaScript / TypeScript
import { Controller, Get } from '@nestjs/common';

@Controller()
export class BusinessPartnerController {
  @Get('business-partners')
  getBusinessPartners() {
    return 'We will implement this in a minute.';
  }
}
```

The `@Controller()` decorator marks our class as controller (i.e. a thing that handles requests), and the `@Get('business-partners')` decorator marks the `getBusinessPartners` method as handler for `GET` requests on the path `/business-partners`. The `@Controller()` decorator allows [providing more configuration](https://docs.nestjs.com/controllers), but this is beyond the scope of this tutorial.

In order for the controller to work, we also need to register it in our application. Open `app.module.ts`, import the controller class you just created and add it to the `controllers` declaration. Your file should look like this:

```JavaScript / TypeScript
import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { BusinessPartnerController } from './business-partner.controller';

@Module({
  imports: [],
  controllers: [AppController, BusinessPartnerController],
  providers: [AppService],
})
export class AppModule {}
```

If you've started your application the following command in the previous tutorial, it should detect the change and restart automatically.
```Shell
npm run start:dev
```
If you've terminated your application, you can restart it by running the start command again. Now, calling `http://localhost:8080/business-partners` should return our placeholder string.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Import service entities)]

In order to use the `SAP Cloud SDK for JavaScript` to make a call to an `OData` service, add the `virtual data model` (`VDM`) for this service to your dependencies. For this tutorial we are using the `VDM` for the business partner service. Install it with the following command:

```Shell
npm install @sap/cloud-sdk-vdm-business-partner-service
```

Import the entity you want to make a call to into your application. In this tutorial we are importing the business partner entity of the business partner service. Add the following line to the top of the `business-partner.controller.ts`.

```JavaScript / TypeScript
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';
```

Now the `BusinessPartner` entity is available for you to be used.

>**Side-note:** The `SAP Cloud SDK for JavaScript` offers packages for each `OData` service exposed by `SAP S/4HANA Cloud`. You can find a list of these services in the [`SAP API Business Hub`](https://api.sap.com/package/SAPS4HANACloud?section=Artifacts) and a list of the corresponding packages in our [documentation](https://help.sap.com/doc/9dbcab0600b346c2b359a8c8978a45ba/1.0/en-US/index.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Execute an OData request)]

In `business-partner.controller.ts` create a function `getAllBusinessPartners` and implement it depending on your API Server:


[OPTION BEGIN [Mock Server]]
In the code snippet below, we assume that you have a mock server running locally on port 3000. Documentation on the mock server can be found [here](https://sap.github.io/cloud-s4-sdk-book/pages/mock-odata.html).

```JavaScript / TypeScript
function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      url: 'http://localhost:3000'
    });
}
```
[OPTION END]


[OPTION BEGIN [SAP S/4HANA Cloud system]]
If you are using an actual `SAP S/4HANA Cloud` system, you can replace the fourth line with a different destination configuration:
```JavaScript / TypeScript
function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      url: '<URI of your SAP S/4HANA Cloud System>',
      username: '<USERNAME>',
      password: '<PASSWORD>'
    });
}
```
[OPTION END]


[OPTION BEGIN [SAP API Business Hub sandbox]]
To use the SAP API Business Hub sandbox for your requests, you will need to pass the API key to the VDM requests using the `withCustomHeaders` method, and you will need to add the correct URL to your destinations. Checkout the following example:

```JavaScript / TypeScript
function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .withCustomHeaders({ APIKey: '<YOUR-API-KEY>'})
    .execute({
      url: 'https://sandbox.api.sap.com/s4hanacloud'
    });
}
```
[OPTION END]

- In line 2, we are creating a request builder for the business partner entity.
- Line 3 indicates, that we want to create a request to get all the business partners.
- Line 4 takes care of the execution and sends a request to an URL based on the given destination `url`.

-------------------

As network requests are asynchronous by nature, the return value of this function is a [Promise](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) to a list of Business Partners (`Promise<BusinessPartner[]>`).

Let's add the execution of this request to `getBusinessPartners` method:

```JavaScript / TypeScript
import { Controller, Get, HttpException } from '@nestjs/common';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

@Get('business-partners')
getBusinessPartners() {
  return getAllBusinessPartners()
    .catch(error => {
      throw new HttpException(`Failed to get business partners - ${error.message}`, 500);
    });
}
```

`nest.js` will handle the `Promise` we return automatically. We add a `.catch` handler to specify how errors are handled (otherwise it would only show "internal server error" when something goes wrong). Here is what your `business-partner.constroller.ts` should look like, if you are using the mock server:

```JavaScript / TypeScript
import { Controller, Get, HttpException } from '@nestjs/common';
import { BusinessPartner } from '@sap/cloud-sdk-vdm-business-partner-service';

@Controller()
export class BusinessPartnerController {
  @Get('business-partners')
  getBusinessPartners() {
    return getAllBusinessPartners()
      .catch(error => {
        throw new HttpException(`Failed to get business partners - ${error.message}`, 500);
      });
  }
}

function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      url: 'http://localhost:3000',
    });
}
```

Reload the `http://localhost:8080/business-partners` ` url`  to retrieve a list of business partners.

Congratulations, you just made your first call with the SAP Cloud SDK!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Manage destinations centrally (optional))]

In order to not repeat your destination configuration for every request execution, you can set a `destinations` environment variable to manage your destinations. In `Node.js` application, it is common to use a `.env` file to maintain such environment variables for a given project. Create a `.env` file in the root directory of your project and define the `destinations` environment variable as follows:

```
destinations=[{"name": "<DESTINATIONNAME>", "url": "<URL to your system>", "username": "<USERNAME>", "password": "<PASSWORD>"}]
```

This is what it would look like for the mock server:

```
destinations=[{"name": "MockServer", "url": "http://localhost:3000"}]
```

Now that we have defined our destinations, we need to make sure that they are available in our process. For this we use the `config` package provided by `nest.js`. You can install it with the following command:

```
npm install @nestjs/config
```

In order to load the environment variables defined in the `.env` file, we need to add the `ConfigModule` provided by the `config` package to the application's `@Module` definition. Open `app.module.ts` and update it with the following code:

```JavaScript / TypeScript
import { Module } from '@nestjs/common';
import { ConfigModule } from '@nestjs/config';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { BusinessPartnerController } from './business-partner.controller';

@Module({
  imports: [ConfigModule.forRoot()],
  controllers: [AppController, BusinessPartnerController],
  providers: [AppService],
})
export class AppModule {}
```

In line 2, `ConfigModule` is imported from the `config` package and in line 8 we add it to the module's `imports`. If no arguments are passed to the `forRoot()` method, the `.env` file has to be located in the project root. For details on the possible configuration see the [nest documentation](https://docs.nestjs.com/techniques/configuration).  To reference a destination in the request execution, simply replace the `url` with a `destinationName` - `MockServer` in our example:

```JavaScript / TypeScript
function getAllBusinessPartners(): Promise<BusinessPartner[]> {
  return BusinessPartner.requestBuilder()
    .getAll()
    .execute({
      destinationName: 'MockServer'
    });
}
```

Note, that every environment variable in the `.env` file has to be defined *on one line*. You can add more destinations to the array.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---
