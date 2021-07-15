---
title: Generate Custom OData Client Library with SAP Cloud SDK's Generator for JavaScript
description: Generate a custom OData client library for the SAP SuccessFactors Candidate service of the Recruiting module.
auto_validation: true
time: 15
tags: [tutorial>intermediate, topic>javascript, products>sap-business-technology-platform, topic>odata]
primary_tag: products>sap-cloud-sdk
---

## Details
### You will learn
- What the SAP Cloud SDK offers for easily accessing OData services via its client libraries
- How to generate an OData client library for other services, like SuccessFactors services
- How to use your custom OData client library

The SAP Cloud SDK's OData Virtual Data Model is a set of client code libraries that abstract the SAP S/4HANA exposed OData services. It provides a fluent, type safe and exploratory way of building requests against S/4HANA. For more details, checkout our tutorial group on how to [build an address manager with the SAP Cloud SDK's OData Virtual Data Model](group.cloudsdk-js-vdm), where we covered this topic more extensively.

The goal of this tutorial is to generate your own client code of the same structure for a non-S/4HANA service at the example of a SuccessFactors service.

---

[ACCORDION-BEGIN [Step 1: ](Install the generator)]

The generator is part of the [SAP cloud-sdk-cli](https://github.com/SAP/cloud-sdk-cli). This command line interface provides everything to quickly generate a web project using the SDK. We have a [getting started tutorial](s4sdkjs-getting-started), where we show that. If you have not already done it install the CLI globally using `npm`:

```Shell
npm install -g @sap-cloud-sdk/cli
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get familiar with non-S/4HANA OData service)]

Take a look at the [SuccessFactors recruiting service for candidates](https://api.sap.com/api/RCMCandidate/resource) on the SAP API Business Hub and get familiar with the entities and possible request structures. In the course of this tutorial we will generate an OData client library for this service and use it to build a query to retrieve data for the `Candidate` entity. Take a look at the capabilities around this entity and the service in general, so that you can compare your generated code later on.

![Get familiar with the service capabilities](service-overview.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Provide service specification files)]

Download the service specification file from the SAP API Business Hub. In the aforementioned `Candidate` service navigate to Details and press `Download Specification`. Choose EDMX to download a file called `RCMCandidate.edmx`. Don't close this page yet, because you will still need it in one of the next steps.

![Download service specification file](service-spec.png)

Create a directory `service-specifications` in your project and move the previously downloaded specification to this directory. We will use this directory as input directory for the generation. Feel free to add other specification files, too.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run generation)]

We will use the cloud-sdk-cli now to generate the OData client from the service specification. To find the right command, just run

```Bash
sap-cloud-sdk --help
```
We want to generate an OData client so `generate-odata-client` is the right command for us. Run `sap-cloud-sdk generate-odata-client --help` to see how the command is used.

Only the input and output directory are mandatory:

```Bash
sap-cloud-sdk generate-odata-client -i service-specification/ -o odata-client/
```

Note that the generator relies on the `@sap-cloud-sdk/generator` package. If you have not installed this, on the first run a prompt will appear asking you to install it.

After the generation is finished you should find in the `odata-client` folder multiple generated classes. But also in the `service-specification` folder a new file has been created: `serviceMapping.json`. The file should contain the following content:

```json
{
  "RCMCandidate": {
    "directoryName": "sfo-data-service",
    "servicePath": "VALUE_IS_UNDEFINED",
    "npmPackageName": "sfo-data-service"
  }
}
```

Some service specifications do not specify a service path. The service path is the part of the URL that points to a service. As we do not want to assume any paths, the value will therefore be set to `"VALUE_IS_UNDEFINED"` and has to be replaced manually. You can add a service path in the `service.mapping.json` that was created in your input directory.

Replace the value for `servicePath` with the one for your service. The SAP SuccessFactors services that are available on the SAP API Business Hub have the service path `'/odata/v2'`.

```JSON
{
  "RCMCandidate": {
    "directoryName": "sfo-data-service",
    "servicePath": "/odata/v2",
    "npmPackageName": "sfo-data-service"
  }
}
```

 Now rerun the generation, but make sure to add the `--forceOverwrite` flag to overwrite the previously generated client.

```Shell
sap-cloud-sdk generate-odata-client -i service-specification/ -o odata-client/ --forceOverwrite
```

Congratulations, you have generated the `sfo-data-service` module in your output directory!
By default, the generated module contains the following sources:

  * TypeScript code (`.ts`)
  * Transpiled JavaScript code (`.js`)
  * Type definition files (`.d.ts`)
  * Source map files (`.js.map` and `.d.ts.map`)
  * `.npmrc`
  * `package.json`
  * `typedoc.json`
  * `tsconfig.json`

Depending on which of those files you need, you can skip the generation of most of those. To find out how and for further options, checkout `sap-cloud-sdk generate-odata-client --help`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Use OData client library to build request)]

Time to test our custom OData client library. Let's build a request to retrieve first names and last names of all candidates that have applied to a position. If you have already a `NestJS` tutorial application from a previous tutorial you can extend it or you create a fresh one as described in the [getting started tutorial](s4sdkjs-getting-started).

When you have your `NestJS` scaffold, create a source file `candidates.service.ts` in the `src` directory and import the `Candidate` entity from the `sfo-data-service` module.

```JavaScript / TypeScript
import { Candidate } from 'odata-client/sfo-data-service';
```

Now use this entity to create a `getAll` request while selecting the properties `FIRST_NAME` and `LAST_NAME` and execute it against a destination. You can connect to the SAP API Business Hub SuccessFactors services by using the URL `https://sandbox.api.sap.com/successfactors`.

You will also need to provide your API Key. You can find it on the top of the SAP API Business Hub page. Simply click on `Show API Key` and then `Copy Key and Close`. Add the key as a custom header, using the `withCustomHeaders` function.

For further information on how to configure a destination check our previous tutorials on how to [create an app using SAP Cloud SDK for JavaScript](group.s4sdk-js-cloud-foundry) and on how to [build an application with the Virtual Data Model](group.cloudsdk-js-vdm).

This is what your `candidate.service.ts` file should look like:

```JavaScript / TypeScript
import { Injectable } from '@nestjs/common';
import { Candidate } from 'odata-client/sfo-data-service';

@Injectable()
export class CandidateService {
  getCandidates(): Promise<Candidate[]> {
    return Candidate.requestBuilder()
      .getAll()
      .top(20)
      .select(Candidate.FIRST_NAME, Candidate.LAST_NAME)
      .withCustomHeaders({
        apikey: '<YOUR-API-KEY>'
      })
      .execute({
        url: 'https://sandbox.api.sap.com/successfactors'
      });
  }
}
```

Congratulations! You created an OData client library based on your own service specification and used it to create a `getAll` request!

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Integrate with NestJS application (optional))]

Now to see this in action, we need to create a `controller` and adjust the `module` to load the new functionality. In a [previous tutorial](cloudsdk-js-vdm-getall) we explained a bit the basics about `controller`, `service` and `module` of `NestJS` applications. For a more general overview we strongly recommend the [Nest tutorials](https://docs.nestjs.com/first-steps). Create a `candidates.controller.ts` and copy the following code there:

```JavaScript / TypeScript
import { Controller, Param, Get } from '@nestjs/common';
import { CandidateService } from './candidate.service';
import { Candidate } from 'odata-client/sfo-data-service';

@Controller('candidates')
export class CandidateController {
  constructor(private readonly candidatesService: CandidateService) {}

  @Get()
  getCandidates(): Promise<Candidate[]> {
    return this.candidatesService.getCandidates();
  }
}
```

And register the new service and controller in the `app.module.ts`:

```JavaScript / TypeScript
import { Module } from '@nestjs/common';
import { AppController } from './app.controller';
import { AppService } from './app.service';
import { CandidateService } from './candidate.service';
import { CandidateController } from './candidate.controller';
@Module({
  imports: [],
  controllers: [AppController, CandidateController],
  providers: [AppService, CandidateService]
})
export class AppModule {}
```

Run `npm start` and go to `http://localhost:3000/candidates` in your browser. You should see a list of your candidates' first and last names.

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
