---
title: Generate Custom OData Client Library with SAP Cloud SDK's Generator
description: Generate a custom OData client library for the SAP SuccessFactors Candidate service of the Recruiting module.
auto_validation: true
time: 15
tags: [tutorial>intermediate, topic>javascript, products>sap-cloud-platform, topic>odata]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Details
### You will learn
- What the SAP Cloud SDK offers for easily accessing OData services via its client libraries
- How to generate an OData client library for other services, like SuccessFactors services
- How to use your custom OData client library

The SAP Cloud SDK's OData Virtual Data Model is a set of client code libraries that abstract the SAP S/4HANA exposed OData services. It provides a fluent, type safe and exploratory way of building requests against S/4HANA. For more details, checkout our tutorial group on how to [build an address manager with the SAP Cloud SDK's OData Virtual Data Model](https://developers.sap.com/group.cloudsdk-js-vdm.html), where we covered this topic more extensively.

The goal of this tutorial is to generate your own client code of the same structure for a non-S/4HANA service at the example of a SuccessFactors service.

---

[ACCORDION-BEGIN [Step 1: ](Install the generator)]

If you are familiar with `npm` and have a suitable development setup, you should remember to add the SAP registry to your project because we will install the generator from this registry. Add the following line in your `.npmrc`:

```NPM
@sap:registry=https://npm.sap.com
```

If you are less experienced, we recommend taking a look at this tutorial group on [how to create an application using SAP Cloud SDK](https://developers.sap.com/group.s4sdk-js-cloud-foundry.html) up to the point where you scaffold an application. This will get you going with a basic setup.

Now you can just install our generator with the following command:

```Shell
npm install --save-dev @sap/cloud-sdk-generator
```

This will add the `@sap/cloud-sdk-generator` package to your `node_modules` and expose a command line interface for generation. The `--save-dev` indicates that this is a development dependency as opposed to a productive dependency, which should be sufficient for most use cases.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get familiar with non-S/4HANA OData service)]

Take a look at the [SuccessFactors recruiting service for candidates](https://api.sap.com/api/RCMCandidate/resource) on the `SAP API Business Hub` and get familiar with the entities and possible request structures. In the course of this tutorial we will generate an OData client library for this service and use it to build a query to retrieve data for the `Candidate` entity. Take a look at the capabilities around this entity and the service in general, so that you can compare your generated code later on.

![Get familiar with the service capabilities](service-overview.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Provide service specification files)]

Download the service specification file from the `SAP API Business Hub`. In the aforementioned `Candidate` service navigate to Details and press `Download Specification`. Choose EDMX to download a file called `RCMCandidate.edmx`. Don't close this page yet, because you will still need it in one of the next steps.

![Download service specification file](service-spec.png)

Create a directory `service-specifications` in your project and move the previously downloaded specification to this directory. We will use this directory as input directory for the generation. Feel free to add other specification files, too.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Run generation)]

The `@sap/cloud-sdk-generator` comes with a command line interface for generating an OData client library. To run the generation, execute the `generate-odata-client` command. Provide the previously created directory containing the specification files as input directory and specify a directory to output the client to:

```Shell
npx generate-odata-client --inputDir service-specifications --outputDir odata-client
```

This will leave you with the following comment:
> No service path could be determined from available metadata! To avoid this in the future, you can provide the correct value in "service-mapping.json". By default, the "service-mapping.json" file will be saved to and read from the input directory. You can supply a custom path using the `-s/--serviceMapping` flag.

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
npx generate-odata-client --inputDir service-specifications --outputDir odata-client --forceOverwrite
```

Now, there should be no warning.

Congratulations, you have generated the `sfo-data-service` module in your output directory! By default, the generated module contains the following sources:

  * TypeScript code (`.ts`)
  * Transpiled JavaScript code (`.js`)
  * Type definition files (`.d.ts`)
  * Source map files (`.js.map` and `.d.ts.map`)
  * `.npmrc`
  * `package.json`
  * `typedoc.json`
  * `tsconfig.json`

Depending on which of those files you need, you can skip the generation of most of those. To find out how and for further options, checkout `npx generate-odata-client --help`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Use OData client library to build request)]

[OPTION BEGIN [TypeScript]]

Time to test our custom OData client library. Let's build a request to retrieve first names and last names of all candidates that have applied to a position.

Create a source file `get-candidates.ts` in the `src` directory and import the `Candidate` entity from the `sfo-data-service` module.

```JavaScript / TypeScript
import { Candidate } from './odata-client/sfo-data-service';
```

Now use this entity to create a `getAll` request while selecting the properties `FIRST_NAME` and `LAST_NAME` and execute it against a destination. You can connect to the SAP API Business Hub SuccessFactors services by using the URL `https://sandbox.api.sap.com/successfactors`.

You will also need to provide your API Key. You can find it on the top of the SAP API Business Hub page. Simply click on `Show API Key` and then `Copy Key and Close`. Add the key as a custom header, using the `withCustomHeaders` function.

For further information on how to configure a destination check our previous tutorials on how to [create an app using SAP Cloud SDK for JavaScript](https://developers.sap.com/group.s4sdk-js-cloud-foundry.html) and on how to [build an application with the Virtual Data Model](https://developers.sap.com/group.cloudsdk-js-vdm.html).

This is what your file should look like:

```JavaScript / TypeScript
// get-candidates.ts

import { Candidate } from './odata-client/sfo-data-service';

export function getCandidates(): Promise<Candidate[]> {
  return Candidate.requestBuilder()
    .getAll()
    .top(20) // look at the top 20 candidates only
    .select(Candidate.FIRST_NAME, Candidate.LAST_NAME)
    .withCustomHeaders({
      apikey: '<YOUR-API-KEY>'
    })
    .execute({
      url: 'https://sandbox.api.sap.com/successfactors'
    });
}
```

Congratulations! You created an OData client library based on your own service specification and used it to create a `getAll` request!

[OPTION END]

[OPTION BEGIN [JavaScript]]

Time to test our custom OData client library. Let's build a request to retrieve first names and last names of all candidates that have applied to a position.

Create a source file `get-candidates.js` in the `src` directory and import the `Candidate` entity from the `sfo-data-service` module.

```JavaScript
const { Candidate } = require('./odata-client/sfo-data-service');
```

Now use this entity to create a `getAll` request while selecting the properties `FIRST_NAME` and `LAST_NAME` and execute it against a destination. You can connect to the SAP API Business Hub SuccessFactors services by using the URL `https://sandbox.api.sap.com/successfactors`.

You will also need to provide your API Key. You can find it on the top of the SAP API Business Hub page. Simply click on `Show API Key` and then `Copy Key and Close`. Add the key as a custom header, using the `withCustomHeaders` function.

For further information on how to configure a destination check our previous tutorials on how to [create an app using SAP Cloud SDK for JavaScript](https://developers.sap.com/group.s4sdk-js-cloud-foundry.html) and on how to [build an application with the Virtual Data Model](https://developers.sap.com/group.cloudsdk-js-vdm.html).

This is what your file should look like:

```JavaScript
// get-candidates.js

const { Candidate } = require('./odata-client/sfo-data-service');

function getCandidates() {
  return Candidate.requestBuilder()
    .getAll()
    .top(20) // look at the top 20 candidates only
    .select(Candidate.FIRST_NAME, Candidate.LAST_NAME)
    .withCustomHeaders({
      apikey: '<YOUR-API-KEY>'
    })
    .execute({
      url: 'https://sandbox.api.sap.com'
    });
}

module.exports.getCandidates = getCandidates;
```

Congratulations! You created an OData client library based on your own service specification and used it to create a `getAll` request!

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Integrate with Express.js application (optional))]

[OPTION BEGIN [TypeScript]]

Now to see this in action, if you are using the SAP Cloud SDK's scaffolding add a route callback to your `get-candidates.ts`:

```JavaScript / TypeScript
// get-candidates.ts
import { Request, Response } from 'express';
import { Candidate } from '../odata-client/sfo-data-service';

// call getCandidates() for the candidates route
export function candidatesRoute(req: Request, res: Response) {
  getCandidates().then((candidates: Candidate[]) => {
    res.status(200).send(candidates);
  });
}
```

Add a `/candidates` route to your application:

```JavaScript / TypeScript
// application.ts
  private routes(): void {
    const router = express.Router();

    ...
    // add this route
    router.get("/candidates", candidatesRoute);

    this.app.use("/", router);
  }
```

Run `npm run start:local` and go to `http://localhost:8080/candidates` in your browser. You should see a list of your candidates' first and last names.

[OPTION END]

[OPTION BEGIN [JavaScript]]

Now to see this in action, if you are using the SAP Cloud SDK's scaffolding add a route callback to your `get-candidates.js`:

```JavaScript
// get-candidates.js
const { Candidate } = require('./odata-client/sfo-data-service');

// call getCandidates() for the candidates route
function candidatesRoute(req, res) {
  getCandidates().then(candidates => {
    res.status(200).send(candidates);
  });
}

module.exports.getCandidates = getCandidates;
```

Add a `/candidates` route to your application:

```JavaScript
// application.js
  private routes() {
    const router = express.Router();

    ...
    // add this route
    router.get("/candidates", candidatesRoute);

    this.app.use("/", router);
  }
```

Run `npm run start:local` and go to `http://localhost:8080/candidates` in your browser. You should see a list of your candidates' first and last names.

[OPTION END]

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
