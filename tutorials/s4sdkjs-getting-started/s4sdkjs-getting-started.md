---
title: Get Started with SAP Cloud SDK for JavaScript
description: Scaffold an application that is ready to be used with the SAP Cloud SDK for JavaScript.
auto_validation: true
time: 10
tags: [tutorial>beginner, products>sap-s-4hana-cloud-sdk, topic>javascript]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Details

### You will learn

- How to scaffold (create from a template) your application
- About the project's structure
- How to run the application locally

---

[ACCORDION-BEGIN [Step 1: ](Scaffold an application)]

To create an `express.js` ([a minimal Node.js web application framework](https://expressjs.com/)) application that already contains all the files and configuration you need to use the SAP S/4HANA Cloud SDK for JavaScript, simply clone our TypeScript scaffolding application as follows:

```Shell
git clone --single-branch --branch scaffolding-ts --origin scaffolding https://github.com/SAP/cloud-s4-sdk-examples.git <path/to/your/project>
```

Then enter the freshly cloned project:

```Shell
cd <path/to/your/project>
```

### Alternative

If you cannot or do not want to use `TypeScript`, you can also checkout the `JavaScript` version. The tutorial will be based on `TypeScript`, but is easily applicable to `JavaScript`. The main differences you will notice are the type annotations and module definitions - ES6 modules in the `TypeScript` version vs. `commonJS` modules in the `JavaScript` version.

```Shell
# clone the JavaScript version of the scaffolding above
git clone --single-branch --branch scaffolding-js --origin scaffold https://github.com/SAP/cloud-s4-sdk-examples.git <path/to/your/project>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get familiar with the project)]

The project contains the following files and folders, among others, to get you started with the `SAP Cloud SDK for JavaScript`:

### NPM / Project

- **`package.json`**: Specifies dependencies, metadata and user-defined scripts. The provided scaffolding comes with some predefined scripts and dependencies, that will be explained in detail in the course of this group of tutorials. The scripts starting with `ci-` are used by the Continuous Delivery pipeline. We recommend not changing them for
- **`.npmrc`**: The **`npm`** configuration file. In the scaffolding we specify the registry for the `@sap` scope, where the `SAP S/4HANA Cloud SDK` libraries are published. The registry for the SAP Cloud SDK for JavaScript is `https://npm.sap.com`. Please always double-check the registries specified in `.npmrc` file that you have not created yourself!

### TypeScript

- **`tsconfig.json`**: Configuration file for `TypeScript`. This is not present in the plain `JavaScript` version.

### Continuous Delivery

- **`Jenkinsfile`**: Jenkins pipeline definition file for quality assurance. It uses the [`SAP Cloud SDK's Continuous Delivery Toolkit`](https://github.com/SAP/cloud-s4-sdk-pipeline).
- **`pipeline_config.yml`**: Pipeline configuration file for the Jenkins pipeline.
- **`cx-server/`**: A directory containing scripts to quickly deploy and start your own CI / CD server based on Jenkins.

### Cloud Foundry

- **`manifest.yml`**: The deployment descriptor file for `Cloud Foundry in SAP Cloud Platform`.

### Local development

- **`initialize.js`**: Script for initial setup.

* **`src/`**: Source code for an initial hello world express application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the application)]

Before you can run the app, you should run the initialization script. This will install your dependencies and replace some placeholders with your application name. This is only necessary once at the start of development. Feel free to delete this file afterwards.

```Shell
npm run init -- <YOUR-APPLICATION-NAME>
```

Now, you are all set to start the application, run:

```Shell
npm run start:local
```

Go to `http://localhost:8080/hello` and you should get a 'Hello, World!' in response.
To stop the server again, press `ctrl + c` in your command line.
Alternatively, you also use `watch:local` instead of `run:local`.
This will run the server and will automatically restart and update it whenever you change some of the source files.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Use the SDK in existing project (optional))]

If you already have an existing project, you will need to specify the registry for the `@sap` scope, in order to make the `SAP Cloud SDK for JavaScript` libraries available. If it does not yet exist create a `.npmrc` file in the root folder of your project. Paste the following line into it:

```Shell
@sap:registry=https://npm.sap.com
```

**Note:** Please be cautious when adding registries to your `npm` configuration and make sure that you trust the registry!
Now you can install the necessary libraries, first of all the `@sap/cloud-sdk-core`, the heart of the `SAP Cloud SDK for JavaScript` and basis for the service libraries you might want to use.

```Shell
npm install @sap/cloud-sdk-core
```

We recommend to also take a look at the continuous delivery artifacts in the scaffold application and adopt those along with the respective **`npm`** scripts.

[DONE]
[ACCORDION-END]

That's it! You should now have a running application that is ready to be integrated with `SAP S/4HANA Cloud`.

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---

This tutorial is part of a larger series.
You can find the next entry [here](Create Your First Application with SAP Cloud SDK for JavaScript).
For questions, you can reach out to us on [`StackOverflow`](https://stackoverflow.com/) using the tag [sap-cloud-sdk](https://stackoverflow.com/questions/tagged/sap-cloud-sdk) and on [answers.sap.com](https://answers.sap.com) using the tag [SAP S/4HANA Cloud SDK](https://answers.sap.com/tags/73555000100800000895).
