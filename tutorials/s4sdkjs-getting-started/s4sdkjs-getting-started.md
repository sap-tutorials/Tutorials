---
title: Get Started with SAP Cloud SDK for JavaScript
description: Scaffold an application that is ready to be used with the SAP Cloud SDK for JavaScript.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-s-4hana-cloud-sdk, topic>javascript ]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Details

### You will learn

 - How to scaffold your application
 - About the project's structure
 - How to run the application locally

---

[ACCORDION-BEGIN [Step 1: ](Scaffold an application)]

Personally, we are fans of `TypeScript` and recommend using it for most applications. However, if you prefer using plain `JavaScript`, we recommend looking at step 4. If a step in a tutorial contains instructions or code that is specific to `TypeScript` or `JavaScript`, you can toggle between whatever you prefer at the top of each step.

The main differences you will notice between `TypeScript` and `JavaScript` are the type annotations and module definitions - ES6 modules in `TypeScript` vs. `commonJS` modules in `JavaScript`. To migrate a `TypeScript` file to `JavaScript`, you only need to change the file extension from `.ts` to `.js`, remove all type annotations and change the `import`s and `export`s.

To create an application that already contains all the files and configuration you need to use the SAP Cloud SDK for JavaScript, you can use SDK's command line interface (CLI). To get the CLI, run the following command:

```Shell
npm install -g @sap-cloud-sdk/cli
```

This will install the CLI globally on your machine, allowing you to use it anywhere.
Now you can create a new project by running the CLI's `init` command:

```Shell
sap-cloud-sdk init my-sdk-project
```

If your folder is empty, this will create a new `nest.js` application from scratch. During the process, the CLI will ask to you provide some details, like your project's name. Since this will already install all the necessary dependencies, this might take a minute. If everything worked correctly, you should see output like this:

```Shell
+---------------------------------------------------------------+
| ✅ Init finished successfully.                                |
|                                                               |
| 🚀 Next steps:                                                |
| - Run the application locally (`npm run start:dev`)           |
| - Deploy your application (`npm run deploy`)                  |
|                                                               |
| 🔨 Consider setting up Jenkins to continuously build your app |
| Use `sap-cloud-sdk add-cx-server` to create the setup script  |
+---------------------------------------------------------------+
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Get familiar with the project)]

The project contains the following files and folders, among others, to get you started with the `SAP Cloud SDK for JavaScript`:

### NPM / Project

- **`package.json`**: Specifies dependencies, metadata and user-defined scripts. The application comes with some predefined scripts and dependencies, that will be explained in detail in the course of this group of tutorials.
- **`.npmrc`**: The **`npm`** configuration file. In the scaffolding we specify the registry for the `@sap` scope, where the `SAP Cloud SDK` libraries are published.

### TypeScript

- **`tsconfig.json`**: Configuration file for `TypeScript`. This is not present in the plain `JavaScript` version.
- **`tslint.json`**: Configuration file for `tslint`, the de facto default linter for `TypeScript`.

### Continuous Delivery

- **`Jenkinsfile`**: Jenkins pipeline definition file for quality assurance. It uses the [`SAP Cloud SDK's Continuous Delivery Toolkit`](https://github.com/SAP/cloud-s4-sdk-pipeline).
- **`pipeline_config.yml`**: Pipeline configuration file for the Jenkins pipeline.
- **`cx-server/`**: A directory containing scripts to quickly deploy and start your own CI / CD server based on Jenkins.

### Cloud Foundry

- **`manifest.yml`**: The deployment descriptor file for `Cloud Foundry in SAP Cloud Platform`.

### Local development

- **`initialize.js`**: Script for initial setup.
- **`src/`**: Source code for the initial application.

### SDK specific

- **`systems.json`+`credentials.json`**: Allows you to maintain destinations for testing purposes.
- **`sap-cloud-sdk-analytics.json`**: Only if you have agreed to usage analytics during the initialization of your project. You can find more information about anonymous usage analytics [in the CLI's repository](https://github.com/SAP/cloud-sdk-cli/blob/master/usage-analytics.md).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run the application)]

To run the application locally, simply run the following command:

```Shell
npm run start:dev
```

Go to `http://localhost:3000` and you should get a 'Hello, World!' in response. Before continuing with the next tutorial, open `src/main.ts` and switch the port from `3000` to `8080`. The corresponding line should then look like this:

```JavaScript/TypeScript
await app.listen(process.env.PORT || 8080);
```

Since `nest` was started in watch mode, it should detect this change and restart the server.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Use the SDK in an existing project (optional))]

If you already have an existing project that you want to start using the SAP Cloud SDK for JavaScript in, the SDK's command line interface (CLI) allows you to add the necessary configuration in a single step. If you haven't done so already, start by installing the CLI with the following command:

```Shell
npm install -g @sap-cloud-sdk/cli
```

Then navigate to your project and execute the CLI's `init` command:

```Shell
cd <path/to/your/project>
sap-cloud-sdk init
```

Most importantly, the will add the necessary dependencies to your project, along with a set of scripts that allow you to use the [`SAP Cloud SDK's Continuous Delivery Toolkit`](https://github.com/SAP/cloud-s4-sdk-pipeline).

[DONE]
[ACCORDION-END]

That's it! You should now have a running application that is ready to be integrated with `SAP S/4HANA Cloud`.

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---
