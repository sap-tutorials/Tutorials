---
title: Get Set to Use SAP Cloud SDK for JavaScript
description: Set up your environment to use SAP Cloud SDK for JavaScript.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-sdk, topic>javascript ]
primary_tag: products>sap-cloud-sdk
---

## Details

### You will learn

- How to install **`Node.js`**,a JavaScript runtime for executing JavaScript code outside of a browser
- How to install **`npm`**, the node package manager for maintaining Node.js projects and their dependencies
- How to install **`cf` CLI**, the command line interface for interacting with Cloud Foundry

---

[ACCORDION-BEGIN [Step 1: ](Node.js and npm)]

You should have **`Node.js`** and **`npm`** installed. To see whether you already have **`Node.js`** and **`npm`** installed and check the installed version, run the following commands on the command line:

```Shell
node -v
npm -v
```

If one of those commands fails because the command was not found you will have to install **`Node.js`**. We recommend using at least **`Node.js`** version `11.0.0` and **`npm`** version `6.0.0`. The latest LTS version (10.15.3 as of today), will also work. If you have **`Node.js`** and **`npm`** installed at a current version, skip ahead and mark this step as done.

### Install Node.js and npm

We recommend to install **`Node.js`** using a [`package manager`](https://nodejs.org/en/download/package-manager) or a node version manager suitable for your platform. You are free to use an [`installer`](https://nodejs.org/en/download), though. **`npm`** will automatically be installed together with **`Node.js`**. **`npm`** can be updated by running `npm install -g npm` from your command line.

<!-- TODO: Maybe mention here that for Windows the installer is the easiest way to setup Node. -->

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](SAP Cloud Platform account)]

For deploying your application to `Cloud Foundry in SAP Cloud Platform`, you will need an account. You can get a trial account [here](https://cloudplatform.sap.com/index.html) by clicking on "Start your free trial" or use an existing account, if you already have one.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Cloud Foundry command line interface)]

You will need the `Cloud Foundry` command line interface (`cf` CLI) to later deploy your application to SAP Cloud Platform. To see whether it is already installed, you can run `cf -v` on your command line. If the command fails, you will need to install it.

You can find installation instructions for all common platforms in the [`Cloud Foundry documentation`](https://docs.cloudfoundry.org/cf-cli/install-go-cli.html). Again, we recommend to use a `package manager` for that. If you are using `chocolatey` on Windows, please find the instructions [here](https://chocolatey.org/packages/cloudfoundry-cli).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Editor / IDE)]

Feel free to choose your favorite `IDE`. We recommend using [`Visual Studio Code`](https://code.visualstudio.com) for its excellent JavaScript and TypeScript support. However, this tutorial does not rely on any specific editor.

[DONE]
[ACCORDION-END]

That's it, you are now ready to start developing your own application with the `SAP Cloud SDK for JavaScript`!

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---

For questions, you can reach out to us on [`StackOverflow`](https://stackoverflow.com/) using the tag [sap-cloud-sdk](https://stackoverflow.com/questions/tagged/sap-cloud-sdk) and on [answers.sap.com](https://answers.sap.com) using the tag [SAP Cloud SDK](https://answers.sap.com/tags/73555000100800000895).
