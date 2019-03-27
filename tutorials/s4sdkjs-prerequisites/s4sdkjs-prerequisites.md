---
title: Prerequisites for using the SAP S/4HANA Cloud SDK for JavaScript for Cloud Foundry
description: In this tutorial, we will guide you through the steps necessary to use the SAP S/4HANA Cloud SDK for JavaScript productively.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-s-4hana-cloud-sdk-js]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Details
### After completing this tutorial, you will have:
 - Installed **`Node.js`** and **`npm`**
 - Installed **`cf` CLI**

---

[ACCORDION-BEGIN [Step 1: ](Install Node.js and npm)]

You should have **`Node.js`** and **`npm`** installed. To see whether you already have **`Node.js`** and **`npm`** installed and check the installed version, run the following commands on the command line:

```Shell
node -v
npm -v
```

If one of those commands fails because the command was not found you will have to install **`Node.js`**. We recommend using at least **`Node.js`** version `11.0.0` and **`npm`** version `6.0.0`. If you have **`Node.js`** and **`npm`** installed at a current version, skip ahead and mark this step as done.

### Installing Node.js and npm

We recommend to install **`Node.js`** using a [`package manager`](https://nodejs.org/en/download/package-manager) or a node version manager suitable for your platform. You are free to use an [`installer`](https://nodejs.org/en/download), though.

<!-- TODO: Maybe mention here that for Windows the installer is the easiest way to setup Node. -->

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](SAP Cloud Platform Account)]

For deploying your application to `Cloud Foundry in SAP Cloud Platform`, you will need an account. You can use your trial account, if you already have one.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Cloud Foundry Command Line Interface)]

You will need the `Cloud Foundry` command line interface (`cf` CLI). To see whether it is already installed, you can run `cf -v` on your command line. If the command fails, you will need to install it.

You can find installation instructions for all common platforms in the [`Cloud Foundry documentation`](https://docs.cloudfoundry.org/cf-cli/install-go-cli.html). Again, we recommend to use a `package manager` for that. If you are using `chocolatey` on Windows, please find the instructions [here](https://chocolatey.org/packages/cloudfoundry-cli).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Editor / IDE)]

Feel free to choose your favorite `IDE`. We recommend using [`Visual Studio Code`](https://code.visualstudio.com) for its excellent JavaScript and TypeScript support. However, this tutorial does not rely on any specific editor.

[DONE]
[ACCORDION-END]

That's it, you are now ready to start developing your own application with the `SAP S/4HANA Cloud SDK for JavaScript`!

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

---
