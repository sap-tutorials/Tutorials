---
title: Get Ready for UI5-Development on Your Local Machine
description: Prepare you local development environment to implement successful SAPUI5 projects.
auto_validation: true
time: 5
tags: [ tutorial>beginner, products>sap-cloud-platform-for-the-cloud-foundry-environment]
primary_tag: topic>sapui5
---

## Prerequisites
 - [Install Node.js](https://nodejs.org/en/download/)
 - [[Only for Windows users] Install Chocolatey](https://chocolatey.org/)

## Details
### You will learn
  - How to install the Yeoman scaffolding tool
  - How to install the easy-ui5 generator plugin for Yeoman
  - How the Multi-Target Application (MTA) build tool for Cloud Foundry works


---

[ACCORDION-BEGIN [Step: ](Install Yeoman)]

[Yeoman](http://yeoman.io) is an Open Source scaffolding tool for modern web-apps. It helps you to kickstart new projects, prescribing best practices and tools to help you stay productive.

Install the module globally:
```Bash
npm install -g yo
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Install the easy-ui5 generator)]

In order to use Yeoman to create SAPUI5 projects, we need to install a plugin (another `npm` module). Yeoman recognizes all plugins automatically on the basis of the `generator-` prefix of the module.

```Bash
npm install -g generator-easy-ui5
```

This plugin has also been published on [GitHub](https://github.com/SAP/generator-easy-ui5).


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Verify the installations)]

Use the following command to test whether the installation has been successful.

```Bash
yo
```

You should see the Easy Ui5 generator in the list of generators:

![EasyUI5](./verify.png)


This plugin has also been published on [GitHub](https://github.com/SAP/generator-easy-ui5).


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Install the MTA build tool)]

Cloud-native applications are being ship as so called `mta` archives. The [Multi-target Application Archive Builder](https://github.com/SAP/cloud-mta-build-tool) is a standalone command-line tool that builds a deployment-ready multi-target application archive `.mtar` file. You can leverage `npm` to install this tool as well:

```Bash
npm install -g mbt
```

Run `mbt --help` to see all available commands.


[VALIDATE_1]
[ACCORDION-END]
[ACCORDION-BEGIN [Step : ](Install Make)]

You might be able to skip this step. Usually, users of unix-based OS already have `make` preinstalled. Verify that `make` is installed with.
```
make -v
```

If you are using Windows and need to install `make` by following [these instructions](https://chocolatey.org/packages/make):
```
choco install make
```

[DONE]
[ACCORDION-END]

---
