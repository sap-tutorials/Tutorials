---
title: Setup Node.js to connect to HXE
description: Use Node.js to connect and read data from your SAP HANA express edition
author_name: Thomas Jung
author_profile: https://github.com/jung-thomas
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, topic>sql, products>sap-hana, products>sap-hana\,-express-edition ]
---

## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [SAP HANA, express edition - Server Only deployment Options](hxe-database-server)


## Next Steps
 - [Deploy a Node.js Application for SAP HANA, Express Edition](hxe-node-express)

## Details
### You will learn  
Now that you have tables and data in your system the next step will be to access that data through the use of an application. The following steps will prepare your system to do the development.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Setup your system)]

> Node.js® is a JavaScript runtime built on Chrome's V8 JavaScript engine. Node.js uses an event-driven, non-blocking I/O model that makes it lightweight and efficient. Node.js' package ecosystem, npm, is the largest ecosystem of open source libraries in the world.

To setup your system you will need to install the appropriate Node.js on your system. To do this please go to the [website](https://nodejs.org/en/) for Node.js and follow the instructions there for the type of system you have.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Install @sap/hdbext)]

Now that you have installed Node.js you can open a command line on your system and check to be sure it is there. To do so type the following command.

```shell
node -v
```

![Node version](1.png)

Now that you have verified that Node.js is installed it is time install the appropriate library to connect to your SAP HANA express edition. This is the `@sap/hdbext` library which you can learn more about in [the SAP Help](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/latest/en-US/54513272339246049bf438a03a8095e4.html#loio54513272339246049bf438a03a8095e4__section_ilt_mkt_vt).

Add the SAP Registry to your NPM configuration

```shell
npm config set @sap:registry=https://npm.sap.com
```

Make a directory for your project and change into that directory.

To install this library from your command line you will run the following command.

```shell
npm install @sap/hdbext
```

![npm install](1_1.png)

Your system may require administrator access to run the install so keep that in mind if you get an error. You can also use the `-g` flag on the install command to make it a universal install otherwise you will have to install the module into each application directory you make.

![permission denied](2.png)

[DONE]

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Check your install)]

Now from the command line you should be able to check to verify the install was successful. To do so run the following command.

```
npm list
```

This command lists all of the modules that are installed on your system and in the list you should see `@sap/hdbext@6.2.1` (or the latest version)

[DONE]

[ACCORDION-END]
