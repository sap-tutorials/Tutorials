---
title: SAP HANA XS Advanced, explore the basic tools
description: This tutorial will walk you through exploring the basics of XS Advanced tools, such as athe adminsitration cockpit or SAP Web IDE for SAP HANA
primary_tag: products>sap-hana
tags: [  tutorial>beginner ]
---

## Prerequisites  
 - **Proficiency:** Beginner | Intermediate | Advanced
 - **Tutorials:** [Get an SAP HANA, express edition, instance with XS Advanced applications](https://www.sap.com/developer/topics/sap-hana-express.html)


## Next Steps
 - [Get started with SAP HANA, XS Advanced development](https://www.sap.com/developer/groups/hana-xsa-get-started.html)


## Details
### You will learn  
You will explore the basic tools to develop and perform administration tasks in XS Advanced,

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Log in to the XS Command Line Interface from the server)]

The XS CLI enables you to maintain not only the applications that are deployed to the XS advanced run-time environment, but also the run-time environment itself, and the users who access and use it. You can get it from the [download manager for SAP HANA, express edition](https://sap.com/cmp/ft/crm-xu16-dat-hddedft/index.html) or from the [SAP Service Marketplace](https://launchpad.support.sap.com/#/softwarecenter). Make sure you have the latest version in order to avoid issues.

The XS CLI is also available by default in the SAP HANA server with XS Advanced applications.
From a console connected to the operating system, switch to user `hxeadm` and log in as follows:

```bash
sudo su - hxeadm
xs login
```

![XS login](xslogin.png)

Enter the master password to log in with `XSA_ADMIN`. You are now logged in to the default organization and space. You can later log in to a different organization or space by adding `-s` for space or `-o` for organization.

>Note: If you get an `SSL error` because you are using the default self-signed certificate from SAP HANA, express edition, add `--skip-ssl-validation` to your login command.

Alternatively, only from the CLI available in the SAP HANA, express edition instance, you can use the following command to log in as `XSA_ADMIN` specifying only the password

```bash
xs-admin-login
```

The XS CLI allows you to perform most of the administration tasks available in the XS Advanced Cockpit. The available commands can be found in the [SAP Help Portal](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/2.0.00/en-US/addd59069e6f444ca6ccc064d131feec.html)

Use command `xs target -s SAP` to switch to the SAP space, where XS Advanced applications such as the Administration Cockpit and the SAP Web IDE for SAP HANA can be found.

You can check on the status of the running applications with command `xs apps`. This will also tell you the URL for the important applications you will need for administration and the SAP Web IDE for SAP HANA.

![XS apps](xsapps.png)

You can also see a list of running services with command `xs services`. You can find more details about [service types and the service marketplace in the SAP Help](https://help.sap.com/viewer/4505d0bdaf4948449b7f7379d24d0f0d/2.0.02/en-US/33e3c5926feb4098a32edcaa7290c3d1.html) portal.

[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Access the SAP HANA Cockpit or the XS Advanced Cockpit)]

You will find some applications with the word `Cockpit` in their names are running by default.
> Note that the running instances need to show `1/1`. If you see `0/1` and `STARTED` is the requested status, the application is probably still starting.

The SAP HANA Cockpit and the XS Advanced Cockpit can be used to perform administration tasks on the system.

The SAP HANA Cockpit is the application called `cockpit-web-app`. The cockpit manager is `cockpit-admin-web-app`. You can use the URLs displayed by the `xs a` command to access the cockpit as `XSA_ADMIN`.

![HANA Cockpit](hanaco.png)

The name for the SAP HANA XS Advanced Cockpit is `xsa-cockpit`

![XS cockpit](xsa.png)

You can find out more about the administration tools for SAP HANA 1.0 SPS12 and SAP HANA 2.0 SPS00 and higher in the [dedicated SAP Help](https://help.sap.com/viewer/6b94445c94ae495c83a19646e7c3fd56/2.0.03/en-US/577f8d3ffebd4265b73e2c673d934412.html) portal.

[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Explore the SAP Web IDE for SAP HANA)]

Open a new browser window and copy the URL for the SAP Web IDE for SAP HANA.

> Important: If you have logged in as `XSA_ADMIN`, alternate between normal and incognito mode to log in as `XSA_DEV`.

 By default, in SAP HANA, express edition, the URL for SAP Web IDE for SAP HANA is `https://hxehost:53075`. Use the user `XSA_DEV` and its password. For SAP HANA, express edition, this is the same password as the master password.

![XS admin](webide2.png)

On the left side panel, you will find access to the development perspective, which is loaded by default and where you can create your Multi Target Applications.

![XS admin](webide2.png)

You can also find the Database Explorer where you can connect to the physical SYSTEMDB and tenant databases, explore the schemas and tables and execute SQL commands.

![XS admin](db.png)

The third icon takes yo to the settings, where you can configure different aspects of the code and modelling editors, shortcuts and plugins enabled for SAP Web IDE for SAP HANA. For example, if you wanted to use the Fiori Launchpad plugin for development, you would need to access the settings.

![XS admin](settings.png)

On the left side panel, you will find additional tools that will be explored incrementally along with the XS Advanced application development tutorials.

[ACCORDION-END]

## Next Steps
- [Get started with SAP HANA, XS Advanced development](https://www.sap.com/developer/groups/hana-xsa-get-started.html)
