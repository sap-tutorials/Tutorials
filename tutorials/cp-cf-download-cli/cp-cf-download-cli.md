---
title: Install the Cloud Foundry Command Line Interface (CLI)
description: Download and install the CLI, which allows access via the command line.
auto_validation: true
author_name: Marius Obert
author_profile: https://github.com/iobert
tags: [tutorial>beginner, topic>cloud, products>sap-business-technology-platform ]
primary_tag: products>sap-btp--cloud-foundry-environment
time: 15
---

## Details
### You will learn  
 - How to download the Command Line Interface (CLI) package
 - How to install the CLI and connect it to Cloud Foundry
 - How to explore a few basic CLI commands

---

[ACCORDION-BEGIN [Step](Download the Command Line interface)]

Download the latest Command Line Interface (CLI) from the Cloud Foundry web site.  Use the following URL:  

<https://github.com/cloudfoundry/cli/wiki/V7-CLI-Installation-Guide>.

On the Cloud Foundry website, choose either the command line installer or the binary from the table listed on the webpage. If you don't know which to use, choose the installer (not the binary) for your Operating System from the table.

Open the installer on your computer, and follow the installation instructions.

> For more help with the CLI installation, look at the [Cloud Foundry CLI installation documentation](http://docs.cloudfoundry.org/cf-cli/install-go-cli.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Test the Cloud Foundry CLI)]

Open a command prompt on your computer.  

> In Windows, you can search in the start menu for the application "command prompt".  
>
> On a Mac, use spotlight to find the application "terminal".
>
> On Linux/Unix, you know how this works, just open your favorite command shell.

Next, test the Cloud Foundry command line interface to make sure it is installed correctly.

To test the CLI, type in the following:

    cf

You should see a list of Cloud Foundry commands.

!![Cloud Foundry list of Commands](cfhelp.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Determine your Cloud Foundry URL)]

First, find the correct API URL for your region.  Select the correct region from this table, and copy the URL.  You will use the URL in the next steps to connect to Cloud Foundry.  

| Region                                          | URL                                         |
| ----------------------------------------------- | ---------------------------------------     |
| Europe (Frankfurt) AWS      | `https://api.cf.eu10.hana.ondemand.com`     |
| US East (VA) AWS              | `https://api.cf.us10.hana.ondemand.com`     |
| Singapore Azure       | `https://api.cf.ap21.hana.ondemand.com`     |

> If your region is not in this list, check the [Regions and API endpoints list](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/350356d1dc314d3199dca15bd2ab9b0e.html#loiof344a57233d34199b2123b9620d0bb41) for the most up to date list.

You can also identify your API Endpoint using your Cloud Foundry account:

First, navigate to the [SAP BTP cockpit](https://hanatrial.ondemand.com) and select **Enter Your Trial Account**

!![entertrial](entertrial.png)

Next, click on the subaccount.

!![subaccount](subaccount.png)

The **API Endpoint** is displayed on the left-hand side of the **Overview** page.

!![endpoint](endpoint.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Log in using the CLI)]

Next, open a command line prompt on your computer.  In the command line screen, type in the following:

```bash
cf login -a <URL>
```
> Replace the `<URL>` section with the URL you selected from the table.

When prompted, enter your username and password.  Once you are logged in, you should see your API endpoint, user, org, and space.

You are now set up to use the SAP BTP, Cloud Foundry environment.

> For more information on the Command Line Interface (CLI), go to the Cloud Foundry [Getting Started with the CLI](http://docs.cloudfoundry.org/cf-cli/getting-started.html) website.

[VALIDATE_2]
[ACCORDION-END]
