---
title: Install the Cloud Foundry Command Line Interface (CLI)
description: Detailed instructions on downloading and installing the CLI, which allows access via the Command Line
primary_tag: products>sap-cloud-platform
auto_validation: false
tags: [  tutorial>beginner, topic>cloud, products>sap-cloud-platform ]
---

## Prerequisites  
 - **Proficiency:** Beginner

## Details
### You will learn  
 - Download the Command Line Interface (CLI) package
 - Install the CLI and connect it to Cloud Foundry
 - Explore a few basic CLI commands

### Time to Complete
**15 Min**

[ACCORDION-BEGIN [Step](Download the Command Line interface)]

Download the latest Command Line Interface (CLI) from the Cloud Foundry web site.  Use the following URL:  [https://github.com/cloudfoundry/cli#downloads](https://github.com/cloudfoundry/cli#downloads).

On the Cloud Foundry website, choose either the command line installer (if you are set up to use one), or the binary from the table listed on the webpage.  If you don't know which to use, choose the installer (not the binary) for your Operating System from the table.

![Download matrix from the Cloud Foundry website](4.png)

Open the installer on your computer, and follow the installation instructions.

> **Note**:  
> For more help with the CLI installation, look at the [Cloud Foundry CLI installation documentation](http://docs.cloudfoundry.org/cf-cli/install-go-cli.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Test the Cloud Foundry CLI)]

Open a command prompt on your computer.  

> In Windows, you can search in the start menu for the application "command prompt".  
> On a Mac, use spotlight to find the application "terminal".  
> On Linux/Unix, you know how this works, just open your favorite command shell.

Next, test the Cloud Foundry command line interface to make certain it is installed correctly.

To test the CLI, type in the following

    cf

You should see a list of Cloud Foundry commands.

![Cloud Foundry list of Commands](2.png)

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Determine your Cloud Foundry URL)]

First, find the correct API URL for your Region.  Select the correct region from this table, and copy the URL.  You will use the URL in the next steps to connect to Cloud Foundry.  

| Region                                          | URL                                         |
| ----------------------------------------------- | ---------------------------------------     |
| US East (VA) running on Amazon AWS              | `https://api.cf.us10.hana.ondemand.com`     |
| US West (CA) running on Microsoft Azure         | `https://api.cf.us20.hana.ondemand.com`     |
| **Europe (Frankfurt) running on Amazon AWS**    | **`https://api.cf.eu10.hana.ondemand.com`** |
| Europe (Netherlands) running on Microsoft Azure | `https://api.cf.eu20.hana.ondemand.com`     |

> **Note**:  
> If your region is not in this list here, check the [Regions and API endpoints list](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/350356d1dc314d3199dca15bd2ab9b0e.html#loiof344a57233d34199b2123b9620d0bb41) for the most up to date list.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Connect the CLI to the cloud region)]

Next, open a command line prompt on your computer.  In the command line screen, type in the following:

    cf api [URL]

Replace the `[URL]` section with the URL you selected from the table.


![Set the Command Line region URL](4-frankfurt.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step](Log in using the CLI)]

In the same command prompt (from the previous step) type in the following command:

    cf login

When prompted, enter your username and password.  Once you are logged in, you should see your API endpoint, user, org, and space as shown in the screenshot.

![Login to the region using the CLI](5b.png)

-----

[VALIDATE_2]
[ACCORDION-END]


---

You are now set up to use the Cloud Foundry systems on SAP Cloud Platform.

---

For more information on the Command Line Interface (CLI), go to the Cloud Foundry [Getting Started with the CLI](http://docs.cloudfoundry.org/cf-cli/getting-started.html) website.


## Next Steps
- Select a tutorial from the [Tutorial Navigator](https://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](https://www.sap.com/developer/tutorial-navigator.tutorials.html)
