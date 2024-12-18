---
parser: v2
auto_validation: true
author_name: Nico Schoenteich
author_profile: https://github.com/nicoschoenteich
tags: [tutorial>beginner, topic>cloud, products>sap-business-technology-platform ]
primary_tag: products>sap-btp--cloud-foundry-environment
time: 15
---

# Install the Cloud Foundry Command Line Interface (CLI)
<!-- description --> Download and install the CLI, which allows access via the command line.

## You will learn  
 - How to download the Command Line Interface (CLI) package.
 - How to install the CLI and connect it to Cloud Foundry.
 - How to explore a few basic CLI commands.

---

### Download the Command Line interface

Download the latest Command Line Interface (CLI) from the Cloud Foundry web site.  Use the following URL:  

<https://github.com/cloudfoundry/cli/wiki/V8-CLI-Installation-Guide>.

On the Cloud Foundry website, choose either the command line installer or the binary from the table listed on the webpage. If you don't know which to use, choose the installer (not the binary) for your Operating System from the table.

Open the installer on your computer, and follow the installation instructions.

> For more help with the CLI installation, look at the [Cloud Foundry CLI installation documentation](http://docs.cloudfoundry.org/cf-cli/install-go-cli.html).


### Test the Cloud Foundry CLI

Open a command prompt on your computer.  

> In Windows, you can search in the start menu for the application "command prompt".  
>
> On a Mac, use spotlight to find the application "terminal".
>
> On Linux/Unix, you know how this works, just open your favorite command shell.

Next, test the Cloud Foundry command line interface to make sure it is installed correctly.

To test the CLI, type in the following:

```Bash
cf
```

You should see a list of Cloud Foundry commands.

![Cloud Foundry list of Commands](cfhelp.png)


### Determine your Cloud Foundry URL

First, find the correct API URL for your region.  Select the correct region from this table, and copy the URL.  You will use the URL in the next steps to connect to Cloud Foundry.  

| Region                                          | URL                                         |
| ----------------------------------------------- | ---------------------------------------     |
| Europe (Frankfurt) AWS      | `https://api.cf.eu10.hana.ondemand.com`     |
| US East (VA) AWS              | `https://api.cf.us10.hana.ondemand.com`     |
| Singapore Azure       | `https://api.cf.ap21.hana.ondemand.com`     |

> If your region is not in this list, check the [Regions and API endpoints list](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/350356d1dc314d3199dca15bd2ab9b0e.html#loiof344a57233d34199b2123b9620d0bb41) for the most up to date list.

You can also identify your API Endpoint using your Cloud Foundry account:

First, navigate to the [SAP BTP cockpit](https://hanatrial.ondemand.com) and select **Enter Your Trial Account**

![entertrial](entertrial.png)

Next, click on the subaccount.

![subaccount](subaccount.png)

The **API Endpoint** is displayed on the left-hand side of the **Overview** page.

![endpoint](endpoint.png)

### Log in using the CLI

Next, open a command line prompt on your computer.  In the command line screen, type in the following:

```bash
cf login -a <URL>
```
> Replace the `<URL>` section with the URL you selected from the table. For more information on the log in using the Command Line Interface (CLI), go to the SAP BTP Help Portal [Log On to the Cloud Foundry Environment Using the Cloud Foundry Command Line Interface](https://help.sap.com/products/BTP/65de2977205c403bbc107264b8eccf4b/7a37d66c2e7d401db4980db0cd74aa6b.html?locale=en-US) website.

When prompted, enter your username and password.  Once you are logged in, you should see your API endpoint, user, org, and space.

You are now set up to use the SAP BTP, Cloud Foundry environment.

> For more information on the Command Line Interface (CLI), go to the Cloud Foundry [Getting Started with the CLI](http://docs.cloudfoundry.org/cf-cli/getting-started.html) website.

