---
title: Install the MultiApps Cloud Foundry CLI Plugin
description: Download and install the Command Line Interface (CLI) plugin. This plugin allows you to deploy MTA achives from the command line.
auto_validation: true
time: 10
tags: [ tutorial>beginner, topic>cloud]
primary_tag: products>sap-cloud-platform-for-the-cloud-foundry-environment
---

## Details
### You will learn
  - How to download a Cloud Foundry CLI plugin
  - How to install a Cloud Foundry CLI plugin
  - How to use the MTA (aka MultiApps) plugin


---

[ACCORDION-BEGIN [Step 1: ](Verify that the Cloud Foundry CLI is installed)]

Make sure you installed the Cloud Foundry CLI successfully:
```Bash
cf --version
```

Now you should see the version of the CLI you are using.


> You can go to [this tutorial](https://developers.sap.com/tutorials/cp-cf-download-cli.html) to install the Cloud Foundry CLI if necessary

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Download the MultiApps plugin)]

[Download the plugin](https://github.com/cloudfoundry-incubator/multiapps-cli-plugin) from the GitHub repo and save it in a folder of your choice.
Make sure to remember the path of the downloaded file as we need it in the next step.

Mac OS X 64 bit | Windows 64 bit | Linux 64 bit
--- | --- | ---
[mta-plugin-darwin](https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/download/v2.0.13/mta_plugin_darwin_amd64) | [mta-plugin-windows](https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/download/v2.0.13/mta_plugin_windows_amd64.exe) | [mta-plugin-linux](https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/download/v2.0.13/mta_plugin_linux_amd64) |

> You need to make the plugin executable before installing it, if you are running on an Unix-based system. You can achieve this by executing the following command `chmod +x <path-to-the-plugin>`

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Install the plugin)]


Install the plugin, using the following command:
```Bash
cf install-plugin <path-to-the-plugin> -f
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Verify the installation of the plugin)]

List all Cloud Foundry CLI plugins to see whether the installation worked.
```Bash
cf plugins
```
You should now see the following output:

![listed plugins](./cfplugins.png)

> This list shows you the all the new commands you added to the CLI. E.g. now you can run `cf deploy` and `cf mta` from the command line.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Inspect the deploy command options)]
Run the following command to inspect all options for the `cf deploy` command:
```Bash
cf deploy --help
```


[VALIDATE_1]
[ACCORDION-END]

---
