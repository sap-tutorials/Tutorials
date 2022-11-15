---
parser: v2
auto_validation: true
author_name: DJ Adams
author_profile: https://github.com/qmacro
time: 5
tags: [ tutorial>beginner, topic>cloud]
primary_tag: products>sap-btp--cloud-foundry-environment

---

# Install the MultiApps Cloud Foundry CLI Plugin
<!-- description --> Download and install the Command Line Interface (CLI) plugin, which allows you to deploy MTA archives from the command line.

## You will learn
  - How to install a Cloud Foundry CLI plugin
  - How to use the [MTA (aka MultiApps) plugin](https://github.com/cloudfoundry-incubator/multiapps-cli-plugin)

---

### Verify that the Cloud Foundry CLI is installed


Make sure you installed the Cloud Foundry CLI successfully:
```Bash
cf --version
```

Now you should see the release number of the CLI you are using.


> You can go to [this tutorial](cp-cf-download-cli) to install the Cloud Foundry CLI if necessary


### Install the plugin



Install the plugin from the OS-specific binary, using the following command:

[OPTION BEGIN [macOS]]
```Bash
cf install-plugin -f https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/latest/download/multiapps-plugin.osx
```
[OPTION END]
[OPTION BEGIN [Windows 64 Bit]]
```Bash
cf install-plugin -f https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/latest/download/multiapps-plugin.win64.exe
```
[OPTION END]
[OPTION BEGIN [Linux 64 Bit]]
```Bash
cf install-plugin -f https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/latest/download/multiapps-plugin.linux64
```
[OPTION END]
[OPTION BEGIN [Windows 32 Bit]]
```Bash
cf install-plugin -f https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/latest/download/multiapps-plugin.win32.exe
```
[OPTION END]
[OPTION BEGIN [Linux 32 Bit]]
```Bash
cf install-plugin -f https://github.com/cloudfoundry-incubator/multiapps-cli-plugin/releases/latest/download/multiapps-plugin.linux32
```
[OPTION END]

### Verify the installation of the plugin


List all Cloud Foundry CLI plugins to see whether the installation worked.

[OPTION BEGIN [macOS]]
```Bash
cf plugins | grep multiapps   
```
[OPTION END]
[OPTION BEGIN [Windows]]
```Terminal
cf plugins | find /I "multiapps"
```
[OPTION END]
[OPTION BEGIN [Linux]]
```Bash
cf plugins | grep multiapps   
```
[OPTION END]

You should now see the following output:

![listed plugins](./cfplugins.png)

> This list shows you all the new commands you added to the CLI. E.g., now you can run `cf deploy` and `cf mta` from the command line.

### Inspect the deploy command options

Run the following command to inspect all options for the `cf deploy` command:
```Bash
cf deploy --help
```




---
