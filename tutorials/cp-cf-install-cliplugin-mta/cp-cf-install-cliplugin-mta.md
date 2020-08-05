---
title: Install the MultiApps Cloud Foundry CLI Plugin
description: Download and install the Command Line Interface (CLI) plugin. This plugin allows you to deploy MTA archives from the command line.
auto_validation: true
time: 5
tags: [ tutorial>beginner, topic>cloud]
primary_tag: products>sap-cloud-platform-for-the-cloud-foundry-environment
---

## Details
### You will learn
  - How to add the community repository to the  Cloud Foundry CLI
  - How to install a Cloud Foundry CLI plugin
  - How to use the [MTA (aka MultiApps) plugin](https://github.com/cloudfoundry-incubator/multiapps-cli-plugin)


---

[ACCORDION-BEGIN [Step 1: ](Verify that the Cloud Foundry CLI is installed)]

Make sure you installed the Cloud Foundry CLI successfully:
```Bash
cf --version
```

Now you should see the release number of the CLI you are using.


> You can go to [this tutorial](https://developers.sap.com/tutorials/cp-cf-download-cli.html) to install the Cloud Foundry CLI if necessary


[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 3: ](Install the plugin)]


Install the plugin, using the following command:
```Bash
cf install-plugin multiapps
```

> If you do not have the community repository in your CF CLI, you can add it first by executing.
```Bash
cf add-plugin-repo CF-Community https://plugins.cloudfoundry.org
```

[DONE]
[ACCORDION-END]
[ACCORDION-BEGIN [Step 4: ](Verify the installation of the plugin)]

List all Cloud Foundry CLI plugins to see whether the installation worked.
```Bash
cf plugins | grep multiapps
```
You should now see the following output:

![listed plugins](./cfplugins.png)

> This list shows you all the new commands you added to the CLI. E.g., now you can run `cf deploy` and `cf mta` from the command line.

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
