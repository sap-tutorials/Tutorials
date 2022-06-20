---
title: Setup a Kyma Cluster on the Command Line
description: Learn how to setup a Kyma cluster on SAP BTP without using a graphical user interface.
time: 15
tags: [tutorial>beginner, topic>cloud, software-product>sap-business-technology-platform, software-product-function>sap-btp-command-line-interface]
primary_tag: software-product>sap-business-technology-platform
keywords: btp, btp cli, btpcli, command line, command line interface, command line tool, sap btp command line interface
---

## Details
### You will learn
  - How to setup a Kyma cluster on SAP BTP using the command line
This tutorial provides the steps to configure the tools to access a Kyma cluster. You can use the Kyma cluster to build applications and extensions.

## Prerequisites
- You have a global account and a subaccount on SAP BTP with the free tier option and entitlements for the Kyma environment.
See [free tier account](btp-free-tier-account)
- You've installed the `btp CLI` and familiarized yourself with it.
See [Get Started with btp CLI](cp-sapcp-getstarted).
- You've installed `kubectl CLI`, including the plugin `kubectl oidc-login` for authentication.
See [Installing the Kubernetes Command Line Tool](cp-kyma-download-cli).
If you're on Windows, we recommend `Chocolatey` to install the `Kubernetes command-line tool`, also known as `kubectl`.
In addition, we recommend `Krew` to install and use the kubectl plugin for `Kubernetes OpenID Connect (OIDC)` authentication, also known as  `kubectl oidc` plugin.
- You're logged on to a global account. Use `btp login --sso` to log in.

[ACCORDION-BEGIN [Step 1: ](Ensure that Kyma is entitled to your subaccount)]
To check if Kyma is already entitled to your subaccount, use the following command:
```Shell/Bash
btp list accounts/entitlements
```

If you don't have a Kyma entitlement yet, call:
```Shell/Bash
btp assign accounts/entitlement --to-subaccount <SUBACCOUNT_ID> --for-service kymaruntime --plan free --amount 1  
```

>You can append `--help` to the end of complete or incomplete commands. For example, `btp accounts/subaccounts --help` can help you to find the correct command to get details about a subaccount.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Create Kyma Environment Instance)]
To create a Kyma environment instance (the Kyma cluster), you need a .json file as a parameter.

1. Create a .json file in a directory of your choice with the following content: `{"name:" "YOUR_OWN_CLUSTER_NAME", "region": "eu-west-2"}`

2. Run the following command to create the Kyma cluster:
```Shell/Bash
btp create accounts/environment-instance --subaccount <YOUR_SUBACCOUNT_ID> --display-name <DISPLAY_NAME> --environment kyma --service kymaruntime --plan free --parameters <JSON_FILE_NAME>
```
>Your Kyma environment instance will be created. This could take a few minutes.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Get kubeconfig.yaml)]
Once you've created the environment instance, you need to get its configuration file, the `kubeconfig.yaml`. The `kubectl` relies on the `kubeconfig.yaml` to configure access to the Kyma cluster.

To do so, you need to retrieve the `environment id` and the `KubeconfigURL`.

[OPTION BEGIN [Windows]]

1. To get your `environment id`, enter:
```Shell/Bash
btp list accounts/environment-instance
```

2. Copy the `environment id` from the command prompt.

3. To retrieve the `KubeconfigURL`, use:
```Shell/Bash
btp get accounts/environment-instance <ENVIRONMENT_ID>
```

4. Copy the `KubeconfigURL`.

5. To download the kubeconfig.yaml file and save it to your user directory, call:
```Shell/Bash
curl <KUBECONFIG_URL> > <YOUR_USER_DIRECTORY>\kubeconfig.yaml
```

[OPTION END]

[OPTION BEGIN [Mac]]

Download the kubeconfig.yaml file as follows:
```Shell/Bash
mv <ENVIRONMENT_ID> kubeconfig.ym
```

[OPTION END]

<!-- Mac example: mv CEAC0EF0-2D7A-4F60-9BB9-04BB27D79D6E kubeconfig.ym -->

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Provide kubeconfig.yaml to Kyma)]
For your convenience, expose the `kubeconfig` as an environment variable or add the location of kubeconfig.yaml to your path in system variables. The environment variable is session based whereas the path is saved permanently.

To use a session based environment variable, call:

[OPTION BEGIN [Windows]]

```Shell/Bash
$ENV:KUBECONFIG="{KUBECONFIG_FILE_PATH}"
```

Alternatively, add the path in system variables as follows:

1. Open the **environment variables**

2. Under **system variables**, select **Path**

3. Click `Edit`

4. Add the location of `kubeconfig.yaml`

[OPTION END]

[OPTION BEGIN [Mac]]

To use a session based environment variable, call:
```Shell/Bash
export KUBECONFIG={KUBECONFIG_FILE_PATH}   
```

[OPTION END]

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 5: ](Check if you can access your cluster)]
If you've created the cluster, you're automatically assigned the cluster-admin role. This role gives you unrestricted access to configure your cluster. For example, you can assign roles to other users.

To check if you can access your cluster, run:
```Shell/Bash
kubectl get namespaces
```

>You get a token which will also be valid for future sessions.

[DONE]
[ACCORDION-END]

<!---[ACCORDION-BEGIN [Step 6: ](Assign Roles to users)]

You can assign roles to other users, so you won't have to do all the work by yourself.

See [Assign Roles in the Kyma Environment](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/148ae38b7d6f4e61bbb696bbfb3996b2.html?locale=en-US&version=Cloud) for cockpit equivalent

[DONE]
[ACCORDION-END]--->



---
