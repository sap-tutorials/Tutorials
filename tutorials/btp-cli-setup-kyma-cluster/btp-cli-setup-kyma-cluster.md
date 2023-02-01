---
parser: v2
author_name: Anna Wenger
time: 30
tags: [tutorial>beginner, topic>cloud, software-product>sap-business-technology-platform, software-product-function>sap-btp-command-line-interface]
primary_tag: software-product>sap-business-technology-platform
keywords: btp, btp cli, btpcli, command line, command line interface, command line tool, sap btp command line interface
---

# Enable SAP BTP, Kyma Runtime Using the Command Line
<!-- description --> Enabling SAP BTP, Kyma Runtime means creating a Kyma environment instance, i.e. a Kyma cluster. You can then use the Kyma cluster to build applications and extensions to SAP and third-party solutions, manage roles, have your Kubernetes objects backed up, and view metrics and logs.   

## You will learn
  - How to set up a Kyma cluster in a subaccount on SAP BTP, without using the SAP BTP cockpit (see [Enable SAP BTP, Kyma Runtime](cp-kyma-getting-started) for the same procedure in the cockpit).
  - That the creation of the Kyma cluster takes about 15 - 25 minutes. It happens after step 3 in this tutorial, so you might want to plan in a coffee break before continuing with step 4.

## Prerequisites
- You've installed the `btp CLI` and familiarized yourself with it. See [Get Started with btp CLI](cp-sapcp-getstarted).
- You've installed `kubectl CLI`, including the plugin `kubectl oidc-login` for authentication. See [Installing the Kubernetes Command Line Tool](cp-kyma-download-cli).
  If you're on Windows, we recommend `Chocolatey` to install `kubectl`.
  In addition, we recommend `Krew` to install and use the `kubectl oidc` plugin for `Kubernetes OpenID Connect (OIDC)`.
- You have a global account and a subaccount on SAP BTP with admin rights. See [Get an Account on SAP BTP to Try Out Free Tier Service Plans](btp-free-tier-account).


### Log in with th btp CLI

1. Log in to your global account. We recommend using:
```Shell/Bash
btp login --sso
```

2. Since you'll be working in a subaccount, run:
```Shell/Bash
btp list accounts/subaccount
```

3. Copy the ID of the subaccount in which you want to set up the Kyma cluster to your clipboard.

4. Set the target to that subaccount with:
```Shell/Bash
btp target --subaccount <subaccount ID from your clipboard>
```

Once the target is set to the subaccount, you no longer need to specify the subaccount ID with every command.


### Ensure that Kyma is entitled to your subaccount


To check if Kyma is already entitled to your subaccount, use the following command:
```Shell/Bash
btp list accounts/entitlements
```


If you don't have a Kyma entitlement yet, run the following command – note that you need to have admin rights in the global account for managing entitlements, and that you do need the subaccount ID one last time in this case (luckily, it should still be in your clipboard):
```Shell/Bash
btp assign accounts/entitlement --to-subaccount <subaccount ID from your clipboard> --for-service kymaruntime --plan free --amount 1  
```

If you're not working in a free tier subaccount, see [Available Plans in the Kyma Environment](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/befe01d5d8864e59bf847fa5a5f3d669.html).

>You can append `--help` to the end of complete or incomplete commands. For example, `btp accounts/subaccounts --help` can help you to find the correct command to get details about a subaccount.




### Create Kyma Environment Instance

To create a Kyma environment instance (the Kyma cluster), you need to pass a .json file as a parameter that contains attributes of the Kyma cluster.

>If you're using a different plan, you might need a different region. See the Subaccount Regions section in [Regions for the Kyma Environment](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/557ec3adc3174ed4914ec9d6d13487cf.html)

1. Create a .json file in a directory of your choice with the following content: `{"name": "my-kyma-cluster", "region": "eu-west-2"}`

2. Run the following command to create the Kyma cluster:
```Shell/Bash
btp create accounts/environment-instance --display-name my-environment-instance --environment kyma --service kymaruntime --plan free --parameters <FULL-PATH-TO-JSON-FILE>
```

A new Kubernetes cluster is set up, in which the Kyma runtime and all its components will run.

To continue with this tutorial, the Kyma cluster needs to be created, but this happens in the background and may take 15‒30 minutes. You can check the status with `btp list accounts/environment-instance` ‒ once Kyma appears with status `OK`, you can continue.


### Get kubeconfig.yaml

Once you've created the environment instance, you need to get its configuration file: the `kubeconfig.yaml`, because `kubectl` relies on the `kubeconfig.yaml` to configure access to the Kyma cluster.

To do so, retrieve the `environment id` and the `KubeconfigURL` from the newly created environment instance. Note that downloading the kubeconfig.yaml may succeed even if the cluster creation has not succeeded yet. But in this case, the content would be unusable. So make sure that the Kyma environment instance is in status OK after the previous step.

1. To get your `environment id`, enter:
```Shell/Bash
btp list accounts/environment-instance
```

2. Copy the environment ID of your new Kyma environment instance to your clipboard for use in the next command.

3. To retrieve the `KubeconfigURL`, enter:
```Shell/Bash
btp get accounts/environment-instance <ENVIRONMENT-ID>
```
>Tip: If you prefer JSON output, use `btp --format json get accounts/environment-instance <ENVIRONMENT_ID> --subaccount <ID>`

4. Copy the `KubeconfigURL`.

5. To download the kubeconfig.yaml file and save it to your user directory, enter:

in PowerShell 5.1 (Windows):
```Shell/Bash
curl https://kyma-env-broker.cp.kyma.cloud.sap/kubeconfig/<EnvironmentID> -OutFile <file-path>\kubeconfig.yaml
```

in PowerShell 7.x (Windows, Mac, Linux):
```Shell/Bash
curl https://kyma-env-broker.cp.kyma.cloud.sap/kubeconfig/<EnvironmentID> > <file-path>\kubeconfig.yaml
```

in macOS and Linux:
```Shell/Bash
curl -o kubeconfig.yaml https://kyma-env-broker.cp.kyma.cloud.sap/kubeconfig/<ENVIRONMENT-ID>
```



### Provide kubeconfig.yaml to Kyma

For your convenience, expose the `kubeconfig` as an environment variable or add the location of kubeconfig.yaml to your path in the system variables. The environment variable is session-based, whereas the path is saved permanently.

To only use a session-based environment variable, enter:

[OPTION BEGIN [Windows]]

```Shell/Bash
$ENV:KUBECONFIG="<path>\kubeconfig.yaml"
```

Alternatively, add the path in system variables as follows:

1. Open the **environment variables**

2. Under **system variables**, select **Path**

3. Click `Edit`

4. Add the location of `kubeconfig.yaml`

[OPTION END]

[OPTION BEGIN [MacOS and Linux]]

```Shell/Bash
export KUBECONFIG={KUBECONFIG_FILE_PATH}   
```

[OPTION END]



### Check if you can access your cluster

If you've created the cluster, you're automatically assigned the cluster-admin role. This role gives you unrestricted access to configure your cluster. For example, you can assign roles to other users.

To check if you can access your cluster, run:
```Shell/Bash
kubectl get namespaces
```

>You get a token which will also be valid for future sessions.

To learn more about the Kyma environment and its functionality, see:

- [SAP BTP, Kyma runtime](https://discovery-center.cloud.sap/serviceCatalog/kyma-runtime)
- [SAP Help Portal - Kyma Environment](https://help.sap.com/viewer/3504ec5ef16548778610c7e89cc0eac3/Cloud/en-US/468c2f3c3ca24c2c8497ef9f83154c44.html)
- [kyma-project](https://kyma-project.io/docs/kyma/latest)
- [Kyma - YouTube](https://www.youtube.com/channel/UC8Q8bBtYe9gQN-dQ-_L8JvQ)
- [Cloud Native for Beginners - YouTube](https://youtube.com/playlist?list=PL6RpkC85SLQCwaJ54TAAHMvSl5wpVPrai)
- [Mission: Develop a Full-Stack Application in the Kyma Runtime](mission.cp-kyma-full-stack)







---
