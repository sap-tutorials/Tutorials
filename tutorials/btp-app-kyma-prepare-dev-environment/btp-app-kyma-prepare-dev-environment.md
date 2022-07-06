---
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
title: Prepare Your Kyma Development Environment
description: This tutorial shows you how to install tools used in this tutorial, log in to your Kyma cluster, create a namespace for your app, and create a container registry secret.
keywords: cap
auto_validation: true
time: 20

tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp-kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Set Up Local Development using VS Code](btp-app-set-up-local-development)
 - [Create a Directory for Development](btp-app-create-directory)
 - [Create a CAP-Based Application](btp-app-create-cap-application)
 - [Create an SAP Fiori Elements-Based UI](btp-app-create-ui-fiori-elements)
 - [Add Business Logic to Your Application](btp-app-cap-business-logic)
 - [Create a UI Using Freestyle SAPUI5](btp-app-create-ui-freestyle-sapui5)
 - [Use a Local Launch Page](btp-app-launchpage)
 - [Implement Roles and Authorization Checks in CAP](btp-app-cap-roles)
 - [Prepare for SAP BTP Development](btp-app-kyma-prepare-btp)
 - For Windows, you'll need Chocolatey. This is a package manager that will speed up and ease installation of the tools in this tutorial. See how to install Chocolatey in [Setup/Install](https://docs.chocolatey.org/en-us/choco/setup).
 - You have prepared a container registry and you've logged in to the container registry through your CLI. A container registry is a repo where you can push your docker images. SAP BTP doesn't currently provide a container registry. You can use any container registry offering as long as it can be reached from public Internet.

## Details

### You will learn
 - How to prepare your Kyma development environment


---

[ACCORDION-BEGIN [Step 1: ](Install kubectl)]
[OPTION BEGIN [macOS]]



1. Run:
```Shell/Bash
brew install kubectl
```
2. Check if the installation is successful:
```Shell/Bash
kubectl version --client
```
You should see a version number.

[OPTION END]
[OPTION BEGIN [Windows]]

You can install it using chocolatey.

1. Run the following command:
```Shell/Bash
choco install kubernetes-cli
```
2. Check if the installation is successful:
```Shell/Bash
kubectl version --client
```
You should see something like `Client Version: version.Info{Major:"1", Minor:"19", GitVersion:"v1.19.3", GitCommit:"1e11e4a2108024935ecfcb2912226cedeafd99df", GitTreeState:"clean", BuildDate:"2020-10-14T12:50:19Z", GoVersion:"go1.15.2", Compiler:"gc", Platform:"windows/amd64"}`.


[OPTION END]
[OPTION BEGIN [Linux]]

Follow the instructions for your preferred way of installing kubectl at [Install and Set Up kubectl on Linux](https://kubernetes.io/docs/tasks/tools/install-kubectl-linux/).

[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Install kubelogin)]
[OPTION BEGIN [macOS]]

```Shell/Bash
brew install int128/kubelogin/kubelogin
```

Find more info and detailed instructions in [int128/`kubelogin`](https://github.com/int128/kubelogin#setup).
[OPTION END]
[OPTION BEGIN [Windows]]

You can install it using chocolatey:

```Shell/Bash
choco install kubelogin
```

Find more info and detailed instructions in [int128/`kubelogin`](https://github.com/int128/kubelogin#setup).

[OPTION END]
[OPTION BEGIN [Linux]]

```Shell/Bash
brew install int128/kubelogin/kubelogin
```

Find more info and detailed instructions in [int128/`kubelogin`](https://github.com/int128/kubelogin#setup).
[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Login to your Kyma cluster)]
1. Choose **KubeconfigURL** under the **Kyma Environment** tab in your subaccount.

    !![Kubeconfig URL](kubeconfigURL.png)

2. A file `kubeconfig.yaml` is downloaded.

    !![Kubeconfig yaml](kubeconfig_yaml.png)

3. Copy the `kubeconfig.yaml` file to the `~/.kube/` directory and rename it to `config`. Replace or rename any existing file with the same name.

> You can also move and rename the `kubeconfig.yaml` file from the CLI.

> 1. Rename or remove any existing file named `config` from the `~/.kube/` directory.

> 2. Run the following commands:

>     ```
>     KUBECONFIG="$HOME/.kube/config:$HOME/Downloads/kubeconfig.yaml" kubectl config view --raw >$HOME/.kube/config-new
>     mv $HOME/.kube/config-new $HOME/.kube/config
>     chmod 600 $HOME/.kube/config
>     ```

>     In case you experience problems running the commands, check [Command Line Interpreters](btp-app-#command-line-interpreters) for more details on recommended CLIs.


=== "Windows"

    There are two additional steps for Windows users.

    5. Go to `C:\ProgramData\chocolatey\bin`.

    6. Rename `kubelogin.exe` to `kubectl-oidc_login.exe`.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Create a namespace for your app)]
1. Run the following command to create a namespace `risk-management`:

    ```Shell/Bash
    kubectl create namespace risk-management
    ```
  You should get a message `namespace/risk-management created`.

2. Now, let's switch to the namespace. Run:

    ```Shell/Bash
    kubectl config set-context --current --namespace risk-management
    ```
  You should get a message `Context "shoot--kyma--X-XXXXXXX" modified.`

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 5: ](Create container registry secret)]
The container registry secret is needed to access docker container images in your registry from your Kyma cluster.

Kubernetes has a special type of secret for container registries and its kubectl command line supports it with the required options:

```Shell/Bash
kubectl create secret docker-registry container-registry \
        "--docker-server=..." \
        "--docker-username=..." \
        "--docker-email=..." \
        "--docker-password=..."
```

1. Run script to create the secret.

    Run the bash script ´./kyma-add-helm-chart/scripts/create-container-registry-secret.sh´ in the `templates` folder, which will prompt you for the required inputs. By using the script, you avoid storing your password in your shell's history.

    > Can't run the `kubectl create secret docker-registry container-registry \` command?

    > * If you encounter any errors running this command, make sure you are using the `Git BASH` command line interpreter, as advised in Step 2: Command Line Interpreters in this [tutorial](btp-app-set-up-local-development).


2. When prompted, provide the required details.

    - `Docker Server`: Use the full qualified hostname for the docker server.
    - `User` and `Email` - provide your username and e-mail that you used to create your container registry.
    - `API Key` - as part of the authentication settings of your container registry, you should be able to generate an API key to provide here.

3. Check if the secret was successfully created:

    ```
    kubectl get secret
    ```
    
    You should be able to see the newly created secret.

    ![New Secret](new_secret.png)

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 6: ](Install helm)]
[OPTION BEGIN [macOS]]

There's a multitude of options to install helm. You can see the full list at [Installing Helm](https://helm.sh/docs/intro/install/). We have also listed some options:

Run the following command:
```Shell/Bash
brew install helm
```

[OPTION END]
[OPTION BEGIN [Windows]]

There's a multitude of options to install helm. You can see the full list at [Installing Helm](https://helm.sh/docs/intro/install/). We have also listed some options:

You can install it using chocolatey.

1. Run the following command:
```Shell/Bash
choco install kubernetes-helm
```

2. Check if the installation is successful:
```Shell/Bash
helm version
```

You should see something like `version.BuildInfo{Version:"v3.8.0", GitCommit:"d14138609b01886f544b2025f5000351c9eb092e", GitTreeState:"clean", GoVersion:"go1.17.5"}`.

[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 7: ](Install Paketo (pack))]
[OPTION BEGIN [macOS]]

Pack lets you build container images, which are collaboratively maintained making it easier to maintain and update.

Install the [pack CLI](https://buildpacks.io/docs/tools/pack/#install).

```Shell/Bash
brew install buildpacks/tap/pack
```

[OPTION END]
[OPTION BEGIN [Windows]]

Pack lets you build container images, which are collaboratively maintained making it easier to maintain and update.

Install the [pack CLI](https://buildpacks.io/docs/tools/pack/#install).

You can install it using chocolatey with the command:

```Shell/Bash
choco install pack
```

As an alternative, you can install `pack` manually:

1. Download `pack` for your platform from [GitHub](https://github.com/buildpacks/pack/releases).
1. Extract the `pack` binary.
4. Enter **Edit the System Environment Variables** in the Windows search box (Windows icon in the task bar). The **System Properties** dialog is opened.
5. Choose **Environment Variables...**.
6. Choose your `Path` environment variable under *User Variables for `<your_user_name>`* and choose **Edit**.
7. Choose **Browse** and navigate to the folder where you extracted the `pack` binary.
8. Choose **OK** to add `pack` to your `Path` environment variable.


[OPTION END]
[OPTION BEGIN [Linux]]

Pack lets you build container images, which are collaboratively maintained making it easier to maintain and update.

Install the [pack CLI](https://buildpacks.io/docs/tools/pack/#install).

Follow the instructions to install the [pack CLI](https://buildpacks.io/docs/tools/pack/#install).

> In case you do not have Helm (it is installed as part of rancher desktop) installed follow the [instructions](https://helm.sh/docs/intro/install/#through-package-managers) to install helm



[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 8: ](Docker)]
Kyma runs on containers. Hence, for this tutorial, you'll need an application that enables you to build containerized applications and a docker-compatible command line interface. In the following we provide two examples - Docker Desktop and Rancher Desktop.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 9: ](Docker Desktop)]
[OPTION BEGIN [macOS]]

Here's how to install Docker Desktop:

1. Download the installer from [Install Docker Desktop on Mac](https://docs.docker.com/desktop/mac/install/).

2. Follow the instructions to install and set up docker desktop.

[OPTION END]
[OPTION BEGIN [Windows]]

Here's how to install Docker Desktop:

1. Download the installer from [Install Docker Desktop on Windows](https://docs.docker.com/desktop/windows/install/).

2. Follow the instructions to install and set up docker desktop.

[OPTION END]


[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 10: ](Rancher Desktop)]
[OPTION BEGIN [macOS]]

Follow the instructions to install Rancher Desktop:

1. Go to the [releases](https://github.com/rancher-sandbox/rancher-desktop/releases) page.

2. Download the Rancher Desktop installer for macOS.

    > The macOS installer is called `Rancher.Desktop-<version.architecture>.dmg`. Here's an example with the current latest version: `Rancher.Desktop-1.2.1.x86_64.dmg`.

3. Run the installer. When the installation is complete, drag the Rancher Desktop icon to the **Applications** folder.

    > You can find details about installation requirements and install/uninstall steps in [macOS](https://docs.rancherdesktop.io/getting-started/installation#macos).

[VALIDATE_1]
[OPTION END]
[OPTION BEGIN [Windows]]

Follow the instructions to install Rancher Desktop:


1. Go to the [releases](https://github.com/rancher-sandbox/rancher-desktop/releases) page.

2. Download the Rancher Desktop installer for Windows.

    > The Windows installer is called `Rancher.Desktop.Setup.<version>.exe`. Here's an example with the current latest version: `Rancher.Desktop.Setup.1.2.1.exe`.

3. Run the installer. When the installation is complete, choose **Finish**.

    > You can find details about installation requirements and install/uninstall steps in [Windows](https://docs.rancherdesktop.io/getting-started/installation#windows).

[VALIDATE_1]
[OPTION END]
[OPTION BEGIN [Linux]]

Follow the instructions to install Rancher Desktop:

There are several different ways to install Rancher Desktop on Linux. You can find details about installation requirements and install/uninstall steps in [Linux](https://docs.rancherdesktop.io/getting-started/installation#linux).


[VALIDATE_1]
[OPTION END]

[ACCORDION-END]
---
