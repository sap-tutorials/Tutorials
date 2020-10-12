---
title: Install the Kubernetes Command Line Tool
description: Download and install the Kubernetes command line tool kubectl which allows access to the SAP Cloud Platform Kyma Runtime via the command line.
time: 15
auto_validation: true
tags: [ tutorial>beginner, topic>cloud, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform\, kyma-runtime
---


## Details
### You will learn  
  - How to download the Kubernetes command line tool `kubectl`.
  - How to install `kubectl` and connect it to a Kyma Runtime.
  - How to explore a few basic `kubectl` commands.

---

[ACCORDION-BEGIN [Step 1: ](Download and install kubectl)]

1. Download the latest command line tool `kubectl` from the Kubernetes web site.  Use the following URL:  

    <https://kubernetes.io/docs/tasks/tools/install-kubectl>

2. On the Kubernetes website, follow the instructions listed for your operating system, for example, `Install kubectl on macOS`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Test the kubectl installation)]

1. Open a command prompt on your computer.

    > In Windows, you can search in the start menu for the application `cmd` or `command prompt`.  
    >
    > On a Mac, use spotlight to find the application "terminal".
    >
    > On Linux/Unix, you know how this works, just open your favorite command shell.

2. Test the `kubectl` command line tool to make sure it is installed correctly by running the following command in your CLI:
```Shell/Bash
kubectl version --client
```

This should return a list of version properties, for example.

```Shell/Bash
'Client Version: version.Info{Major:"1", Minor:"18", GitVersion:"v1.18.0", GitCommit:"9e991415386e4cf155a24b1da15becaa390438d8", GitTreeState:"clean", BuildDate:"2020-03-26T06:16:15Z", GoVersion:"go1.14", Compiler:"gc", Platform:"darwin/amd64"}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Download the Kyma Runtime Kubeconfig)]

1. Navigate to the Console UI of your Kyma runtime.

2. Log onto the Kyma runtime.

3. At the top-right of the Kyma runtime window ,choose the user information dropdown.

    ![kubeconfig](kubeconfig.png)

4. Choose the option `Get Kubeconfig` to download the file.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set the KUBECONFIG environment variable)]

1. Open a command line prompt on your computer.  In the command line screen, type in the following:

    ```Shell/Bash
    export KUBECONFIG=<KUBECONFIG_FILE_PATH>
    ```

    Replace the `<KUBECONFIG_FILE_PATH>` section with the file path of the `Kubeconfig` you downloaded in the previous step.

2. Test the configuration by running:  

    ```Shell/Bash
    kubectl config get-contexts
    ```

    This should return a response similar to:

    |CURRENT|NAME|CLUSTER|AUTHINFO|NAMESPACE|
    |-------|----|-------|--------|---------|
    |*      | `c-#######.kyma.shoot.live.k8s-hana.ondemand.com`|`c--#######.kyma.shoot.live.k8s-hana.ondemand.com`|`OIDCUser`| |

3. You are now set up to use the Kyma runtime on SAP Cloud Platform.

> For more information on `kubectl`, visit the Kubernetes [overview](https://kubernetes.io/docs/reference/kubectl/overview/) and [kubectl-commands](https://kubernetes.io/docs/reference/generated/kubectl/kubectl-commands) websites.

[VALIDATE_1]
[ACCORDION-END]

---
