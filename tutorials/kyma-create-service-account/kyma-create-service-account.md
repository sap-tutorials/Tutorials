---
title: Create a Kyma service account
description: Learn how to create a Kubernetes service account that you can leverage to interact with your Kyma cluster.
auto_validation: true
time: 10
tags: [ tutorial>beginner, products>sap-cloud-platform]
primary_tag: products>sap-cloud-platform\, kyma-runtime
---

## Prerequisites
 - [Enable SAP BTP, Kyma Runtime](cp-kyma-getting-started)
 - [Install the Kubernetes Command Line Tool](cp-kyma-download-cli)

## Details
### You will learn
  - What Kubernetes service accounts are used for
  - How to create a new service account
  - How to replace the [`kubeconfig`](https://rancher.com/learning-paths/how-to-manage-kubernetes-with-kubectl/)

---

[ACCORDION-BEGIN [Step 1: ](Create a namespace)]

The [`kubeconfig`](https://rancher.com/learning-paths/how-to-manage-kubernetes-with-kubectl/) file that you are currently using is based on your [User Account](https://kubernetes.io/docs/reference/access-authn-authz/service-accounts-admin/#user-accounts-versus-service-accounts), which represents a user that has been logged in the Kyma dashboard when you downloaded the `kubeconfig`. This file contains a token that expires after 8 hours.

This tutorial will show you how to create a new `kubeconfig` file based on a service account. In contrast to the `kubeconfig` file from the Kyma dashboard, this token won't expire every 8 hours and are therefore well-suited for scenarios like CI/CD pipelines. Please note that this could be a potential security issue.

Service accounts are bound to a namespace, so we need to create a new namespace before any service account can be created. Run the following command the create a new namespace "tutorial":

```Bash
kubectl create namespace tutorial
```



[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step : ](Create a service account)]

A service account alone won't do the job. You also need to define a role that contains all the desired permissions and assign it to a service account with a role binding. You need to create all three artifacts to use a service account via `kubectl`.

1. Create a new file called `tutorial-sa.yaml` with the following payload to create all artifacts (service account, role, role binding, and a `ConfigMap` for verification).

    ```YAML
    apiVersion: v1
    kind: ServiceAccount
    metadata:
      name: tutorial-service-account
    ---
    kind: Role
    apiVersion: rbac.authorization.k8s.io/v1
    metadata:
      name: tutorial-role
    rules:
      - apiGroups:
          - ""
          - extensions
          - apps
          - gateway.kyma-project.io
          - servicecatalog.k8s.io
        resources:
          - deployments
          - replicasets
          - pods
          - configmaps
          - apirules
          - serviceinstances
          - servicebindings
          - services
        verbs:
          - create
          - update
          - patch
          - delete
          - get
          - list
    ---
    kind: RoleBinding
    apiVersion: rbac.authorization.k8s.io/v1
    metadata:
      name: tutorial-role-binding
    subjects:
      - kind: ServiceAccount
        name: tutorial-service-account
    roleRef:
      kind: Role
      name: tutorial-role
      apiGroup: rbac.authorization.k8s.io
    ---
    apiVersion: v1
    kind: ConfigMap
    metadata:
      name: tutorial-config-map
    data:
      out: "Congrats, you completed the tutorial successfully!"
    ```
    Note that the `rules` section specified the permissions of the service account. Modify this section to adjust the role to your needs.

2. Create a service account based on the file.

    ```Bash
    kubectl apply -f tutorial-sa.yaml -n tutorial
    ```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Understand the structure of the configuration file)]

The `kubeconfig` file that we want to create must look similar to this:
```YAML
apiVersion: v1
kind: Config
preferences: {}
clusters:
- cluster:
    certificate-authority-data:
    server: https://apiserver.<id>.kyma.shoot.live.k8s-hana.ondemand.com
  name: <id>.kyma.shoot.live.k8s-hana.ondemand.com
users:
- name:
  user:
    token:  
contexts:
- context:
    cluster: <id>.kyma.shoot.live.k8s-hana.ondemand.com
    user:
  name: <id>.kyma.shoot.live.k8s-hana.ondemand.com
current-context: <id>.kyma.shoot.live.k8s-hana.ondemand.com
```

You can see that this file is moderately easy to read. [The configuration file](https://kubernetes.io/docs/tasks/access-application-cluster/configure-access-multiple-clusters/#define-clusters-users-and-contexts) defines clusters (the location of the system), users (with authentication tokens), and contexts (to map users to clusters).

Go to the next step to learn how to fill this template with the proper values.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step : ](Create a new kubeconfig)]

Now that you understand how the `kubeconfig` file is structured, create a new one that leverages your just created service account.


[OPTION BEGIN [Windows]]



1.    Create a temporary file `temp.ps1` that will fetch all required information and create the `kubeconfig` file.

    ```PowerShell
    $API_SERVER_URL = kubectl config current-context

    $SECRET_NAME = kubectl get sa -n tutorial tutorial-service-account -ojsonpath='{.secrets[0].name}'

    $CA = kubectl get secret/$SECRET_NAME -n tutorial -o jsonpath='{.data.ca\.crt}'

    $TOKEN = kubectl get secret/$SECRET_NAME -n tutorial -o jsonpath='{.data.token}'
    $DEC_TOKEN = [System.Text.Encoding]::UTF8.GetString([System.Convert]::FromBase64String($TOKEN))

    Add-Content -Path kubeconfig.yaml @"
    apiVersion: v1
    kind: Config
    clusters:
    - name: default-cluster
      cluster:
        certificate-authority-data: $CA
        server: https://api.$API_SERVER_URL
    users:
    - name: default-user
      user:
        token: $DEC_TOKEN
    contexts:
    - name: default-context
      context:
        cluster: default-cluster
        namespace: default
        user: default-user
    current-context: default-context
    "@


    Write-Host "Finished"
    ```

2.   Run the following commands **[in PowerShell](https://docs.microsoft.com/en-us/powershell/)** to replace the current `kubeconfig` file:


    > You can use `Set-ExecutionPolicy Unrestricted` to change the execution policy if needed.

    ```PowerShell
     .\temp.ps1  
     cp .\kubeconfig.yaml  ~/.kube/config
    ```

    > The default location for `kubeconfig` might vary depending on your `kubectl` installation.


[OPTION END]


[OPTION BEGIN [Mac and Linux]]

1.   Create a temporary file `temp.sh` that will fetch all required information and print the content of the `kubeconfig` file.

    ```Shell
    # API server URL is api.KYMA_CLUSTER_DOMAIN
    API_SERVER_URL=$(kubectl config current-context)

    # the name of the secret containing the service account token goes here
    SECRET_NAME=$(kubectl get sa -n tutorial tutorial-service-account -ojsonpath='{.secrets[0].name}')

    CA=$(kubectl get secret/${SECRET_NAME} -n tutorial -o jsonpath='{.data.ca\.crt}')
    TOKEN=$(kubectl get secret/${SECRET_NAME} -n tutorial -o jsonpath='{.data.token}' | base64 --decode)

    echo "
    apiVersion: v1
    kind: Config
    clusters:
    - name: default-cluster
      cluster:
        certificate-authority-data: ${CA}
        server: https://api.${API_SERVER_URL}
    users:
    - name: default-user
      user:
        token: ${TOKEN}
    contexts:
    - name: default-context
      context:
        cluster: default-cluster
        namespace: default
        user: default-user
    current-context: default-context
    "
    ```

2.   Run the following commands to replace the current `kubeconfig` file:

    ```bash
    chmod +x temp.sh
    ./temp.sh > tutorial-kubeconfig.yaml  
    cp tutorial-kubeconfig.yaml ~/.kube/config
    ```

    > Alternatively, you can write the content to a new file and export the path to this file `export KUBECONFIG=./tutorial-kubeconfig.yaml`.
    >
    > The default location for `kubeconfig` might vary depending on your `kubectl` installation.



[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Read the config map)]

Besides the service account, you also created `ConfigMap` in step 2. Let's try to read a value from this map to check if the new `kubeconfig` is working:
```Shell
kubectl get configmap -n tutorial tutorial-config-map -o jsonpath='{.data.out}'
```


[VALIDATE_1]
[ACCORDION-END]


---
