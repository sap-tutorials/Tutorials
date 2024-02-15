---
author_name: Mahati Shankar
author_profile: https://github.com/smahati
title: Add Helm Chart
description: Learn how to add a Helm chart to your project and configure container image, pull secret, cluster domain, and SAP HANA secret in the Helm chart.
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp\\, kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

## Prerequisites
 - [Prepare User Authentication and Authorization (XSUAA) Setup](btp-app-kyma-prepare-xsuaa)


## Details
### You will learn
 - How to add a Helm chart to your project
 - How to configure container image
 - How to configure pull secret
 - How to configure cluster domain
 - How to configure SAP HANA secret


---
> This tutorial will soon be phased out. 
> 
> For more tutorials about how to develop and deploy a full stack CAP application on SAP BTP, see:
>
> - [Develop a Full-Stack CAP Application Following SAP BTP Developer’s Guide](https://developers.sap.com/group.cap-application-full-stack.html)
> - [Deploy a Full-Stack CAP Application in SAP BTP, Cloud Foundry Runtime Following SAP BTP Developer’s Guide](https://developers.sap.com/group.deploy-full-stack-cap-application.html)
> - [Deploy a Full-Stack CAP Application in SAP BTP, Kyma Runtime Following SAP BTP Developer’s Guide](https://developers.sap.com/group.deploy-full-stack-cap-kyma-runtime.html)
>
> To continue learning how to implement business applications on SAP BTP, see:
>
> - [SAP BTP Developer’s Guide](https://help.sap.com/docs/btp/btp-developers-guide/what-is-btp-developers-guide?version=Cloud&locale=en-US)
> - [Related Hands-On Experience](https://help.sap.com/docs/btp/btp-developers-guide/related-hands-on-experience?version=Cloud&locale=en-US)
> - [Tutorials for ABAP Cloud](https://help.sap.com/docs/btp/btp-developers-guide/tutorials-for-abap-cloud?version=Cloud&locale=en-US)
> - [Tutorials for SAP Cloud Application Programming Model](https://help.sap.com/docs/btp/btp-developers-guide/tutorials-for-sap-cloud-application-programming-model?version=Cloud&locale=en-US)

[ACCORDION-BEGIN [Step 1: ](Add Helm chart)]
1. In the root directory of your project, run:

    ```Shell/Bash
    cds add helm
    ```

    This creates a directory `chart` with the CAP Helm chart in your project directory.

[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 2: ](Configure container image)]
1. Open the file `chart/values.yaml`.

2. Replace the placeholder `<your-container-registry>` with your docker server URL.

    ```YAML[3,8]
    srv:
        image:
            repository: <your-container-registry>/cpapp-srv
            tag: latest
    ...
    hana-deployer:
        image:
            repository: <your-container-registry>/cpapp-hana-deployer
            tag: latest
    ```

    > In case you're using Docker Hub as your container registry, replace the placeholder `<your-container-registry>` with your Docker Hub user ID.



[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 3: ](Configure pull secret)]
1. In the `chart/values.yaml` file, define the pull secret as the one you created before:

    ```YAML[4]
    global:
        domain: null
        imagePullSecret:
            name: container-registry
    srv:
        ...
        bindings:
            ...
        image:
            repository: <your-container-registry>/cpapp-srv
            ...
    ```
    > The name of the secret created in `Step 6: Create container registry secret` of [Prepare Your Kyma Development Environment](btp-app-kyma-prepare-dev-environment) and the entry for `imagePullSecret` should match.



[DONE]
[ACCORDION-END]
---
[ACCORDION-BEGIN [Step 4: ](Configure cluster domain)]
The HTML5 applications need the Internet-accessible URL of the CAP service. For that the Helm chart needs to know the domain name to access the cluster.

1. Get the host name pattern of the cluster with the following command:

    ```Shell/Bash
    kubectl get gateway -n kyma-system kyma-gateway -o jsonpath='{.spec.servers[0].hosts[0]}'
    ```

    Result should look like:

    ```Shell/Bash
    *.<xyz123>.kyma.ondemand.com
    ```

    >  `<xyz123>` is a placeholder for a string of characters that's unique for your cluster.

2. Add the result without the leading `*.` in the `domain` property of your `chart/values.yaml` file. For example:

    ```YAML[2]
    global:
        domain: <xyz123>.kyma.ondemand.com
    ```

[VALIDATE_1]
The result of this tutorial can be found in the [`kyma-add-helm-chart`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/kyma-add-helm-chart) branch.


[ACCORDION-END]
---