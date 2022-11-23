---
parser: v2
author_name: Iwona Hahn
author_profile: https://github.com/iwonahahn
keywords: cap
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp\\, kyma-runtime, software-product>sap-fiori]
primary_tag: software-product-function>sap-cloud-application-programming-model
---

# Add Helm Chart
<!-- description --> Learn how to add a Helm chart to your project and configure container image, pull secret, cluster domain, and SAP HANA secret in the Helm chart.

## Prerequisites
 - [Prepare User Authentication and Authorization (XSUAA) Setup](btp-app-kyma-prepare-xsuaa)


## You will learn
 - How to add a Helm chart to your project
 - How to configure container image
 - How to configure pull secret
 - How to configure cluster domain
 - How to configure SAP HANA secret


---

### Add Helm chart

1. In the root directory of your project, run:

    ```
    cds add helm
    ```

    This creates a directory `chart` with the CAP Helm chart in your project directory.

---
### Configure Container Image

1. Open the file `chart/values.yaml`.

2. Replace the placeholder `<your-container-registry>` with your docker server URL.

    ```YAML[3,8]
    srv:
        image:
            repository: <your-container-registry>/cpapp-srv
            tag: latest
    ...
    hana_deployer:
        image:
            repository: <your-container-registry>/cpapp-hana-deployer
            tag: latest
    ```

    > Looking for your docker server URL?

    > The docker server URL is the same as provided in `Step 6: Create container registry secret` of [Prepare Your Kyma Development Environment](btp-app-kyma-prepare-dev-environment). It's also the path used for docker login, so you can quickly check it by running the following command in your terminal:

    > ```json
    > cat ~/.docker/config.json
    > ```

---
### Configure pull secret

1. In the `chart/values.yaml` file, make sure that the pull secret is defined:

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



---
### Configure cluster domain

The HTML5 applications need the Internet-accessible URL of the CAP service. For that the Helm chart needs to know the domain name to access the cluster.

1. Get the host name pattern of the cluster with the following command:

    ```YAML
    kubectl get gateway -n kyma-system kyma-gateway -o jsonpath='{.spec.servers[0].hosts[0]}'
    ```

    Result should look like:

    ```
    *.c-<xyz123>.sap.kyma.ondemand.com
    ```

    >  `<xyz123>` is a placeholder for a string of characters that's unique for your cluster.

2. Add the result without the leading `*.` in the `domain` property of your `chart/values.yaml` file. For example:

    ```YAML[2]
    global:
        domain: c-<xyz123>.sap.kyma.ondemand.com
    ```

---
### Configure SAP HANA secret

2. In the `chart/values.yaml` file, add the binding `db` pointing to the SAP HANA HDI container secret:

    ```YAML[5-6]
    srv:
        bindings:
            auth:
              ...
            db:
              fromSecret: cpapp-db
    ```

3. Point the binding `hana` of the SAP HANA deployer to the SAP HANA HDI container secret:

    ```YAML[5]
    hana_deployer:
        ...
        bindings:
            hana:
                fromSecret: cpapp-db
    ```

The result of this tutorial can be found in the [`kyma-add-helm-chart`](https://github.com/SAP-samples/cloud-cap-risk-management/tree/kyma-add-helm-chart) branch.
---
