---
title: Prepare Multi-Tenant Applications for Deployment with CAP Operator
description: This tutorial shows you how to prepare your multi-tenant application for deployment in SAP BTP, Kyma runtime using the CAP Operator.
parser: v2
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-cap-operator--kubernetes-environment, topic>cloud-operations, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp--kyma-runtime]
primary_tag: software-product>sap-cap-operator--kubernetes-environment
author_name: Anirudh Prasad
author_profile: https://github.com/anirudhprasad-sap
---

## You will learn

- How to build container images for your multi-tenant application and push them to a container registry.

## Prerequisites

- You've configured the respective entitlements, enabled the Kyma runtime in your SAP BTP subaccount, and created an SAP HANA Cloud service instance in the SAP BTP cockpit. Follow the steps in the [Setting Up SAP BTP and Kyma Runtime for Deployment](cap-operator-01-prepare) tutorial that is part of the [Application Lifecycle Management using CAP Operator](group.kyma-cap-operator-lifecycle) tutorial group.
- You've installed all the required tools. Follow the steps in the [Install Tools for Deployment](cap-operator-02-tools) tutorial that is part of the [Application Lifecycle Management using CAP Operator](group.kyma-cap-operator-lifecycle) tutorial group.

### Download and set up the project locally

Clone the application from the [Incident Management Application GitHub Repository](https://github.com/cap-js/incidents-app/tree/cap-operator-tutorials). For e.g. using:
```bash
git clone https://github.com/cap-js/incidents-app.git -b cap-operator-tutorials
```

This is a multi-tenant SAP Cloud Application Programming Model (CAP) application. It utilizes the [application router](https://www.npmjs.com/package/@sap/approuter) for routing, SAP Authorization and Trust Management service (XSUAA), and SAP HANA Cloud as the database. The front end is built with SAP Fiori and deployed to the HTML5 Application Repository.

Open a command-line window in the folder where your application holds **incidents-app** and run the following command to open the project in Visual Studio (VS) Code:

```bash
code .
```

### Build images

> Make sure you're logged in to your container registry. In case if you don't have access to a container registry, you can make use of the [Docker Registry Community Module](https://kyma-project.io/external-content/docker-registry/docs/user/README.html) from Kyma.

> If you're using a device with a non-x86 processor (for example, MacBook M1/M2), you need to instruct Docker to use x86 images by setting the **DOCKER_DEFAULT_PLATFORM** environment variable using the command `export DOCKER_DEFAULT_PLATFORM=linux/amd64`. Check the [environment variables](https://docs.docker.com/engine/reference/commandline/cli/#environment-variables) for more information.

> Make sure to replace `<your-container-registry>` with the link to your container registry and keep in mind that `<image version>` is a string.

> Looking for your Docker server URL?

> The Docker server URL is the same as the path used for Docker login, so you can quickly check it by running the following command in your terminal:

> ```json
> cat ~/.docker/config.json
> ```

> In case you're using Docker Hub as your container registry, replace the placeholder `<your-container-registry>` with your Docker Hub user ID.

#### Build the CAP Node.js and the MTXS sidecar image

1. In VS Code, choose **Terminal** &rarr; **New Terminal** and run the following command:

    ```bash
    npm install
    ```

    This command installs the required dependencies and updates the **package-lock.json** file of your project.

2. Create the productive CAP build for your application:

    ```bash
    npx cds build --production
    ```

    The CAP build writes to the **gen/srv** folder.

3. Build the CAP Node.js image:

    ```bash
    pack build <your-container-registry>/incident-management-srv:<image-version> \
        --path gen/srv \
        --builder paketobuildpacks/builder-jammy-base \
        --publish
    ```

    > The pack CLI builds the image that contains the build result in the **gen/srv** folder and the required npm packages by using the [Cloud Native Buildpack for Node.js](https://github.com/paketo-buildpacks/nodejs) provided by Paketo.

4. Build the MTXS sidecar image:

    ```bash
    pack build <your-container-registry>/incident-management-mtxs-sidecar:<image-version> \
        --path gen/mtx/sidecar \
        --builder paketobuildpacks/builder-jammy-base \
        --publish
    ```

    > **IMPORTANT:** The **project.toml** file in the **gen/mtx/sidecar** folder is copied automatically from the **mtxs/sidecar** folder during the build. This file exposes the node process inside the container so that CAP Operator can trigger tenant operations using the MTXS CLIs.

#### Build the application router image

1. In the VS Code terminal, navigate to the **app/router** folder and run the following command:

    ```bash
    npm install
    ```

2.  In the VS Code terminal, navigate back to the root folder of your project:

    ```bash
    cd ../..
    ```

3. Build the application router image:

    ```bash
    pack build <your-container-registry>/incident-management-approuter:<image-version> \
        --path app/router \
		--buildpack paketo-buildpacks/nodejs \
		--builder paketobuildpacks/builder-jammy-base \
		--env BP_NODE_RUN_SCRIPTS="" \
		--publish
    ```

#### Build the HTML5 deployer image

1.  In the VS Code terminal, navigate to the **ui-resources** folder and run the following command:

    ```bash
    npm install && npm run package
    ```

    This command builds and copies the archive **nsincidents.zip** inside the **ui-resources/resources** folder.

2.  In the VS Code terminal, navigate back to the root folder of your project:

    ```bash
    cd ..
    ```

3. Build the UI deployer image:

    ```bash
    pack build <your-container-registry>/incident-management-html5-deployer:<image-version> \
        --path ui-resources \
        --builder paketobuildpacks/builder-jammy-base \
        --publish
    ```
