---
title: Enable CAP Operator Community Module in Kyma Cluster
description: Learn how to enable the CAP Operator community module in your Kyma cluster.
parser: v2
auto_validation: true
time: 5
tags: [ tutorial>beginner, software-product>sap-cap-operator--kubernetes-environment, topic>cloud-operations, software-product-function>sap-cloud-application-programming-model, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-btp--kyma-runtime]
primary_tag: software-product>sap-cap-operator--kubernetes-environment
author_name: Anirudh Prasad
author_profile: https://github.com/anirudhprasad-sap
---

## You will learn

- How to enable the CAP Operator community module in your Kyma cluster.

## Prerequisites

- You've enabled the Kyma runtime in your subaccount. Follow the steps in the [Setting Up SAP BTP and Kyma Runtime for Deployment](cap-operator-01-prepare) tutorial that is part of the [Application Lifecycle Management using CAP Operator](group.kyma-cap-operator-lifecycle) tutorial group.
- You have an [enterprise global account](https://help.sap.com/docs/btp/sap-business-technology-platform/getting-global-account#loiod61c2819034b48e68145c45c36acba6e) in SAP BTP. To use services for free, you can sign up for an SAP BTPEA (SAP BTP Enterprise Agreement) or a Pay-As-You-Go for SAP BTP global account and use the free tier services only. See [Using Free Service Plans](https://help.sap.com/docs/btp/sap-business-technology-platform/using-free-service-plans?version=Cloud).
- You have a platform user. See [User and Member Management](https://help.sap.com/docs/btp/sap-business-technology-platform/user-and-member-management).
- You're an administrator of the global account in SAP BTP.
- You have a subaccount in SAP BTP to deploy the services and applications.

### Enable CAP Operator community module

1. Open your Kyma dashboard.

2. Navigate to **Configuration** &rarr; **Modules** and choose **Add** within the **Community Modules** list.

    <!-- border; size:540px --> ![Add Community Module](./img/community-module-1.png)

3. Choose **Add** in the **Source YAMLs** section to load the list of community modules.

    <!-- border; size:540px --> ![Load Community Modules](./img/community-module-2.png)

4. In the popup, you can see the list of available community modules. Choose **Add**.

    <!-- border; size:540px --> ![Select CAP Operator Module](./img/community-module-3.png)

5. Select the **CAP Operator** module and choose **Add**.

    <!-- border; size:540px --> ![Add CAP Operator Module](./img/community-module-4.png)

6. Wait until the automatic installation is complete and the **Module State** changes to **Ready**.

    <!-- border; size:540px --> ![CAP Operator Installed](./img/community-module-5.png)
