---
title: Enable SAP BTP, Kyma Runtime
description: The SAP Kyma runtime is a Kubernetes cluster which is comprised of a collection of projects united together to simplify the extension, and integration of software.
time: 10
auto_validation: true
tags: [ tutorial>beginner, topic>cloud, software-product>sap-business-technology-platform]
primary_tag: software-product>sap-btp\\, kyma-runtime
---

## Prerequisites
 - **Tutorials:** [Get an Account on SAP BTP to Try Out Free Tier Service Plans](btp-free-tier-account)

## Details
### You will learn
  - How to enable the SAP BTP, Kyma runtime using the SAP BTP cockpit (see [Set up a Kyma Cluster using the Command Line](btp-cli-setup-kyma-cluster) for the same procedure using the CLI)

---

[ACCORDION-BEGIN [Step 1: ](Enable the Kyma environment)]

1. In your Free Tier subaccount click **Enable Kyma** to trigger the enablement of the Kyma environment.

    !![Kyma Free Tier](cp-kyma-getting-started-01.png)

2. In the popup, enter your cluster name of choice and click **Create**.

    !![Kyma Free Tier](cp-kyma-getting-started-02.png)

    In the background, a new Kubernetes cluster is being set up where the Kyma runtime and all its components will run on. This might take between 15-25 min.

    To continue with this tutorial, you need to wait until the enablement is finished. You can still use the SAP BTP Free Tier or close the window as the setup process will continue automatically.

    !![Kyma Free Tier](cp-kyma-getting-started-03.png)

    To learn more about the Kyma environment and its functionality, see:

    - [SAP BTP, Kyma runtime](https://discovery-center.cloud.sap/serviceCatalog/kyma-runtime)
    - [SAP Help Portal - Kyma Environment](https://help.sap.com/viewer/3504ec5ef16548778610c7e89cc0eac3/Cloud/en-US/468c2f3c3ca24c2c8497ef9f83154c44.html)
    - [kyma-project](https://kyma-project.io/docs/kyma/latest)
    - [Kyma - YouTube](https://www.youtube.com/channel/UC8Q8bBtYe9gQN-dQ-_L8JvQ)
    - [Cloud Native for Beginners - YouTube](https://youtube.com/playlist?list=PL6RpkC85SLQCwaJ54TAAHMvSl5wpVPrai)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open Kyma runtime console UI)]

To open the Kyma runtime console UI, click on **Link to Dashboard** under the **Kyma Environment** section of the Subaccount Overview page.

!![Kyma Free Tier](cp-kyma-getting-started-04.png)

The Kyma Dashboard should open in a new Browser Tab.

The dashboard or Kyma console UI is your graphical playground for managing and deploying applications or services on the Kyma runtime. With the dashboard you can not only deploy or delete deployments but also manage them for scale, expose them over self-defined API Rules and much more.

To learn more about the capabilities and features of the SAP BTP, Kyma runtime, follow the Kyma tutorials, blog posts, read the documentation or check out the YouTube videos. If you aren't aware, there is also a [Kyma slack channel](https://kyma-community.slack.com/) where you can ask questions to the active community.

!![Kyma Free Tier](cp-kyma-getting-started-05.png)

[VALIDATE_2]
[ACCORDION-END]
