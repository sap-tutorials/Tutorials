---
author_name: René Jeglinsky
author_profile: https://github.com/renejeglinsky
auto_validation: true
primary_tag: software-product-function>sap-cloud-application-programming-model
tags: [ tutorial>beginner, programming-tool>node-js, software-product>sap-business-technology-platform, software-product>sap-fiori, software-product>sap-hana-cloud, software-product-function>sap-cloud-application-programming-model ]
time: 30
parser: v2
---

# Deploy a CAP Business Service to SAP Business Technology Platform
<!-- description --> This tutorial shows you how to deploy your SAP Cloud Application Programming Model (CAP) application to SAP Business Technology Platform (BTP), Cloud Foundry environment using SAP HANA Cloud service.

## You will learn
  - How to deploy your CAP business service on SAP BTP and binding appropriate service instances. See the [Developer Guide for Cloud Foundry](https://docs.cloudfoundry.org/devguide/) for more details

## Prerequisites
- You've finished the tutorial [Create a CAP Business Service with Node.js using Visual Studio Code](cp-apm-nodejs-create-service).
- If you don't have a Cloud Foundry Trial subaccount and dev space on [SAP BTP](https://cockpit.hanatrial.ondemand.com/cockpit/) yet, create your [Cloud Foundry Trial Account](hcp-create-trial-account) with **US East (VA) as region** and, if necessary [Manage Entitlements](cp-trial-entitlements).
- You've downloaded and installed the [cf command line client](https://github.com/cloudfoundry/cli#downloads) for Cloud Foundry as described in the tutorial [Install the Cloud Foundry Command Line Interface (CLI)](cp-cf-download-cli).
- You've downloaded and installed the [MBT Built Tool](https://sap.github.io/cloud-mta-build-tool/download/).
- You've downloaded and installed the [MultiApps CF CLI plugin](https://github.com/cloudfoundry/multiapps-cli-plugin/blob/master/README.md).
- You've to [Use an existing SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#42a0e8d7-8593-48f1-9a0e-67ef7ee4df18) or [set up a new SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#3b20e31c-e9eb-44f7-98ed-ceabfd9e586e) to deploy your CAP application.

---

## Intro

It's now time to switch to SAP HANA as a database and prepare your project for an MTA deployment to SAP BTP Cloud Foundry. To continue with this tutorial you need to [Use an existing SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#42a0e8d7-8593-48f1-9a0e-67ef7ee4df18) or [set up a new SAP HANA Cloud service instance](https://developers.sap.com/tutorials/btp-app-hana-cloud-setup.html#3b20e31c-e9eb-44f7-98ed-ceabfd9e586e) to deploy your CAP application.

> Your SAP HANA Cloud service instance will be automatically stopped overnight, according to the server region time zone. That means you need to restart your instance every day, before you start working with your trial.


### Enhance project configuration for production

1. If `cds watch` is still running in VS Code, choose <kbd>Ctrl</kbd> + <kbd>C</kbd> in the command line to stop the service.

2. To prepare the project, execute in the root level of your project in VS Code:

    ```Shell/Bash
    cds add hana,mta,xsuaa,approuter --for production
    ```

    > `--for production` adds all configuration added by this command in the `package.json` file into a `cds.requires.[production]` block.

    > `hana` configures deployment for SAP HANA, so a data source of type `hana` is added in the `cds.requires.[production].db` block. See section [Node.js configuration](https://cap.cloud.sap/docs/node.js/cds-env#profiles) in the CAP documentation for more details.

    > `mta` adds the `mta.yaml` file. This file reflects your project configuration.

    > `xsuaa` creates an `xs-security.json` and also the needed configuration in the `mta.yaml` file. An authentication of kind `xsuaa` is added in the `cds.requires.[production].auth` block.

    > `approuter` adds the configuration and needed files for a standalone AppRouter so that the authentication flow works after deployment.

    Learn more about those steps in the [Deploy to Cloud Foundry](https://cap.cloud.sap/docs/guides/deployment/to-cf#prepare-for-production) guide in the CAP documentation.

3. Update your dependencies, as with the previous command you added some dependencies to your `package.json`.

    ```Shell/Bash
    npm update --package-lock-only
    ```

4. Following this tutorial strictly, you don't have an own UI yet in your project. In this case you need to open the `app/xs-app.json` file and remove the `welcomeFile` property. Otherwise you'll run into a `Not Found` error after deployment as an `index.html` file is requested that is not available.

    > For productive applications this is different and the command `cds add approuter` is of course tailored for productive applications. That's why we need this extra step here in this starter tutorial.

5. (Optional) To enable SAP Fiori preview add the following configuration in the `package.json` of your `my-bookshop` project in VS Code:

    ```JSON
    "cds": {
      "features": {
        "fiori_preview": true
      },
    }

    ```
    > `fiori_preview:true` enables SAP Fiori preview also in `production` mode as you saw it in your local application in the previous tutorial in step 4 when using `cds watch`. This feature is meant to help you during development and should not be used in productive applications.

    > Don't edit the `gen/db/package.json` file.



### Identify SAP BTP Cloud Foundry endpoint

The Cloud Foundry API endpoint is required so that you can log on to your SAP BTP Cloud Foundry space through Cloud Foundry CLI.

1. Go to the [SAP BTP Trial Cockpit](https://cockpit.hanatrial.ondemand.com/cockpit#/home/trial) and choose **Go To Your Trial Account**.

    <!-- border -->![business technology platform cockpit view](cockpit.png)

2. From the **Account Explorer** overview navigate to your subaccount.

    <!-- border -->![subaccount tile](subaccount.png)

3. From your subaccount copy the **Cloud Foundry Environment API Endpoint** value.

    <!-- border -->![CF API endpoint value](api_endpoint.png)

4. Go back to VS Code to the command line. Authenticate with your login credentials using the following command:

    ```Shell/Bash
    cf login
    ```

    > For this you need the cf command line client, see the prerequisites.

    > This will ask you to select Cloud Foundry API, org, and space.

    > The API endpoint is taken by default. If you want to change the API endpoint use `cf api <CF_API_ENDPOINT>` to change the API. Replace `<CF_API_ENDPOINT>` with the actual value you obtained in the previous step.

    > If you don't know whether you're logged on to Cloud Foundry or if you're wondering to which Cloud Foundry org and space are you logged on, you can always use `cf target` in a terminal to find out.

### Deploy using cf deploy

SAP provides an application format that respects the single modules and their technologies and keeps those modules in the same lifecycle: [Multitarget Application](https://help.sap.com/docs/BTP/65de2977205c403bbc107264b8eccf4b/d04fc0e2ad894545aebfd7126384307c.html?version=Cloud)

The MBT Build tool uses the `mta.yaml` file that has been created using `cds add mta` before, to build the deployable archive. The MultiApps CF CLI plugin adds the `deploy` command and orchestrates the deployment steps.

> In the previous step, you identified your API endpoint. If that has a format like `...us10-001...`, you need to add the following configuration to your `xs-security.json`:
> ```json
> "oauth2-configuration": {
>     "redirect-uris": ["https://*.us10-001.hana.ondemand.com/**"]
> }
> ```
> For other API endpoints you'd need to adapt it accordingly.

1. In VS Code, in the root of your project, execute the following command to build the archive.
    ```Shell/Bash
    mbt build -t gen --mtar mta.tar
    ```

    > For this you need the MBT Build Tool, see the prerequisites.

    The `-t` option defines the target folder of the build result as the `gen` folder of your project. As part of this build implicitly `cds build --production` is executed. This implicit build uses then all the configuration you've added in the step 1.2 when using `--for production`.

2. Deploy the archive using `cf deploy`.
    ```Shell/Bash
    cf deploy gen/mta.tar
    ```

    > For this you need the MultiApps CF CLI plugin, see the prerequisites.

    During deployment all needed service instances are created and the applications as well as database artifacts are deployed.

    > This process takes some minutes. In this one step the archive is uploaded to Cloud Foundry, service instances are created, the applications are staged, and then deployed to their target runtimes.

    > If your deployment fails during deploy of `my-bookshop-db-deployer`, make sure that your IP address is configured for connections to SAP HANA Cloud. Or allow connections to all IP addresses at your own risk. We recommend to revert that setting after you've completed the tutorial.

3. In the deploy log, find the application URL of `my-bookshop`:

    ```Shell/Bash
    Application "my-bookshop" started and available at "[org]-[space]-my-bookshop.cfapps.[region].hana.ondemand.com"
    ```
    This is the URL of the AppRouter, which enforces the authentication flow.

4. Open this URL in the browser and try out the provided links, for example, `.../catalog/Books`. Application data is fetched from SAP HANA. If enabled in step 1.4 you can also try the **Fiori preview**.

    <!-- border -->![application preview](application_cloud_fiori.png)

> What you achieved
>
> You've deployed your CAP application as multitarget application including deployment to SAP HANA Cloud, using the standalone AppRouter, and using authentication (XSUAA). Congratulations!
