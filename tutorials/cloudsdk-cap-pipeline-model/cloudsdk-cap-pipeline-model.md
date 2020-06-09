---
title: Continuous Delivery Pipeline for SAP Cloud Application Programming Model
description: Use the SAP Cloud SDK Continuous Integration and Continuous Delivery toolkit for SAP Cloud Application Programming Model.
time: 15
tags: [ tutorial>intermediate, products>sap-s-4hana-cloud-sdk]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
 - [Set up CI/CD](https://developers.sap.com/tutorials/cloudsdk-ci-cd.html)
 - Please have a look for setting up a [SAP Cloud Application Programming Model](https://cap.cloud.sap/docs/get-started/)

## Details
### You will learn
  - How to set up CI/CD pipeline for SAP Cloud Application Programming Model on SAP Cloud Platform

SAP Cloud Application Programming Model enables you to quickly create business applications by allowing you to focus on your business domain. It offers a consistent end-to-end programming model for full-stack development on SAP Cloud Platform.

You can use a sophisticated CI/CD pipeline without having to write or maintain the pipeline yourself. Rather, you can take advantage of an open-sourced pipeline, maintained by the SAP Cloud SDK team on [GitHub](https://github.com/SAP/cloud-s4-sdk-pipeline).

---

[ACCORDION-BEGIN [Step 1: ](Create a project)]

>If you had already set up a project you can skip this step.

For local development, you need to install the `cds` command line tools once, which in turn require Node.js , as follows:

1. Install Node.js from <https://nodejs.org>
  (use the latest LTS release)

2. Set the NPM registry for @sap packages

    ```
    npm set @sap:registry=https://npm.sap.com
    ```

3. Install the `cds` development kit globally

    ```
    npm i -g @sap/cds-dk
    cds  #> test-run it
    ```
4. Get the sample project

    ```
    cds init --add java,mta,bookshop
    ```

For more details regarding a project, please refer to [Cloud Application Model](https://cap.cloud.sap/docs/get-started/)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Set up a pipeline)]

SAP offers a pipeline which helps you to implement continuous delivery out-of-the-box. To add this pipeline to your project run the following command

```
cds add pipeline
```

Now, you can see that your project contains two new files.

-  Jenkinsfile
-  pipeline_config.yml


The SAP Cloud SDK for Continuous Delivery is based on Jenkins. Jenkins offers the possibility to implement build pipelines as code. However, as explained before, you don't have to write any line of code. As you can see in `Jenkinsfile`, the actual pipeline implemented as open source and executed here. We call this approach a centrally maintained pipeline.

!![Jenkinsfile](jenkinsFile.png)

Set `pipelineVersion` to a fixed released version (e.g. "v15") when running in a productive environment.

To find out about available versions and release notes, visit: <https://github.com/SAP/cloud-s4-sdk-pipeline/releases>

However, You still want to control the behavior of this pipeline. Therefore, the command adds a configuration file `pipeline_config.yml`, which new location will be `.pipeline/config.yml`.

You can use the `config.yml`, for example, to control where to deploy your application productively. Of course, there are many more configuration options you can use in order to control the behavior of the pipeline. You will find more information about this in the pipeline documentation.

Now, You'll need to push the code to a git repository. This is required because the pipeline gets your code via git. This might be GitHub, or any other cloud or on-premise git solution you have in your company.

!![GitHub repository](gitRepo.png)

Having an example ready which already contains the build pipeline, we need something to run the pipeline.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Start Jenkins server)]

Next, start your build server instance. We'll utilize the so called Cx-Server for that, which is developed by SAP's project "piper". It is based on popular and battle-tested open source components, such as Jenkins, Docker and Nexus. Thanks to the Cx-Server provided life-cycle management scripts, it is ready to work within a few minutes.

To start the build server, you just need a machine which has Docker installed. Create a new directory and initialize Cx-Server by using this `docker run` command:

```
docker run -it --rm -u $(id -u):$(id -g) -v "${PWD}":/cx-server/mount/ ppiper/cx-server-companion:latest init-cx-server
```

This creates a few files in your current working directory. The shell script Cx-Server and the configuration file `server.cfg` are of special interest.

Now, you can start the Jenkins server by using the following command:

```
chmod +x ./cx-server
./cx-server start
```

Congratulations! Your Jenkins is now starting up. Once it is running, you can open it by entering the IP or domain name of your host machine in your favorite browser.

>If Jenkins asks you to login for creating a new job so in terminal run this command `./cx-server initial-credentials` to find the default credentials.

> **IMPORTANT:** We recommend to change the default password immediately.

Jenkins should welcome you with the following screen:

!![Jenkins Welcome](jenkins-welcome.png)

Next, you can continue with the basic setup and start building your project by adding your source code repository.

>For configure the SAP cloud SDK Cx-Server, please follow the [Cx-Server operations guide](https://github.com/SAP/devops-docker-cx-server/blob/master/docs/operations/cx-server-operations-guide.md).


### Create a Jenkins job for your project
To create a build job for your project, navigate to **New Item** in the Jenkins main menu. On the following screen, choose **Multi-branch Pipeline** and specify a name for your project´s build job.

![Jenkins Create Build](jenkins_create_build.png)

If your repository is set to private, please also create and use a pair of suitable credentials and choose them for your repository. If you are using github.com, this will also protect you from running into rate limitations.

Next, you need to connect your project on Jenkins for running the build. Switch to Git and copy the `URL` of project repository. Look for **Branch Sources** at Jenkins screen and go to **Add Source** and add the link of your Git repository and click **Save**.

Jenkins will now scan your repository and trigger a build.

The best way to get an overview of the build status is to open the job, navigate to the currently running build, and open it in the blue ocean UI of Jenkins.

!![Jenkins Blue Ocean](jenkins-blue-ocean.png)

Please have a look at this sample visual representation of the pipeline:

!![Cloud SDK](cappipeline.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Set up credentials)]

As an option, if you already built integration tests that require credentials, you also need to create corresponding records in the Jenkins credentials store and map them via an appropriate entry in the configuration file. Credentials can be created by navigating to **Credentials > System > Global Credentials**.

!![Jenkins System](jenkins-system.png)

On this screen, click **Add Credentials** and enter username, password, as well as an ID for your credentials record.

!![Add Credentials](jenkis-add-credentials.png)

As a result, navigating to **Credentials** should show your freshly created entry.

!![Jenkins Credentials](jenkins-credentials.png)

Finally, you can now leverage this credentials record by adding the credentials configuration property to the `integrationTests` stage of your `config.yml`.

The example below shows the mapping to the system alias `MyErpSystem`, which you defined earlier in your `systems.yml` file.

```YAML
stages:
  integrationTests:
    credentials:
      - alias: 'MyErpSystem'
        credentialId: 'MY-ERP'
```

To learn more about pipeline configuration, feel free to have a look on our [documentation on GitHub](https://github.com/SAP/cloud-s4-sdk-pipeline/blob/master/configuration.md).

Alternatively, you can run SAP Cloud SDK Continuous Delivery Pipeline also on a Kubernetes cluster, if you wish to do so. An in-depth guide can be found in this [blog post](https://blogs.sap.com/2018/09/26/autoscaling-of-sap-s4hana-cloud-sdk-continuous-delivery-toolkit-on-kubernetes/).


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Production deployment)]

As explained you can configure the behaviour of the pipeline using the file `.pipeline/config.yml`. In this step, we will configure the production deployment as an example.

In order to deploy applications, you need to create a free trial account. You can create your account by visiting [SCP Cloud Foundry](https://cloudplatform.sap.com/try.html)

After creating your account and activating it via email, you can log in to your personal `Cloud Cockpit`. For your first visit, it should look like this:

![SAP Cloud Platform Cockpit first view](cloudplatform-cockpit-first-view.png)

Now click the `Home` button in the upper navigation bar and then click `Start Cloud Foundry Trial`.

![SAP Cloud Platform Cockpit start trial view](cloudplatform-cockpit-start-cf-trial1.png)

After selecting your region, your account will be automatically set up for development with `Cloud Foundry`.

Now that your account is activated and configured.

In order to deploy applications on `SAP Cloud Foundry` you need to provide the API endpoint. The API endpoint depends on the region you chose for your account:

  - for EU: `https://api.cf.eu10.hana.ondemand.com`
  - for US EAST: `https://api.cf.us10.hana.ondemand.com`
  - for US CENTRAL: `https://api.cf.us20.hana.ondemand.com`


Configure the `Jenkinsfile` and `config.yml` for Cloud Foundry settings.

```
#Jenkinsfile
general:
  cloudFoundry:
    org: 'myorg'
    space: 'Prod'
    apiEndpoint: '<Cloud Foundry API endpoint>'
    credentialsId: 'CF-DEPLOY-DEFAULT'
    manifestVariablesFiles: ['manifest-variables.yml']
stages:
  productionDeployment:
    appUrls:
      - url: <application url>
        credentialId: e2e-test-user-cf
    cfCreateServices:
      - serviceManifest: 'services-manifest.yml'
      - serviceManifest: 'services-manifest.yml'
        space: 'Prod2'
        org: 'myorg2'
    cfTargets:
        space: 'my-space'
        org: 'my-org'
        credentialsId: 'credentials'
        apiEndpoint: '<Cloud Foundry API endpoint>'
```        

```
#config.yml
productionDeployment:
  appUrls:
   - url: <application url>
     credentialId: e2e-test-user-cf
  cfTargets:
   - space: 'my-space'
     org: 'my-org'
     manifest: 'manifest.yml'
     credentialsId: 'credentials'
     apiEndpoint: '<Cloud Foundry API endpoint>'
```

For further details regarding stage production deployment, Please refer to [pipeline documentation]( https://github.com/SAP/cloud-s4-sdk-pipeline/blob/master/configuration.md#productiondeployment)

For a detailed documentation of the individual properties please consult the [Step documentation](https://sap.github.io/jenkins-library/steps/cloudFoundryCreateService/).

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 6: ](More information)]

This picture puts everything into context of the broader SAP ecosystem:

!![Cloud SDK](capSDK.png)

As you can see, [SAP Cloud SDK](https://developers.sap.com/topics/cloud-sdk.html) is a natural companion for SAP Cloud Application Programming Model applications, providing useful features like tight integration with SAP LoB solutions such as SAP S/4HANA and SAP SuccessFactors.

More detailed information on the qualities checked by the pipeline can be found [here](https://sap.github.io/jenkins-library/pipelines/cloud-sdk/cloud-qualities/)


[DONE]
[ACCORDION-END]
