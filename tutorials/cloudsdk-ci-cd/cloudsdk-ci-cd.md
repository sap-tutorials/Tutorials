---
author_name: Benjamin Heilbrunn
author_profile: https://github.com/benhei
title: Set Up Continuous Integration and Delivery for SAP Cloud SDK
description: Use the General Purpose Pipeline of project "Piper" to implement CI/CD for an SAP Cloud SDK project.
auto_validation: true
time: 30
tags: [tutorial>intermediate, products>sap-cloud-sdk]
primary_tag: products>sap-cloud-sdk
---

## Prerequisites
For getting started with Continuous Integration and Delivery (`Cx`) in your project, you need to assure the following prerequisites:
    - **Linux host with Docker**: For instantiating the Cx-server of project "Piper", you need to provide a suitable host or virtual machine with a Linux operating system and Docker installed. Please also ensure that the user with whom you start the Cx-server belongs to the Docker group.
    - **Project sources in GitHub or Git**: Your project source files need to be available on a Git or GitHub server, which is accessible from the Cx-server host. Creating your project is explained in <https://developers.sap.com/group.s4sdk-cloud-foundry.html>.

The General Purpose Pipeline of project "Piper" uses Docker images for each individual build step. Accordingly, you do not need to take care of installing any further dependencies on your host machine. All tools required for building, testing, quality checking, and deploying your applications, are dynamically retrieved in the form of Docker images.

## Details
### You will learn
  - How to develop and release your application in short cycles
  - How to use project "Piper" to set up a CI/CD pipeline for SAP Cloud SDK based projects on SAP Cloud Platform
  - How to set up a CI/CD build server and run your pipeline

---

[ACCORDION-BEGIN [Step 1: ](Generate a new project using the SAP Cloud SDK for Java)]

First, generate a new project using the SAP Cloud SDK for Java which can later be deployed to SAP Cloud Platform via tools provided by project "Piper". If you already completed the tutorial [Create a Sample Application on Cloud Foundry Using SAP Cloud SDK](s4sdk-cloud-foundry-sample-application) before, you can also reuse the project during this tutorial.

Otherwise, the command to generate a new SAP Cloud SDK project is:

```Shell
mvn archetype:generate "-DarchetypeGroupId=com.sap.cloud.sdk.archetypes"\
"-DarchetypeArtifactId=scp-cf-tomee" "-DarchetypeVersion=RELEASE"
```

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 2: ](Install and manage the Cx-server)]
Cx-server is a build server based on Jenkins. Its lifecycle is maintained by a script called `cx-server`. It can be found in the same named folder on the root of each SAP Cloud SDK project archetype. Alternatively, you can also follow the first steps in the [Operations Guide for CX Server](https://github.com/SAP/devops-docker-cx-server/blob/master/docs/operations/cx-server-operations-guide.md#introduction). Together with the `server.cfg` file, this is all you need for starting your instance of the Cx-server.

!![CX Server Folder](cx-server-folder.png)

### Copy the Cx-server lifecycle management script to the target host
First, Cx-server folder needs to be copied to the future host on which the Cx-server is intended to run. For this, copy the folder from your archetype to a place on your Linux server, for example, via secure copy:

```Shell
scp -r cx-server username@cx-host.corp:~/cx-server
```

>If you are copying the Cx-server from a Microsoft Windows machine, the target files might use Windows line endings, which makes them unusable on the Linux host. To correct this, you can use `dos2unix` or similar tools to convert the line endings of the Cx-server script and its configuration in `server.cfg`. Also ensure that the Cx-server script is executable by the current user.

>Example:

>
```Shell
$ dos2unix ./cx-server/*
dos2unix: converting file ./cx-server/cx-server to Unix format...
dos2unix: converting file ./cx-server/server.cfg to Unix format...
$ chmod +x cx-server/cx-server
```

Next, log into your Linux host and navigate to the Cx-server folder. After that, you are ready to start your Cx-server instance.

### Start the Cx-server
For starting the Cx-server, all you need to do is to change to the Cx-server directory and execute the `cx-server start` command. This will start the Cx-server with its default settings on port 80 of your host machine.

If not done before, the script downloads the newest version of the Cx-server from Docker Hub and instantiates a container from it. Because of the initial download, the first invocation might take a few minutes. In the case where the Cx-server has been started and stopped earlier, the existing container will be reused.

The state of Jenkins will be stored in a persistent Docker volume named `jenkins_home_volume`. This volume contains all critical data, comprising the Jenkins configuration, plugins, and build related artifacts. Therefore, even if your container crashes, no data of your Cx-server will be lost.

If everything goes well, you should be able to see output, which is similar to the following:

```Shell
$ cd cx-server
$ ./cx-server start
Checking for newer version of this script on github...
no newer version detected.

Starting Docker container for Cx Server.
Parameters:
   - http_port=80
   - docker_image=ppiper/cx-jenkins-master:latest
   - jenkins_home=jenkins_home_volume

>> docker pull ppiper/cx-jenkins-master:latest
latest: Pulling from ppiper/cx-jenkins-master
...
e7ad665b2ea1: Already exists
9094d1930c09: Pull complete
1ae0fd7ec49a: Downloading [==>        ]  9.191MB/221MB
...
Status: Image is up to date for ppiper/cx-jenkins-master:latest
>> docker run -u 1000:115 --name cx-jenkins-master -d -p 80:8080
 -v /var/run/docker.sock:/var/run/docker.sock
 -v jenkins_home_volume:/var/jenkins_home
 ppiper/cx-jenkins-master-sap:latest
0e214f9dcf3d9e1d2839638901d60f3067edb635067c640fe16b7c18a6b65607
Waiting for the Cx server to start......... success.
```

Congratulations! Your Cx-server is now starting up. Once it is running, you can open it by entering the IP or domain name of your host machine in your favorite browser.

>Jenkins will ask you to login for creating a new job, so in terminal run this command `./cx-server initial-credentials` to find the default credentials.

> **IMPORTANT:** We recommend to change the default password immediately.

Jenkins should welcome you with the following screen:

!![Jenkins Welcome](jenkins-welcome.png)

Next, you can continue with the basic setup and start building your project by adding your source code repository.

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Update and customize the Cx-server)]

In certain cases, you will need to create a new Cx-server container, for example, for:

- Updating the Cx-server to a new version

- Updating the Cx-server for security reasons. Please refer to [Cx-server recommendation](https://github.com/SAP/devops-docker-cx-server/blob/master/cx-server-companion/life-cycle-scripts/CX-SERVER-SECURITY.md).

- Effectively applying configuration changes

- Troubleshooting

### Configure the SAP Cloud SDK Cx-server
You can use the `cx-server/server.cfg` file to customize your instance of the SAP Cloud SDK Cx-server. To get an overview of the available options, you can open the file and have a look at the explanatory comments of each option. Changes to the configuration will become effective after creating a new container. This means that, if you are already running an instance of the Cx-server, you have to stop and remove it before the new settings will become effective.

As of today, the following options can be customized:

- **`docker_registry` and `docker_image`**: The Docker registry and Docker image to be used for instantiating your Cx-server. When no `docker_registry` is specified, the Docker engine will use Docker hub as a default and pull the `ppiper/cx-jenkins-master` image in its latest version.
- **`http_port`**: The port on which your Cx-server will be reachable.
- **`jenkins_home`** : The resource to be used as Jenkins home directory. You can use an absolute path pointing to a folder on your host machine, for example, `jenkins_home="/var/jenkins_home"`.  Alternatively, you can use a named Docker data volume such as the default value `jenkins_home="jenkins_home_volume"`.
- **x`_java_opts`**: Custom java options to be passed to the Jenkins Java VM. You can use it to adapt the maximum heap size of its Java VM, for example, `java_opts="-Xmx1024m"`
- **`http_proxy`, `https_proxy`, `no_proxy`**: Proxy settings to be passed as environment variables to the Docker container, for example: `http_proxy="http://proxy:8080", http_proxy="http://proxy:8080", no_proxy="localhost,.corp"`

>If you choose to store Jenkins home in a local folder, you have to ensure that the Jenkins user within the Cx-server container, which is fixed to `uid=1000`, is able to read and write within the specified folder. To avoid permission related issues, we recommend to use named Docker volumes.

### Apply configuration or update the SAP Cloud SDK Cx-server

To apply your configuration changes you did in the last section or to fetch a new Cx-server version, you have to create a new instance.

For stopping your Cx-server instance, you can use the command `cx-server stop`. It will stop the Cx-server container in a safe manner by waiting for all jobs to finish before shutdown.

>Before the shutdown can be performed, you have to provide the credentials of a (technical) user who is enabled to perform a safe shutdown of the server.

```
$ ./cx-server stop
Checking for newer version of this script on github...
no newer version detected.

Jenkins username (leave empty for unprotected server): user
Password for user:

Initiating safe shutdown...
>> docker exec 0e214f9dcf3d curl
 -w '%{http_code}' -u 'user:******' -o /dev/null
 -s -X POST 'http://localhost:8080/safeExit'
200
Waiting for running jobs to finish...... success.
```

Afterwards call `cx-server remove` to delete its corresponding Docker container. To create a new instance, you can subsequently call `cx-server start`. This will instantiate a new container based on the current configuration. As long as you do not change the home directory of Jenkins, the new container will reuse the previous Cx-server configuration and all other persisted state information.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Build your first project)]
After configuring and starting your Cx-server instance, you are ready to build, test, and deploy your first cloud application. We will use the application we generated in step 1.

Before creating your build job, you need to configure your SCM system of choice. In our example, we will assume that your project is located in GitHub Enterprise. Alternatively, you can also skip the next step (Setup GitHub Enterprise) and use your account on github.com.

>Jenkins also supports other SCM systems, such as GitLab or plain git. You are free to install the corresponding plugin to enable your custom SCM setup.

### Set up GitHub Enterprise
Go to your Jenkins main page and navigate to **Manage Jenkins > Configure System**. On the configuration page, look for `GitHub Enterprise Servers`. Here, add the API endpoint of your GitHub Enterprise server, which typically ends with `/api/v3` (for example `https://github.corp/api/v3`), and assign a name to it. Now you are ready to create a build job for your project.

!![GitHub Enterprise Server](github-enterprise-server.png)

### Create a Jenkins job for your project
To create a build job for your project, navigate to **New Item** in the Jenkins main menu. On the following screen, choose **Multi-branch Pipeline** and specify a name for your project´s build job.

![Jenkins Create Build](jenkins_create_build.png)

Next, you need to set up the SCM source of your project´s build job. Look for **Branch Sources** and add one by specifying your repository https URL.

If your repository is set to private, please also create and use a pair of suitable credentials and choose them for your repository. If you are using github.com, this will also protect you from running into rate limitations.

To control when Jenkins will start a build run and what it will build during that run, you can use the **Behaviors** section of the configuration. The example below shows a configuration that will build all branches and GitHub pull requests. Finally, save your updated configuration by clicking `Save` on the bottom of the page.

!![Branch source](branche-source.png)

Congratulations! Jenkins will now scan your repository and trigger a build.

The best way to get an overview of the build status is to open the job, navigate to the currently running build, and open it in the blue ocean UI of Jenkins.

!![Jenkins Blue Ocean](jenkins-blue-ocean.png)

The resulting pipeline should look as shown in the following screenshot.

!![Pipeline](pipeline.png)

Please note that your pipeline skipped multiple stages, such as the "Release" stage, because you did not configure a deployment target yet. To deploy your application to your SAP Cloud Platform account, you need to add a target to your `.pipeline/config.yml` file.

Moreover, if you already built integration tests that require credentials, you also need to create corresponding records in the Jenkins credentials store and map them via an appropriate entry in the configuration file. Credentials can be created by navigating to **Credentials > System > Global Credentials**.

!![Jenkins System](jenkins-system.png)

On this screen, click **Add Credentials** and enter username, password, as well as an ID for your credentials record.

!![Add Credentials](jenkis-add-credentials.png)

As a result, navigating to **Credentials** should show your freshly created entry.

!![Jenkins Credentials](jenkins-credentials.png)

Finally, you can now leverage this credentials record by adding the credentials configuration property to the `Integration` stage of your `.pipeline/config.yml`.

The example below shows the mapping to the system alias `MyErpSystem`, which you defined earlier in your `systems.yml` file.

```YAML
stages:
  Integration:
    credentials:
      - alias: 'MyErpSystem'
        credentialId: 'MY-ERP'
```

To learn more about pipeline configuration, feel free to have a look on our [documentation on GitHub](https://sap.github.io/jenkins-library/configuration/).

For enabling Jenkins to automatically run builds in the future, you should open GitHub Enterprise and navigate to your **GitHub organization > Settings > Hooks** and create a webhook pointing to your freshly created Cx-server, for example, `http://cx-server.corp/github-webhook/`. Depending on your SCM system, there might be similar ways to realize the notification.

This is also a good point to set up Jenkins to know its own URL. Navigate to **Manage Jenkins > Configure System**, search for **`Jenkins Location`**, and specify **`Jenkins URL`**.

!![Jenkins Location](jenkins-location.png)

Jenkins will use this value to create links to its resources, for example, when creating quality check entries in context of pull requests that are initiated by developers within the project.

!![Checks Passed](checks-passed.png)

Furthermore, you can adopt the number of executors to match the power of your host machine. By default, the Cx-server uses 4 executors, which means that up to three stages of build jobs can run in parallel.

We recommend to increase the number executors to at least the number of cores of your machine. This will help your Cx-server to finish build jobs faster, especially when running multiple in parallel.

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Troubleshoot)]

### Pipeline does not start after commit
Check if your webhook is setup properly and that it succeeds in delivering commit events. Furthermore, ensure that your project and, therefore, also the Jenkins file are located in the root of your git repository. One way of achieving this, is to generate the archetype in the parent folder of your local repository with `artifactId=<repository name>`.

In the future, we will use this section to address common challenges and how they can be resolved. For example, how you can access the Cx-server log files, how you can get shell access to your Cx-server, and how to customize it for your environment when you are using self signed certificates.


[DONE]
[ACCORDION-END]



[ACCORDION-BEGIN [Step 6: ](More information)]


### Questions and Bugs
Are you facing a development question or found a bug? Feel free to open a issue in our GitHub repository on <https://github.com/SAP/jenkins-library/issues>.

### Security Notes
The Cx-server is built with care and based on well established technologies. However, as with most tools, their are certain risks involved that you should properly manage within your setup. Therefore, please carefully read the following security notes before starting to use your Cx-server productively.

### Take Care Who Can Modify the Pipeline
To be able to start other Docker containers, the Cx-server has access to Dockers control socket under `/var/run/docker.sock`. Attackers that manage to intrude your Cx-server container, will be able to launch other containers that then might interfere with other resources on your system.

This is a general security risk in setups where Docker containers need to be able to launch other containers. Accordingly, we advise you to carefully control who can directly or indirectly access your Cx-server. To avoid unnecessary risks, you should avoid running any other business critical applications on the same host.

### Alternative Environments

This tutorial describes the approach of project "Piper" to setup and maintain your own Jenkins server using the cx-server lifecycle scripts. However, there are also alternatives to that:

- SAP offers the [SAP Cloud Platform Continuous Integration and Delivery](https://help.sap.com/viewer/SAP-Cloud-Platform-Continuous-Integration-and-Delivery/618ca03fdca24e56924cc87cfbb7673a.html) which lets you configure and run predefined continuous integration and delivery (CI/CD) pipelines as a service.
- You can also make use of other CI/CD services, such as [GitHub Actions](https://github.com/features/actions), [Travis CI](https://travis-ci.com/), etc. Project "Piper" also offers a [CLI variant](https://sap.github.io/jenkins-library/cli/) of the library which can be used on these services to implement a continuous delivery pipeline.

### Additional Resources

[Configuration Parameters of the Pipeline](https://sap.github.io/jenkins-library/configuration/)

[Jenkins Documentation](https://jenkins.io/doc/)

[Docker Documentation](https://docs.docker.com/)

[DONE]

[ACCORDION-END]

---
