---

title: CI Best Practices Guide: SAPUI5/SAP Fiori on ABAP Front-End Server
description: Part 4.4: Implementing the CI pipeline to build an SAPUI5/SAP Fiori application on ABAP Front-End Server.
tags: [  tutorial>intermediate, tutorial:type/project ]

---

## Prerequisites  

  - **Proficiency:** Intermediate
  - [Generic Project](http://go.sap.com/developer/tutorials/ci-best-practices-generic.html)

## Next Steps

  - [Back to the Navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)
  
---


### 1. Introduction

For Fiori and SAPUI5 development there is a lot of infrastructure available that supports a single developer creating and maintaining a SAPUI5 or Fiori project. SAP Web IDE provides a rich tool set to support single developers or small teams; for example, wizards which generate a skeleton and the metadata files that are required for new projects. But the larger the team the more urgent is the need for an automated CI process based on a central build including automated testing and code quality checks.

The runtime used in this chapter's scenario is an ABAP Front-End Server. ABAP systems are usually part of large landscapes, in which change processes are managed by life cycle management tools using ABAP transport requests. The most common ABAP system setup is a staging landscape consisting of a development system, an acceptance testing system and a productive system. They are linked to each other in this order by transport relationships. Usually, direct modification of objects in the productive system is not permitted. Instead, changes must be applied to the development system under governance of a transport request. Only inside this transport request, the change may make its way to the productive system.

The CI process can coexist with the ABAP life cycle management process in the following way: the CI process ensures the quality of the code that is implemented outside the ABAP system. It converts the sources into the correct format (mainly minification and preload generation), runs automated tests and code checks, and finally produces an artifact that is ready to be uploaded to the ABAP development system. From this point in time ABAP life cycle management takes control and governs the delivery process usually consisting of transporting the changes into the test system and later into the productive system. From the perspective of software delivery to production, ABAP life cycle management is the leading process; the CI process supports development, and it is rather a preparation step for providing the objects for being uploaded into the ABAP development system.

Hence, the delivery process infrastructure for SAPUI5 or Fiori objects consists of two distinct parts:

A. The CI process outside the ABAP system: it processes single code changes, verifies and tests them automatically, minifies them, produces a preload file and provides a zip file containing the application. A CI server (Jenkins in our example) is used for this.

B. ABAP life cycle management: it controls the transport of the changes from the development to the test system (where acceptance testing can be done), and finally from the test to the productive system.

The upload into the ABAP development system seems in some sense to stand between these two parts. But since it is not part of the artifact creation in the CI build and requires the existence or automated creation of a transport request in the system, it rather belongs to the ABAP life cycle management part. Nevertheless, the upload can be triggered automatically immediately after the CI build. Alternatively, a manually triggered upload is possible if this is desired.

![Figure 1: The high-level process flow](principles.png)

Figure 1: The high-level process flow


#### A. Overview: the CI Process

1. Developers use SAP Web IDE to work on a SAPUI5 or Fiori project. For immediate testing, they run the application directly from SAP Web IDE on SAP HANA Cloud Platform.

2. In SAP Web IDE, the developer creates a commit and pushes it into the `master` branch of the central Git instance.

3. The CI build starts. It executes the following steps:

    1. Static code checks for JavaScript.

    2. Automated tests.
    
    3. Minification: the load on the network can be reduced by removing all comments and white spaces from the JavaScript sources.

    4. Preload generation: when a SAPUI5 or Fiori application is called from the browser, usually a lot of resources are downloaded causing a high number of requests from the browser to the server. This can significantly be improved by merging all JavaScript files into one single file.
  
Remark: this process does not yet contain a review or validation step. The changes by the developers are pushed directly into the master branch of the central Git server. There are the following options how to implement a review and validation step before the merge of the change into the `master` branch is done:
    
- In the case you are using GitHub, pull requests can be used to pre-validate and prepare changes. Build hooks can be installed to trigger CI-like builds on the underlying feature branches. See also the part [Generic Project with CI using Cloud Services](http://go.sap.com/developer/tutorials/ci-best-practices-generic-cloud.html) to get an idea on the principle.

- You may use feature branches in your central Git repository. CI-like builds are triggered whenever a change is pushed into the feature branch. Manual work is required to merge the feature branches into the `master` branch where the CI build is running. The central Git repository can be hosted by a Gerrit instance which controls the access permissions. Review and voter features are not used. It is this scenario which we describe in the following. In the near future, SAP Web IDE will support a Gerrit-based review process by generating commit IDs into the commit messages.

The task runner tool we chose is Grunt, which is a common open source tool for processing JavaScript applications. The ecosystem of Grunt offers a lot of open source plugins to perform all kinds of tasks, for example minification or static code analysis. The preload generation is covered by a plugin which is published by the SAP OpenUI5 project. It is planned that SAP Web IDE will incorporate Grunt support in the future such that the build steps in SAP Web IDE and in the CI infrastructure are identical.

> [Grunt home](http://gruntjs.com)

What is described in the following is meant to be taken as an example. It is possible to use different infrastructure than described here, for example a different SCM tool or CI server than Git or Jenkins. Also, the usage of Grunt is not mandatory as there are alternative build processors like for example Gulp.

> [Gulp home](http://gulpjs.com)



#### B. Overview: the Delivery Process Using ABAP Life Cycle Management

It depends on the local requirements to what extend automation is applied with respect to the import of the SAPUI5 or Fiori application into the ABAP development system. In this guide we describe two alternative approaches. The first approach uses fully automated creation of the transport request, upload of the application and release of the transport request. The second approach assumes that the transport request is created and released manually; only the upload of the application using the given transport request is done automatically triggered by a successful CI build. Your solution may be situated somewhere in between using automation in the needed extend, or you may even decide that the upload to the ABAP system remains a strictly manual step.

1. Triggered immediately after a successful CI build for one change, the build scheduler automatically creates a new, individual transport request in the ABAP development system, uploads the application to the ABAP system and releases the transport request. The task of the responsible person for transports is to import the transport requests to the ABAP test system. Figure 2 shows the process flow. The advantage of this approach is the complete automation, but it comes with the price of a loss of control upon the creation of transport requests.
    
    ![Figure 2: Process for Fiori Development with automatic transport request creation](fiori-1c.png)

    Figure 2: Process for Fiori Development with automatic transport request creation

2. The transport request which the imported SAPUI5 or Fiori objects are assigned to is created manually by a responsible person, for example the quality manager. All application versions uploaded to the system are attached to this open transport request. On a defined schedule - for example once a day - the SAPUI5 or Fiori application is planned to be transported into the test system for acceptance testing. The responsible then releases the transport request and imports it into the test system. For continuous CI builds, the responsible must create a new transport request and provide the new transport request number to the CI process such that newly changed objects are assigned to the latter instead. The creation of the new transport request must happen before the old one is closed to avoid a gap in time in which no open transport request exists. Figure 3 shows the process. The advantage is the controlled creation of transport requests by a responsible person. But it requires manual activity.

    ![Figure 3: Process for Fiori Development with manual transport request creation](fiori-1a.png)

    Figure 3: Process for Fiori Development with manual transport request creation

Figure 4 summarizes the infrastructure which is required to run the process.

![Figure 4: Landscape for Fiori Development](fiori-1b.png)

Figure 4: Landscape for Fiori on ABAP front-end server

The scenario described here builds up on the chapters that describe how to set up an example infrastructure using Git/Gerrit as SCM and Jenkins as CI task engine. For details, have a look on the corresponding chapters that are linked from our [navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html) page. But you may decide for any other SCM system or CI task engine.


### 2. Prerequisites

- An account on SAP HANA Cloud Platform.
- An SAP HANA Cloud Connector as revers proxy to pass requests from SAP Web IDE to your corporate Git installation. 

> [SAP HANA Cloud Platform Documentation](https://help.hana.ondemand.com/help/frameset.htm?e9137493bb57101492c6858c8d6b0b62.html)  
> [SAP HANA Cloud Platform Cockpit](https://account.hana.ondemand.com)  
> [Connecting to your Corporate Git System](https://help.hana.ondemand.com/webide/frameset.htm?b8427ec16ae64347b97d2d46fb28f7cd.html)


### 3. Creating Sources for a new Project


Standard way to create a new SAPUI5 or Fiori project is using the wizard in SAP Web IDE to choose from the available templates and to create a skeleton in your workspace. In our example, we choose the wizard which creates one of the sample Fiori applications for the sake of having something more concrete than a simple Hello-World example.

There are two alternatives how to use SAP Web IDE: there is the service available on SAP HANA Cloud Platform, or alternatively, you may use SAP Web IDE Personal Edition which offers the same features but runs on your local machine.

> [SAP Web IDE](https://help.hana.ondemand.com/webide/frameset.htm?0221845d73ad403ab2852142f3179177.html)  
> [SAP Web IDE Personal Edition](https://help.hana.ondemand.com/webide/frameset.htm?5b8bca3147ee4dfd99be8aaf6bd4f421.html)

#### Procedure

1. Open SAP Web IDE:

    - In the case you are using SAP Web IDE on the cloud:

        - Log in to the SAP HANA Cloud Platform Cockpit.
    
        - Select **Services > SAP Web IDE**, then click **Open SAP Web IDE**.

    > [Opening SAP Web IDE](https://help.hana.ondemand.com/webide/frameset.htm?51321a804b1a4935b0ab7255447f5f84.html)
        
    - In the case you are working with SAP Web IDE Personal Edition, install it and start it as described here:
    
    > [SAP Web IDE Personal Edition](https://help.hana.ondemand.com/webide/frameset.htm?5b8bca3147ee4dfd99be8aaf6bd4f421.html)
  
2. In SAP Web IDE, go to **Tools > Preferences > Git settings**. Enter your Git user name and email address, and save your settings.

3. In Gerrit, create a project with a `master` branch as already described in the part [Generic Project](http://go.sap.com/developer/tutorials/ci-best-practices-generic.html).

4. Depending on whether you would like to start a completely new, fresh Fiori project or rather would like to run through a demo, there are two possibilities:

    - Mark the workspace folder and select from the context menu **New > Project from Template**.
    
    - Mark the workspace folder and select from the context menu **New > Project from Sample Application**.

    In both cases, the next steps are identical.

    ![Sample Application in SAP Web IDE](fiori-2.png)
    
5. Mark the newly created project and select **Git > Initialize Local Repository** from the context menu. Enter the following data:

    - Name: `origin`
    
    - URL: `<The ssh based URL of the Gerrit project created above>`
    
    - Press **OK**.

6. On the right sidebar of SAP Web IDE, open the Git pane. Mark **Stage All** and enter a commit description. Press **Commit**.

    ![Initial Commit](fiori-git-commit.png)

7. In the Git pane, press the **Pull** button. The beforehand separated version graphs of the local Git repository in SAP Web IDE and the remote repository in Gerrit are merged now. You may check it in the Git history pane.

    ![Amend Initial Commit](fiori-6.png)

8. To propagate you changes from SAP Web IDE to the central Git repository, go back to the Git pane, press **Push > origin/master**.

    ![Initial Commit](fiori-git-push.png)

The changes are now merged into the `master` branch on the central Git repository.


### 4. Installing Node.js Jenkins Slave Machine

Grunt requires Node.js and the included package manager npm.

#### Procedure

1. Install Node.js on the Jenkins slave machine.

    > [Node.js Home Page](https://Nodejs.org/en/)   
    > [Node.js Downloads](https://nodejs.org/en/download/)
    
    You may install the `tar.gz` package on Linux in any directory. It is recommended that you define a common installation directory on all your Jenkins slave machines.

2. Open the Jenkins front end, go to **Manage Jenkins > Manage Plugins > Available** and select **Node.js Plugin**. Trigger the installation.
    
    Strictly speaking, it is not absolutely necessary to install this plugin since its primary feature (possibility to use JavaScript directly in job implementations) is not used in our example. But it is able to handle possibly multiple Node.js versions in parallel such that you can choose the appropriate one on job level.
    
3. In the Jenkins front end, go to **Manage Jenkins > Configure System**. Scroll down to the **Node.js** section and select **Node.js installations**. Enter the path to the Node.js binaries and an appropriate name of this installation. The name is referred to by build jobs definitions.


### 5. Creating the Grunt Build File

The Grunt build is controlled by a `Gruntfile.js` file. It controls the task flow for processing sources and uses Grunt plugins, which are expected to be present during the Grunt run. To install the plugins needed for Grunt, the npm package manager is used. The information for npm which plugins are required is contained in a file named `package.json`. It contains all dependencies to plugins that are needed for the Grunt build with package name and version.

This scenario has been tested with the Grunt plugin versions that are described in the code listing for `package.json` in the appendix below.

#### Procedure

1. Open your project in SAP Web IDE.

2. Select your project folder, choose **New > File** and enter `package.json` as name.

3. Copy the content of `package.json` from the appendix and paste it into the new file.
    
4. Adapt the `package.json` file according to your context by entering the following values:

    - Name of the package

    - Package version

    - Description

5. Select your project folder, choose **New > File** and enter `Gruntfile.js` as name.

6. Copy the content of `Gruntfile.js` from the appendix and paste it into the new file.
   
7. In the Git pane, stage the two new files, enter a commit description and select **Commit and Push**.


### 6. Working on a Local Machine

If you do not already work with Web IDE Personal Edition, you may clone the Git repository to your machine either using Web IDE Personal Edition or using the Git command line. It is helpful for testing changes on the `Gruntfile.js` locally first before pushing them to the central Git repository and having to deal with build errors on the CI server. As prerequisite you need Node.js and Grunt installed on your machine.

#### Procedure

1. Clone the Git repository to your machine (with Web IDE Personal Edition or Git command line).

2. Open a shell and enter the project directory.

3. Execute the following commands on the shell:

    ```
    npm install
    grunt
    ```

    The `npm install` command installs all needed Node.js modules into the project folder. To avoid versioning them with Git, add the line containing "`node_modules*`" into the `.gitignore` file.
    
4. Do some example changes, rerun the Grunt build and finally create a Git commit and push it to Gerrit.


### 7. Creating the CI Build

We create the job for the CI build on the current `master` snapshot, which is triggered on each change. The configuration and credentials used in this section are based on our examples in [Build Scheduler](http://go.sap.com/developer/tutorials/ci-best-practices-build.html).

#### Procedure

1. Open Jenkins, select **New Item** to create a new job for the voter build and enter an appropriate item name. We use `CI_nw.epm.refapps.ext.shop_master` throughout this example. Select **Freestyle Project** and press **OK**.

2. In the Job configuration, enter the following values:

    Field                                  | Value
    :------------------------------------- | :------------------------------------------
    Restrict where this project can be run | `builds`
    Source Code Management                 | `Git`
    Repository URL                         | `<URL of the Git repository>`
    Credentials                            | `jenkins`
    Branches to build                      | `master`
    Build Triggers                         |
    Poll SCM                               | `checked`
    Schedule                               | `<Enter a pull frequency. For immediate results, two minutes is an appropriate value.>`
    Build Environment                      |
    Delete workspace before build starts   | `checked`
    Inject password to build as environment variables | `checked`
    Global passwords                       | `checked`
    Mask password parameters               | `checked`
    Provide Node & npm bin/folder to PATH  | `checked`
    Installation                           | `<The node installation name that was defined above>`
    
3. In the **Build** section of the job configuration, select **Add build step > Execute shell**. In the **Command** field, enter the following code:

    ```
    npm install
    grunt --no-color default createZip
    ```

    All the build logic happens here. First, the npm call installs all the needed Grunt plugins. In the second step, Grunt is called executing the tasks defined in `Gruntfile.js`. The option `--no-color` suppresses odd characters in the build log that control the font color when Grunt is called from the console. For details concerning the contents of the files, please have a look into the appendix.

3. Press **Save**.   

You may test the job: apply a local change on your project, create a Git commit and push it to the `master` branch in Gerrit. After two minutes, the CI build starts running.


### 8. Enabling the Jenkins Slave to Perform RFC Calls

Next step of the CI process is the implementation of the upload of the application into the ABAP development system. The technical tool for the communication between CI server (Jenkins in our case) and the ABAP system is the SAP NetWeaver RFC (remote function call) library which is able to execute ABAP function modules (which are enabled for this) from remote locations. From inside a Grunt build, we use the library via the Node.js wrapper `node-rfc`. As prerequisite, the SAP NetWeaver RFC Library must be installed on the Jenkins slave machine. You can download it from the Software Downloads page of SAP ONE Support Launchpad.

> [SAP ONE Support Launchpad](https://launchpad.support.sap.com/)

We have tested the `node-rfc` module together with Node.js 4.4.3 on the following platforms:

- SUSE Linux Enterprise Server 12
- Ubuntu 16.04
- Red Hat Enterprise Linux Server release 7.2

To install the library, the `g++` compiler collection package is required on the machine. Ensure that it is installed.

#### Procedure

1. Enter SAP ONE Support Launchpad, navigate to **Software Downloads** and search for `SAP NW RFC SDK 7.20`. Choose the operating system which is running on your Jenkins slave machine and download the archive. To extract the archive, you will also need SAPCAR, which is available in the SAP ONE Support Launchpad as well.

2. Log in to the Jenkins slave machine as `root`.

3. Install the SAP NetWeaver RFC library by extracting it using `SAPCAR.EXE` into a dedicated directory, for example `/opt/sap/nwrfcsdk`:

    ```
    mdkir -p /opt/sap
    cd /opt/sap
    <SAPCAR executable> -xvf <path to the sar archive>/NWRFC_40-20004565.SAR
    ```

    For details, please refer to the installation documentation.

    > [SAP NW RFC Installation on the node-rfc](http://sap.github.io/node-rfc/install.html)

    
4. Still as user `root`, create a new file `/etc/ld.so.conf.d/nwrfcsdk.conf` with the following content:

    ```
    # include nwrfcsdk
    /opt/sap/nwrfcsdk/lib
    ```
    
    The entered directory must reflect your actual installation directory.
    
5. Run `ldconfig` on the command line. This command sets symbolic links to the dynamic libraries such that they can be found at runtime. 

6. Make sure that the variable `SAPNWRFC_HOME` pointing to the RFC library directory is set in the environment of user `jenkins`. Good practice is to add a line like below for example into the `.bashrc` file of the user `jenkins` provided `bash` is the login shell:

    ```
    export SAPNWRFC_HOME=/opt/sap/nwrfcsdk
    ```


### 9. Creating a Jenkins Job for RFC Calls

We implement the upload of the application into the ABAP development system as a separate Jenkins job. This corresponds to the separation of the CI build on one side from ABAP life cycle management on the other side. Accordingly, we create a new file `Gruntfile_ABAP.js` separated from the already existing file `Gruntfile.js`. `Gruntfile_ABAP.js` implements `node-rfc` module calls inside Grunt tasks for the different purposes: transport request creation, application upload and transport request release. Depending on the scenario that is desired (full automation or manual creation of transport requests) the appropriate tasks are passed to the `grunt` command as parameters.

The upload of the zipped application is done by a pull from the ABAP system: it requests the zip via HTTP directly from the Jenkins workspace directory. In the case that access to the Jenkins workspace is more restrictive, an alternative solution using Nexus as artifact repository can be applied. This is described in a paragraph below in this document. 

#### Procedure

##### Defining the Job

1. Open Jenkins and select **New Item** to create a new job named `AI_nw.epm.refapps.ext.shop_master` (AI stands for ABAP import) and select **Freestyle Project**. Press **OK**.

2. Select **This build is parametrized** and enter the following string parameters:

    Name                        |  Default Value
    :-------------------------- | :--------------------
    `ABAP_PACKAGE`              | 
    `ABAP_APPLICATION_NAME`     |
    `ABAP_APPLICATION_DESC`     | 
    `ABAP_DEVELOPMENT_SERVER`   | 
    `ABAP_DEVELOPMENT_INSTANCE` | 
    `ABAP_DEVELOPMENT_CLIENT`   |
    `CI_BUILD_NUMBER`           |
    `GIT_COMMIT`                |
    
    Leave the default values all empty.
    
3. Continue entering the following data into the job configuration:

    Field                                  | Value
    :------------------------------------- | :------------------------------------------
    Restrict where this project can be run | `builds`
    Source Code Management                 | `None`
    Build Environment                      |
    Delete workspace before build starts   | `checked`
    Inject password to build as environment variables | `checked`
    Global passwords                       | `checked`
    Mask password parameters               | `checked`
    Provide Node & npm bin/folder to PATH  | `checked`
    Installation                           | `<The node installation name that was defined above>`

4. In the **Build** section, add a build step **Copy artifacts from another project**. Enter the following values:

    Field                                  | Value
    :------------------------------------- | :------------------------------------------
    Project name                           | `CI_nw.epm.refapps.ext.shop_master`
    Which build                            | `Upstream build that triggered this job`
    Artifacts to copy                      | `**`
    
    In this simple example, we use the Jenkins archive mechanism to pass the zipped application from the CI build job to this one. Also, the Grunt build file is passed this way for not having to clone the source code repository.
    
5. Add a second build step **Execute shell** and enter as **Command**:

    ```
    npm install
    npm install https://github.com/SAP/node-rfc.git
    ln -s rfc-v4.4.3.node node_modules/node-rfc/build/linux_x64/rfc
    grunt --no-color --gruntfile Gruntfile_ABAP.js createTransportRequest uploadToABAP releaseTransport
    ```

    The `node-rfc` library is not available from the public npm repository. It must be downloaded from GitHub directly, and its installation includes an automatically triggered compilation step against the installed native RFC library. The symbolic link is a needed technical step such that the library just compiled can be found. `Gruntfile_ABAP.js` (which we create in one of the next steps) contains the definition of the tasks given here. The task `createTransportRequest` creates a transport request in the ABAP system. The connection data are taken from the job parameters and the masked credentials which will be defined below. The number of the transport request is persisted in the job workspace in a file named `target/CTS_Data.txt` from where it can be fetched by the other tasks.

    The task `uploadToABAP` uploads the application as zip file into the ABAP system. The transport request number that is required is read from the data file explained above.
    
    The task `releaseTransport` finally releases the transport request including all transport tasks inside.

    This example implementation corresponds to the fully automated scenario. We discuss the scenario which works with manually created transport requests below.
    
6. Save.

7. We now need the credentials that are used for establishing the RFC connection. In Jenkins, go to **Manage Jenkins > Configure System > Global Passwords**.

8. Define the credentials for accessing the ABAP system to upload the application. Add the names `ABAP_DEVELOPMENT_USER`, `ABAP_DEVELOPMENT_PASSWORD` and set their correct values. The values of these variables will be masked in the build log.
    
9. Save.

With this configuration, parallel execution of this job is disabled. Do not enable it since otherwise conflicts due to locked objects in the transport request could happen.


##### Adapting the CI Build Job

The idea is that the job just defined is automatically triggered after successful execution of the CI build job.

1. Open the definition of `CI_nw.epm.refapps.ext.shop_master`.

2. In the generic part of the job configuration, choose **This build is parametrized** and enter the string parameters from the table. Choose appropriate names for the ABAP package and the BSP application. The ABAP package name must be created manually in the ABAP system. How to do this is described in paragraph "10. Preparing the ABAP development system" below.

    Name                        |  Default Value
    :-------------------------- | :------------------------------------------
    `ABAP_PACKAGE`              | `<The package name in the ABAP system where the application resides>`
    `ABAP_APPLICATION_NAME`     | `<The name as the BSP application in the ABAP system>`
    `ABAP_APPLICATION_DESC`     | `<The description of the BSP application in the ABAP system>`
    `ABAP_DEVELOPMENT_SERVER`   | `<The host name of ABAP system>`
    `ABAP_DEVELOPMENT_INSTANCE` | `<The instance number of the ABAP system>`
    `ABAP_DEVELOPMENT_CLIENT`   | `<The client number of the ABAP system to be used>`  

3. In the **Post-build Action** section, add an **Archive the artifacts** step. Enter the following data into the **Files to archive** field:

    ```
    package.json, Gruntfile_ABAP.js, target/*-opt-static-abap.zip
    ```
    
    For `<npm package name>` enter the name that was entered in the `package.json` file.
    
4. Add a second step of type **Trigger parametrized build on other projects** and enter the following data:

    Field                                  | Value
    :------------------------------------- | :------------------------------------------
    Projects to build                      | `AI_nw.epm.refapps.ext.shop_master`
    Trigger when build is                  | `Stable`
    
    Select **Add Parameters > Current build parameters** to ensure that the parameters of this job are forwarded to the sequel job.
    
    Select **Add Parameters > Predefined Parameters** and enter the following:
    
    ```
    CI_BUILD_NUMBER=$BUILD_NUMBER
    GIT_COMMIT=$GIT_COMMIT
    ```
    
    The Git commit ID and the CI build number can then be written by the upload job into the change request text field. 
    
5. Save.


### 10. Preparing the ABAP Development System

The SAPUI5/Fiori application needs to be assigned to an ABAP package.

1. Log in to the ABAP development system with the SAP Logon.

2. Enter transaction `se80`, select **Repository Browser** and select `Package` from the drop-down list. Enter an appropriate name. You might also create sub packages as needed.

    ![Package creation in the ABAP system](abap-workbench-1.png)
    
    The creation of a package requires a transport request. Create one by pressing **Create**. In the dialog, enter an appropriate description and **Save**. 
    
    ![Creation of a transport request](abap-workbench-2.png)
    
3. Release the transport request. In the object navigator of transaction `se80`, open the **Transport Organizer** tab, select the transport request just created and its contained task. Release both.

    ![Release of the transport request](abap-workbench-3.png)


### 11. Creating the Grunt File for RFC Calls

The RFC features are implemented in a second Grunt file named `Gruntfile_ABAP.js` for the sake of making `Gruntfile.js` not too complicated.
The main ingredient of this file is the wrapping of the `node-rfc` library as Grunt tasks. Additional documentation for the `node-rfc` library is available
using the following links: 

> [Node.js RFC connector Documentation](http://sap.github.io/node-rfc/)  
> [Node.js RFC connector on GitHub](https://github.com/SAP/node-rfc) 

#### Procedure

1. Open your SAPUI5 or Fiori project in SAP Web IDE.

2. Select your project folder, choose **New > File** and enter `Gruntfile_ABAP.js` as name.

3. Copy the content of `Gruntfile_ABAP.js` from the appendix and paste it into the new file.
   
4. In the Git pane, stage the new file, enter a commit description and select **Commit and Push**.

Within two minutes you can see that the CI build starts and after having finished, triggers the ABAP upload build.


### 12. Roundtrip through the CI Process

Everything is now prepared to execute a full roundtrip through the CI process.

#### Procedure

The following steps simulate the actions which are done many times a day by developers.

1. Apply changes to the sources of your project using SAP Web IDE or working locally.

2. Create a Git commit and push it to Gerrit.

3. Monitor the CI build. Check the result: open transaction `se80` in the SAP Logon and navigate to the package. Check that the application has been uploaded.

4. Monitor the transports. Open transaction `stms` in the SAP Logon and navigate to **Import overview**, select your target system - in our case the ABAP test system - and display the import queue. A new request entry is waiting for import.

    ![Transaction `stms`: import queue](stms-1.png)


### 13. Static Code Analysis with ESLint

ESLint is a commonly used static code analysis tool for JavaScript. We show very roughly how to integrate ESLint into the Grunt build. 

> [ESLint Home](http://eslint.org/)

#### Procedure

1. Create an ESLint configuration file. A convenient way is to generate an ESLint configuration file on a local machine using

    ```
    npm install -g eslint
    eslint --init
    ```
    
    You can find additional information on the following documentation page:

    > [ESLint on the npm repository](https://www.npmjs.com/package/eslint)
    
    Depending on the configuration format you have chosen, there is now a new file, for example named `.eslintrc.json`. You may later reconfigure this file by switching on and off rules according to your requirements.

2. Open the file `Gruntfile.js`. Switch on ESLint tests by adding the `eslint` sub task into the `default` task:

    ```
        grunt.registerTask("default", ["clean", "eslint", "copy:copyToDbg", "openui5_preload:preloadDbg", "copy:copyToTmp",
        "uglify:uglifyTmp", "cssmin", "openui5_preload:preloadTmp", "copy:copyDbgToTmp",
        "uglify:uglifyPreload"]);
    ```

    Check that the name of the ESLint configuration file is correct:
    
    ```
        "eslint": {
            options: {
                configFile: ".eslintrc.json",
            },
            target: [webAppDir + "/**/*.js"]
        },
    ```

3. Commit the change and push. Monitor the CI build result.


### 14. Code Page Check

The upload of the zipped application to the ABAP system requires specifying a code page. As default, the standard code page of SAP GUI is taken. But this may differ from the code pages used by developers who are developing the SAP UI5 or Fiori application. To be on the save side, defining a standard code page is recommended. UTF8 is a good candidate for such a standard, and our example follows this. Depending on the majority of locales used by developers in your organization, another code page can be chosen. The rule is enforced by using the `grunt-encoding` plugin which checks the source files for their correct encoding.

#### Procedure

1. Open the file `Gruntfile.js`. Switch on the encoding check tests by adding the `encoding` sub task into the `default` task:

    ```
        grunt.registerTask("default", ["clean", "encoding", "eslint", "copy:copyToDbg", "openui5_preload:preloadDbg", "copy:copyToTmp",
        "uglify:uglifyTmp", "cssmin", "openui5_preload:preloadTmp", "copy:copyDbgToTmp",
        "uglify:uglifyPreload"]);
    ```

    Check whether UTF-8 is the correct encoding for your use case. Eventually, change it according to your setup:
    
    ```
        "encoding": {
            options: {
                encoding: "UTF8"
            },
    ```

2. Commit the change and push. Monitor the CI build result.


### 15. Automated Testing

The examples given above shall only give you an impression of how checks can be handled in Grunt. For automated unit testing and application testing there are a lot of frameworks available. It depends on the explicit project setup which one to use. Therefore, we do not give an explicit recommendation. The `Gruntfile.js` in general will grow with additional checks as your requirements on the code quality grows.



### 16. Alternative Approach Using Manual Transport Request Creation

As an alternative, we discuss now a process in which the transport requests are created manually. It is intended that you get an idea how you could easily adapt the presented process. 


#### Procedure

##### A: Creating the Registration Job for the Transport Request 

The prerequisite for the upload job is that an open transport request exists in the system and its number is available to the upload task. We implement a registration job that is manually triggered by the QA manager or any other responsible person immediately after having created the transport request in the ABAP system. The job takes the transport request number as argument and stores it in Jenkins for being fetched by the CI build job, which has to be modified accordingly.

1. Open Jenkins and select **New Item** to create a new job for registration of the transport request number. Enter an appropriate item name like `TR_nw.epm.refapps.ext.shop_master` (TR stands for Transport Request) and select **Freestyle Project**. Press **OK**.

2. In the general section of the project configuration, select **This build is parametrized**, select **Add Parameter > String Parameter** and enter `TRANSPORT_REQUEST` as **Name**. Leave the **Default Value** empty.

3. Select **Restrict where this project can be run** and enter `builds`.

4. In the **Build environment** section, select **Delete workspace before build starts**.

5. In the **Build** section, select **Add build step > Execute shell** and enter the following code snippet:

    ```
    mkdir target
    echo "{\"REQUESTID\":\"$TRANSPORT_REQUEST\"}" > target/CTS_Data.txt
    ```

6. In the **Post-build Actions** section, select **Add post-build action > Archive the artifacts** and enter `target/CTS_Data.txt` into the **Files to archive** field.

7. Save.

8. Open the configuration of the job `AI_nw.epm.refapps.ext.shop_master`.

9. In the **Build** section, select **Add build step > Copy artifacts from another project**. Place this step by drag and drop at second position between the already existing copy step and the **Execute shell** build step and enter the following data:

    Field                                  | Value
    :------------------------------------- | :------------------------------------------
    Project name                           | `TR_nw.epm.refapps.ext.shop_master`
    Which build                            | `Latest successful build`
    Artifacts to copy                      | `target/CTS_Data.txt`

12. Change the command in the **Execute shell** step as follows:

    ```
    npm install
    npm install https://github.com/SAP/node-rfc.git
    ln -s rfc-v4.4.3.node node_modules/node-rfc/build/linux_x64/rfc
    grunt --no-color --gruntfile Gruntfile_ABAP.js uploadToABAP
    ```

13. Save.


##### B: Roundtrip through the CI Process

Everything is now prepared to execute a full roundtrip through the CI process.

#### Procedure

###### Preparing the ABAP Development System

The ABAP package must exist. Create it as described above if it does not yet exist.

###### Creating the Transport Request and Registering its Number to Jenkins

A transport request must be available such that the application can be uploaded to the ABAP system.

1. Log in to the ABAP development system with the SAP Logon.

2. Enter the transport organizer via transaction `se09` and press **Create**.

    ![Transaction `se09`, entry](se09-1.png)
    
3. Select **Workbench request**.

    ![Transaction `se09`, request creation](se09-2.png)

4. Enter a description and press **Save**.

    ![Transaction `se09`, request creation dialog](se09-3.png)
    
5. Copy the number of the transport request just created.

    ![Transaction `se09`, transport request created](se09-4.png)

6. Open the job `TR_nw.epm.refapps.ext.shop_master` in Jenkins. Select **Build with Parameters** and enter the transport request number.

    ![Registring the transport request in Jenkins](transport-request-jenkins.png)
    
7. Select **Build**.


###### Processing Changes


1. Apply changes to the sources of your project using SAP Web IDE or working locally.

2. Create a commit and push it to Gerrit.

3. Monitor the CI build. Check the result: open transaction `se80` in the SAP Logon and navigate to the package. Check that the files have been uploaded.


###### Releasing the Change Request and Creating a New One

At predefined times, for example once a day, the responsible person releases the transport request to bring the changes of the day to the ABAP test system. It must be avoided that the CI build runs into an error due to the transport request, to which it tries to assign the files, being already closed. A new transport request must be created before and registered to Jenkins.

1. Execute all the steps described under "Creating the transport request and registering its number to Jenkins". From now on, the CI build uses the new transport request.

2. Log in to the ABAP development system with the SAP Logon.

3. In transaction `se09`, select the transport request that was in usage before and release it.


### 17. Using Nexus as Repository (optional)

Due to access restrictions, HTTP requests from the ABAP development system to Jenkins for fetching the application zip file might be disabled. In this case, a Nexus repository for temporarily storing the artifact comes in place. We modify the two jobs for CI build and ABAP import accordingly.

#### Procedure

##### A: Changing the CI Build Job

1. Open the definition of `CI_nw.epm.refapps.ext.shop_master`.

2. Add a new string parameter named `NEXUS_SNAPSHOT_REPO`. As **Default value**, enter the URL of the Nexus snapshot repository where the application zip file shall be stored.

3. In the **Build** section, change the command of the **Execute shell** step as follows:

    ```
    npm install
    grunt --no-color default createZip deployToNexus
    ```
    
4. In the **Post-build Action > Archive the artifacts** step, remove the zip file entry:

    ```
    package.json, Gruntfile_ABAP.js
    ```

5. Save.


##### B. Changing the Job for RFC Calls

1. Open the definition of `AI_nw.epm.refapps.ext.shop_master`.

2. Add a new string parameter named `NEXUS_SNAPSHOT_REPO`. Leave the **Default Value** empty.

3. Save.

The implementation of the upload task in our example `Gruntfile_ABAP.js` file checks if there is a local zip file in the Jenkins workspace. If not, then the alternative `zipFileURL`, which is calculated from the artifact co-ordinates, is passed to the ABAP system.

Start a roundtrip for this process implementation by pushing a change. The CI build now deploys the generated zip file to Nexus from where it is fetched by the ABAP system.


### Appendix

#### `package.json`

The `package.json` file contains meta-information about the JavaScript project. Most important here are the dependencies to Grunt plugins that are needed during build time, not at run time. Therefore they are entered as `devDependencies`. The scenario described here was tested with the versions given below. We cannot guarantee the compatibility of the plugins in other versions.

The file listed here is the minimum needed for make the process skeleton run. You may feel free to add some more data according to your requirements.

When you use it, you will have to enter some data by yourself, as the name and the version of your package.

> [Npm documentation for `package.json`](https://docs.npmjs.com/files/package.json)

```
{
  "name": "{name of the package}",
  "version": "{version of the package}",
  "description": "{description of the package}",
  "private": true,
  "devDependencies": {
    "grunt": "0.4.5",
    "grunt-cli": "1.2.0",
    "grunt-encoding": "0.3.0",
    "grunt-eslint": "19.0.0",
    "grunt-contrib-clean": "1.0.0",
    "grunt-contrib-copy": "1.0.0",
    "grunt-contrib-cssmin": "1.0.1",
    "grunt-contrib-uglify": "1.0.1",
    "grunt-folder-list": "1.1.0",
    "grunt-mkdir": "1.0.0",
    "grunt-nexus-deployer": "0.0.8",
    "grunt-openui5": "0.9.0",
    "grunt-rename": "0.1.4",
    "grunt-zip": "0.17.1",
    "q": "1.4.1"
  }
}
```

#### `Gruntfile.js`

The `Gruntfile.js` defines the tasks to be executed for building the SAPUI5/Fiori application. All tasks are provided by the Grunt community, and some of them are specific for SAPUI5 development. The tasks provide all needed steps for minifying the JavaScript sources, creating the preload file and uploading the result into an ABAP system. Optionally, the result may be packaged into a zip file (not needed in our scenario).

In the build result, the minified files inherit the name from the original source files. For debugging purposes, the original files are kept and uploaded to the ABAP system under the original name but including a `dbg` suffix.

The tasks that are executed during a Fiori build are the following:

1. `clean`: Cleans up the build workspace to avoid pollution by build results of previous build runs.

2. `copy:copyToDbg`: The original files are copied to a debug file folder and renamed using a `dbg` suffix.

3. `openui5_preload:preloadDbg`: Generation of the preload file containing the contents of the debug files.

4. `copy:copyToTmp`: The original files are copied for minification.

5. `uglify:uglifyTmp`: Minification of JavaScript files.

6. `cssmin`: Minification of `.css` files.

7. `openui5_preload:preloadTmp`: Generation of the preload file containing the contents of minified files.

8. `copy:copyDbgToTmp`: Merge of the minified files and debug files into one folder.

9. `uglify:uglifyPreload`: Minification of the preload file.

10. `zip`: Creation of the application zip file.

Please note that the Grunt script given below is meant as an example for implementing a very rudimentary CI process. It should only give a rough idea how to implement the CI process that is fitting in your concrete situation.

> [Configuring tasks in Grunt documentation](http://gruntjs.com/configuring-tasks)

```
"use strict";

module.exports = function(grunt) {
    // Variables from environment
    var nexusUser = process.env.NEXUS_DEPLOY_USER;
    var nexusPassword = process.env.NEXUS_DEPLOY_PASSWORD;
    var nexusSnapshotRepoURL = process.env.NEXUS_SNAPSHOT_REPO;

    // Project properties
    var webAppDir = "webapp";
    var targetDir = "target";
    var tmpDir = targetDir + "/tmp";
    var tmpDirDbg = targetDir + "/tmp-dbg";
    var zipFileSuffix = "-opt-static-abap.zip";
    var preloadPrefix = "nw/epm/refapps/ext/shop";
    var nexusGroupId = "com.yourcompany";

    // Project configuration.
    grunt.initConfig({
        pkg: grunt.file.readJSON("package.json"),
        clean: {
            build: [targetDir]
        },
        encoding: {
             options: {
                 encoding: "UTF8"
             },
             files: {
                 src: [webAppDir + "/**/*.js", webAppDir + "/**/*.css",
                      webAppDir + "/**/*.xml", webAppDir + "/**/*.json",
                       webAppDir + "/**/*.html", webAppDir + "/**/*.properties"]
             }
        },
        eslint: {
            options: {
                configFile: ".eslintrc.json"
            },
            target: [webAppDir + "/**/*.js"]
        },
        copy: {
            copyToDbg: {
                files: [
                    {
                        expand: true,
                        src: "**/*.js",
                        dest: tmpDirDbg,
                        cwd: webAppDir,
                        filter: function(filepath) {
                            // prevent js from localService to be copied
                            return !filepath.match(new RegExp(webAppDir + "(\\/|\\\\)localService", "gi"));
                        }
                    },
                    {
                        expand: true,
                        src: "**/*.css",
                        dest: tmpDirDbg,
                        cwd: webAppDir
                    }]
            },
            copyToTmp: {
                files: [
                    {
                        expand: true,
                        src: "**/*.js",
                        dest: tmpDir,
                        cwd: webAppDir,
                        filter: function(filepath) {
                            // prevent js from localService to be copied
                            return !filepath.match(new RegExp(webAppDir + "(\\/|\\\\)localService", "gi"));
                        }
                    },
                    {
                        expand: true,
                        src: "**/*.css",
                        dest: tmpDir,
                        cwd: webAppDir
                    },
                    {
                        expand: true,
                        src: "localService/metadata.xml",
                        dest: tmpDir,
                        cwd: webAppDir
                    },
                    {
                        expand: true,
                        src: "**/*",
                        dest: tmpDir,
                        cwd: webAppDir,
                        filter: function(filepath) {
                            // prevent js and css files and contents of webapp/test from being copied
                            return !filepath.match(new RegExp("(" + webAppDir + "(\\/|\\\\)test|${webAppDir}(\\/|\\\\)localService|\\.js$|\\.css$|\\test.html$)", "gi"));
                        }
                    }]
            },
            copyDbgToTmp: {
                files: [
                    {
                        expand: true,
                        src: "**/*.js",
                        dest: tmpDir,
                        cwd: tmpDirDbg,
                        rename: function(dest, src) {
                            return dest + "/" + src.replace(/((\.view|\.fragment|\.controller)?\.js)/, "-dbg$1");
                        }
                    },
                    {
                        expand: true,
                        src: "**/*.css",
                        dest: tmpDir,
                        cwd: tmpDirDbg,
                        rename: function(dest, src) {
                            return dest + "/" + src.replace(".css", "-dbg.css");
                        }
                    }]
            }
        },
        uglify: {
            uglifyTmp: {
                files: [
                    {
                        expand: true,
                        src: "**/*.js",
                        dest: tmpDir,
                        cwd: webAppDir,
                        filter: function(filepath) {
                            // prevent js from localService to be copied
                            return !filepath.match(new RegExp(webAppDir + "(\\/|\\\\)localService", "gi"));
                        }
                    }]
            },
            uglifyPreload: {
                files: [
                    {
                        expand: true,
                        src: tmpDir + "/Component-preload.js"
                    }]
            }
        },
        cssmin: {
            build: {
                files: [
                    {
                        expand: true,
                        src: "**/*.css",
                        dest: tmpDir,
                        cwd: webAppDir
                    }]
            }
        },
        openui5_preload: {
            preloadDbg: {
                options: {
                    resources: {
                        cwd: tmpDirDbg,
                        src: ["**/*.js"],
                        prefix: preloadPrefix
                    },
                    compress: false,
                    dest: tmpDirDbg
                },
                components: true
            },
            preloadTmp: {
                options: {
                    resources: {
                        cwd: tmpDir,
                        src: ["**/*.js"],
                        prefix: preloadPrefix
                    },
                    compress: false,
                    dest: tmpDir
                },
                components: true
            }
        },
        nexusDeployer: {
            build: {
                options: {
                    groupId: nexusGroupId,
                    artifactId: "<%= pkg.name %>",
                    version: "<%= pkg.version %>-SNAPSHOT",
                    packaging: "zip",
                    auth: {
                        username: nexusUser,
                        password: nexusPassword
                    },
                    pomDir: targetDir + "/pom",
                    url: nexusSnapshotRepoURL,
                    uploadMetadata: false,
                    artifact: targetDir + "/<%= pkg.name %>" + zipFileSuffix
                }
            }
        },
        zip: {
            build: {
                cwd: tmpDir,
                src: tmpDir + "/**/*",
                dest: targetDir + "/<%= pkg.name %>" + zipFileSuffix
            }
        }
    });

    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-contrib-copy");
    grunt.loadNpmTasks("grunt-contrib-uglify");
    grunt.loadNpmTasks("grunt-contrib-cssmin");
    grunt.loadNpmTasks("grunt-zip");
    grunt.loadNpmTasks("grunt-openui5");
    grunt.loadNpmTasks("grunt-nexus-deployer");
    grunt.loadNpmTasks("grunt-eslint");

    grunt.registerTask("default", ["clean", "copy:copyToDbg", "openui5_preload:preloadDbg", "copy:copyToTmp",
          "uglify:uglifyTmp", "cssmin", "openui5_preload:preloadTmp", "copy:copyDbgToTmp",
          "uglify:uglifyPreload"]);
    grunt.registerTask("createZip", ["zip"]);
    grunt.registerTask("deployToNexus", ["nexusDeployer"]);
};
```


#### `Gruntfile_ABAP.js`

This Grunt file contains the logic for the RFC connections. It implements 3 tasks for creating a transport request, for uploading the application to the ABAP system, and for releasing the transport request. The connection data to the ABAP system are passed from the environment that is built up by the Jenkins job.

There is a file `target/CTS_Data.txt` created by the transport request creation task to persist the transport request number. From the file, this number can be fetched by the two other tasks. This is how the transport request number is passed in a call like

```
grunt --gruntfile Gruntfile_ABAP.js createTransportRequest uploadToABAP releaseTransport
```

Alternatively, it is possible to execute the upload without having created a transport request before. The transport request number can be passed as argument like this:

```
grunt --gruntfile Gruntfile_ABAP.js uploadToABAP:<transport request number> 
```

The implementation of the Grunt tasks is rather straight forward using the documentation of the `node-rfc` module.

> [Node.js RFC connector Documentation](http://sap.github.io/node-rfc)  
> [Node.js RFC connector on GitHub](https://github.com/SAP/node-rfc)

Attention has to be paid on the fact that the RFC call is asynchronous. The Grunt process flow has to wait for them being finished which is the reason that we have to work with promises from the `q` library.

```
"use strict";

var rfc = require("node-rfc");
var fs = require("fs");
var q = require("q");

module.exports = function(grunt) {

    // Project specific variables
    var abapDevelopmentUser = process.env.ABAP_DEVELOPMENT_USER;
    var abapDevelopmentPassword = process.env.ABAP_DEVELOPMENT_PASSWORD;
    var abapDevelopmentServer = process.env.ABAP_DEVELOPMENT_SERVER;
    var abapDevelopmentInstance = process.env.ABAP_DEVELOPMENT_INSTANCE;
    var abapDevelopmentClient = process.env.ABAP_DEVELOPMENT_CLIENT;
    var abapApplicationName = process.env.ABAP_APPLICATION_NAME;
    var abapApplicationDesc = process.env.ABAP_APPLICATION_DESC;
    var abapPackage = process.env.ABAP_PACKAGE;
    var jobURL = process.env.JOB_URL;
    var nexusSnapshotRepoURL = process.env.NEXUS_SNAPSHOT_REPO;
    var gitCommit = process.env.GIT_COMMIT;

    // Global Variables
    var targetDir = "target";
    var zipFileSuffix = "-opt-static-abap.zip";
    var ctsDataFile = targetDir + "/CTS_Data.txt";
    var nexusGroupId = "com.yourcompany";

    // Project configuration.
    var abapConn = {
        user: abapDevelopmentUser,
        passwd: abapDevelopmentPassword,
        ashost: abapDevelopmentServer,
        sysnr: abapDevelopmentInstance,
        client: abapDevelopmentClient
    };
    grunt.initConfig({
        pkg: grunt.file.readJSON("package.json"),
        createTransportRequest: {
            options: {
                conn: abapConn,
                author: abapDevelopmentUser,
                description: "Commit: " + gitCommit
            }
        },
        uploadToABAP: {
            options: {
                conn: abapConn,
                zipFile: targetDir + "/<%= pkg.name %>" + zipFileSuffix,
                zipFileURL: nexusSnapshotRepoURL + "/" + nexusGroupId.replace(/\./g, "/") + "/<%= pkg.name %>/<%= pkg.version %>-SNAPSHOT/<%= pkg.name %>-<%= pkg.version %>-SNAPSHOT.zip",
                codePage: "UTF8"
            }
        },
        releaseTransport: {
            options: {
                conn: abapConn
            }
        }
    });

    var rfcConnect = function(functionModule, importParameters, gruntContext) {
        var conn = gruntContext.options().conn;
        var client = new rfc.Client(conn);
        var deferred = q.defer();

        grunt.log.writeln("RFC client lib version:", client.getVersion());

        client.connect(function(err) {
            if (err) { // check for login/connection errors
                deferred.reject();
                grunt.log.errorlns("could not connect to server", err);
                return deferred.promise;
            }
            // invoke remote enabled ABAP function module
            grunt.log.writeln("Invoking function module", functionModule);
            client.invoke(functionModule,
                importParameters,
                function(err, res) {
                    if (err) { // check for errors (e.g. wrong parameters)
                        grunt.log.errorlns("Error invoking", functionModule, err);
                        deferred.reject();
                        return;
                    }
                    client.close();
                    grunt.log.writeln("Messages:", res.EV_LOG_MESSAGES);
                    deferred.resolve(res);
                });
        });
        return deferred.promise;
    };


    grunt.registerTask("createTransportRequest", "Creates an ABAP Transport Request", function() {
        grunt.log.writeln("Creating Transport Request");
        var importParameters = {
            AUTHOR: this.options().author,
            TEXT: this.options().description
        };
        var done = this.async();
        rfcConnect("BAPI_CTREQUEST_CREATE", importParameters, this)
            .then(
            function(returnValue) {
                if (returnValue.EV_SUCCESS == "E" || returnValue.EV_SUCCESS == "W") {
                    grunt.log.errorlns("Error invoking BAPI_CTREQUEST_CREATE.");
                    grunt.log.errorlns("Message Id:", returnValue.EV_MSG_ID);
                    grunt.log.errorlns("Message No:", returnValue.EV_MSG_NO);
                    grunt.log.errorlns("Messages:", returnValue.EV_LOG_MESSAGES);
                    done(false);
                    return;
                }
                if (returnValue.REQUESTID == "") {
                    grunt.log.errorlns("Error invoking BAPI_CTREQUEST_CREATE.");
                    grunt.log.errorlns("Transport request could not be created.");
                    grunt.log.errorlns(returnValue.RETURN.MESSAGE);
                    done(false);
                    return;
                }
                grunt.log.writeln("Transport request", returnValue.REQUESTID, "created.");
                if (fs.existsSync(targetDir) === false) {
                    fs.mkdirSync(targetDir);
                }
                fs.writeFile(ctsDataFile,
                    JSON.stringify(
                        { REQUESTID: returnValue.REQUESTID }
                    ),
                    function(err) {
                        if (err) {
                            grunt.log.errorlns("Error Creating file:", err);
                            done(false);
                            return;
                        }
                        grunt.log.writeln("Created file:", ctsDataFile);
                        done();
                    }
                )
            },
            function() {
                done(false);
            });
    });

    grunt.registerTask("uploadToABAP", "Uploads the application to the ABAP System", function(transportRequest) {
        grunt.log.writeln("Uploading to ABAP");
        if (!transportRequest) {
            if (!fs.existsSync(ctsDataFile)) {
                grunt.log.errorlns("No Transport request specified. Pass one explicitly or run createTransportRequest first.");
                return (false);
            }
            transportRequest = JSON.parse(fs.readFileSync(ctsDataFile, { encoding: "utf8" })).REQUESTID;
        }
        grunt.log.writeln("Transport request:", transportRequest);
        var url = "";
        if (!(typeof this.options().zipFile === "undefined") && fs.existsSync(this.options().zipFile)) {
            url = jobURL + "/ws/" + this.options().zipFile;
        }
        else {
            url = this.options().zipFileURL;
        }
        var importParameters = {
            IV_URL: url,
            IV_SAPUI5_APPLICATION_NAME: abapApplicationName,
            IV_SAPUI5_APPLICATION_DESC: abapApplicationDesc,
            IV_PACKAGE: abapPackage,
            IV_WORKBENCH_REQUEST: transportRequest,
            IV_TEST_MODE: "-",
            IV_EXTERNAL_CODE_PAGE: this.options().codePage
        };
        var done = this.async();
        grunt.log.writeln("Uploading application from", url);
        rfcConnect("/UI5/UI5_REPOSITORY_LOAD_HTTP", importParameters, this)
            .then(
            function(returnValue) {
                if (returnValue.EV_SUCCESS == "E" || returnValue.EV_SUCCESS == "W") {
                    grunt.log.errorlns("Error invoking", "/UI5/UI5_REPOSITORY_LOAD_HTTP");
                    grunt.log.errorlns("Message Id:", returnValue.EV_MSG_ID);
                    grunt.log.errorlns("Message No:", returnValue.EV_MSG_NO);
                    grunt.log.errorlns("Messages:", returnValue.EV_LOG_MESSAGES);
                    done(false);
                    return;
                }
                grunt.log.writeln("Application uploaded.");
                done();
            },
            function() {
                done(false);
            });
    });

    grunt.registerTask("releaseTransport", "Releases an ABAP Transport Request", function(transportRequest) {
        grunt.log.writeln("Releasing Transport Request");
        if (!transportRequest) {
            if (!fs.existsSync(ctsDataFile)) {
                grunt.log.errorlns("No Transport request specified. Pass one explicitly or run createTransportRequest first.");
                return (false);
            }
            transportRequest = JSON.parse(fs.readFileSync(ctsDataFile, { encoding: "utf8" })).REQUESTID;
        }
        grunt.log.writeln("Transport request:", transportRequest);
        var importParameters = {
            REQUESTID: transportRequest,
            COMPLETE: "X",
            BATCH_MODE: "X"
        }
        var done = this.async();
        rfcConnect("BAPI_CTREQUEST_RELEASE", importParameters, this)
            .then(
            function(returnValue) {
                if (returnValue.EV_SUCCESS == "E" || returnValue.EV_SUCCESS == "W") {
                    grunt.log.errorlns("Error invoking", "BAPI_CTREQUEST_RELEASE");
                    grunt.log.errorlns("Message Id:", returnValue.EV_MSG_ID);
                    grunt.log.errorlns("Message No:", returnValue.EV_MSG_NO);
                    grunt.log.errorlns("Messages:", returnValue.EV_LOG_MESSAGES);
                    done(false);
                    return;
                }
                grunt.log.writeln("Transport request released.");
                done();
            },
            function() {
                done(false);
            });
    });
};
```
