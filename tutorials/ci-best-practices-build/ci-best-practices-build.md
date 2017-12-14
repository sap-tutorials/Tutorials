---

title: Continuous Integration (CI) Best Practices with SAP: Build Scheduler
description: Part 3.2: Setting up a Jenkins Instance.
primary_tag: products>sap-cloud-platform
tags: [  tutorial>intermediate, tutorial:type/project ]

---

## Prerequisites  

  - **Proficiency:** Intermediate

## Next Steps
 
  - [Artifact Repository](https://www.sap.com/developer/tutorials/ci-best-practices-artifacts.html)
  - [Back to the Navigator](https://www.sap.com/developer/tutorials/ci-best-practices-intro.html)
  
---


> The purpose of this guide is to enable you to do first steps in designing your own CI processes using components like Gerrit, Jenkins and Nexus. These setup instructions for these components serve educational purposes only and are not meant as reference setup for productive purposes; for productive use, refer to the official component documentation.

What we offer in this part is not more than a basic recipe to set up a minimum installation including only those components on Linux that we consider as absolutely necessary to run a CI/CD process for development with SAP. However, the setup best suited to your concrete requirements cannot be part of this document because it highly depends on your concrete local situation, the network setup, the overall landscape into which the CI/CD process will be embedded, and so on. Therefore, we will restrict ourselves here to showing only the principles and the core elements.

If you are interested in other examples of CI/CD processes with a focus on specific needs and local conditions, just follow the references we provide to the official documentation for the described components. For each component, we provide hints to how the described setup could be improved. This concerns the professional and reliable operation, for example, further security activities and operational refinements.

Note: This document is restricted to the description of component installation on Linux. For installation on Windows, we refer to the installation documentation on the web.


### 1. Introduction

The build scheduler is the component where you define build procedures and trigger builds. One of the most popular build schedulers is Jenkins, but other solutions, especially on the cloud, are possible as well. 

Jenkins integrates well with polling (Gerrit) or event-based mechanisms (GitHub). It is available as an open source tool and has a large community that provides plugins for any purpose. It is easy to write plugins for Jenkins as well.

Jenkins works normally with several instances: one central master machine and many slave machines (the words "slave" and "node" are used as synonyms). In this setup, the master works only as scheduler: it hosts the job definitions, metadata, and schedules. It triggers the builds and delegates the build work to a slave. A slave machine runs only a slim process that is connected and controlled by the master. The setup and installation of a slave machine, dependent on the requirements of the builds, can differ significantly from the master. For example, it can host one or more JDKs (for Java builds) that differ from the master. To support multiplatform builds, the slave may even run with a completely different operating system.

Therefore, there are some important differences:

- The Jenkins master process runs using a dedicated JDK version on the master.
- The Jenkins slave process or processes run using a dedicated JDK version on the slave.
- When compiling java, build processes on the slaves may run with custom JDKs.
 
In addition, you can run Jenkins also as a one-instance installation which means builds can run directly on the master without using a slave. For very small projects and for process startup, this approach might be sufficient. However, when projects become bigger and the requirements for professional operation increase, you may find that it is necessary to use a master-slave operation. One aspect to consider is that the Jenkins master is a single point of failure in the build landscape that should not be exposed to direct neighborhood of rather uncontrollable processes started inside a running build.

This document describes a master-slave installation. Even if you decide on a master-only approach, please also read through the Jenkins slave section, since it describes the additional steps to be done on the master.

Our description is based on Jenkins version 1.647. In the case you use Jenkins 2, all what is described also works, but the navigation might have changed, and some of the core features have been moved to plugins which must be installed in addition to the mentioned ones.

### 2. Jenkins Master

This section describes how to set up the Jenkins master.

> [Jenkins, downloads](https://jenkins-ci.org)  
> [Jenkins installation guide](https://wiki.jenkins-ci.org/display/JENKINS/Installing+Jenkins)

#### Prerequisites

- Install Java JDK 1.7.0 or higher on the machine, for running the Jenkins master process.
- If you are executing builds directly on the master, you may also want to install other JDKs to provide alternative compilation environments.

#### Procedure

1. Log in as user `root` onto the hosting machine.

2.  Create an OS user `jenkins`. This is the user name used throughout this example; however, you can use any other OS user that runs the Jenkins daemon.

3. Create a Jenkins data directory owned by `jenkins`. This example uses `/data/jenkins`, but any other directory that follows the respective conventions works as well. Ensure that the file partition of the directory is large enough to store all of the Jenkins data. Keep in mind that all build logs (including those for builds that are running on slaves) are stored inside this directory.
    
    ```
    mkdir -p /data/jenkins
    chown jenkins /data/jenkins
    ```

4. To make the installation easy, download the fitting installation package to your Linux distribution from the Jenkins web site, and install the package on the hosting machine. For example, if you use SUSE or Redhat Linux, download the corresponding `rpm` package and install it with the package manager (`zypper` or `rpm` in this case) on the machine. Consult the official Jenkins documentation for details concerning your Linux platform.

5. Configure the Jenkins home, the Java home directory, the Jenkins daemon user and the Jenkins port. The location may differ between various Linux distributions, for example on `SUSE Linux`, it is located in `/etc/sysconfig/jenkins`. 

    ```
    JENKINS_HOME="/data/jenkins"
    JENKINS_JAVA_HOME=<Java home of your Java JDK installation>
    JENKINS_USER="jenkins"
    JENKINS_PORT="8082"
    ```

    You can use any other available port number.

6. To enable the Jenkins master to connect to Jenkins slaves, use an SSH-based connection mechanism. To prepare this, proceed as follows:
    - Log in as `jenkins` onto the Jenkins master machine.
    - Create an RSA key (`id_rsa`, `id_rsa.pub`) pair in the `.ssh` directory of the `jenkins` home  directory (in our example `/home/Jenkins/.ssh`):

        ```
        mkdir /home/jenkins/.ssh
        chmod 700 /home/jenkins/.ssh
        cd /home/jenkins/.ssh
        ssh-keygen
        ```

    - You will need a copy of the public key file later on during the setup of the Jenkins slave (see below).

7. Start the Jenkins daemon. This step might depend on the Linux distribution and the way you have installed Jenkins. For SUSE Linux, log in to the Jenkins master machine as `root` and execute:

    ```
    service jenkins start
    ```

    You see a Jenkins process running as user `jenkins`.
   
8. Open a browser and access the Jenkins master on port 8082. You see the Jenkins master main page.

9. To enable features that will be used throughout our guide, you must install some plugins. To use the Jenkins plugin download and installation mechanism, you must configure a proxy. On the Jenkins main page, go to **Manage Jenkins > Manage Plugins > Advanced**, enter the needed proxy settings specific to your environment, press on **Submit** and then on **Check now** to get the plugin information updated.

    ![Jenkins Configuration](build-scheduler-1.png)
    
    ![HTTP Proxy Configuration](build-scheduler-2.png)

10. Select **Manage Jenkins > Manage Plugins > Available**, then choose the following plugins:

    - **Build Pipeline Plugin**. This will be needed for defining Jenkins jobs connected to a pipeline.
    - **Conditional Build Step**.
    - **Copy Artifact Plugin**.
    - **Credentials Binding Plugin**: Provides credential data as environment variables to a job.
    - **Environment Injector Plugin**: Injects environment variable into the job runtime.
    - **Gerrit Trigger**.
    - **Git plugin**.
    - **Workspace Cleanup Plugin**.
  
11. Choose **Download now and install after restart**.

12. On the installation progress page, you may check **Restart Jenkins when installation is complete** to immediately trigger the restart. Alternatively, log in as `root` onto the Jenkins master machine and execute:

    ```
    service jenkins restart
    ```

13. To be able to connect slave nodes to the Jenkins master, an ssh user must be defined. Open the Jenkins front end and go to **Manage Jenkins > Manage Credentials > Add Credentials > SSH Username with private key**.

    Note: in newer versions of Jenkins respective the credentials plugin, the **Manage Credentials** link might be missing. In this case, the definition of the credential is done together with the Jenkins slave definition below.

14. Enter the following data:  

    Field        | Value |
    :----------- | :--------  
    Scope	     | `Global` 
    Username     | `jenkins` 
    Private Key  | `From the Jenkins master ~/.ssh` 

15. If you are using the master itself for running builds, open **Manage Jenkins > Configure System** and go the **JDK** section.   
    Select **JDK installations...** and choose **Add JDK**.

    - Enter an appropriate symbolic name for your JDK installation used for builds.
    - Deselect **Install automatically**.
    - Enter the `JAVA_HOME` directory of the JDK installation.

    You will later enter JDKs for the slaves here also.
 
16. Save.


#### Further Enhancements

- Jenkins offers a sophisticated permission concept based on roles. After installation of the needed plugins Jenkins also supports SSH and LDAP. These features are required for a productively used Jenkins landscape to avoid misuse of administrative settings or unintended triggering of builds, for example.
- Since the Jenkins master constitutes a single point of failure, we also recommend ensuring some additional isolation from the rest of the network with a firewall.


### 3. Jenkins Slave

If you are planning to run a master-only scenario, you do not need a slave machine. However this section also contains some considerations for the master in a master-only approach; if this is your scenario follow only those steps that are marked with "master-only (MO)". If you plan to run a separate slave, follow all the steps.

Our examples are restricted to Linux as the operating system for the slaves, as we discuss only build technologies that do not depend on the underlying platform (like Java or Node.js). No explicit Jenkins software installation is required on the slave machines, other than some basic tools that are needed to run a build, like Java, Git and Maven. The Jenkins master connects to the slave using SSH and places the slave jar file on it, which then starts automatically.

> [Jenkins](https://jenkins-ci.org)  
> [Jenkins: Distributed builds](https://wiki.jenkins-ci.org/display/JENKINS/Distributed+builds)

#### Prerequisites

- Install Java JDK 1.7.0 or higher on the slave machine to run the Jenkins slave process. Add the path to the `java` command to the `PATH` environment variable of the machine.
- (MO) If you plan to compile Java applications (for example running on SAP Cloud Platform) on the slave, also install SAP JVM 7.1.

    > [SAP JVM download](https://tools.hana.ondemand.com/#cloud)  
    > [SAP JVM installation guide](https://help.sap.com/viewer/65de2977205c403bbc107264b8eccf4b/Cloud/en-US/76137f42711e1014839a8273b0e91070.html)
    
- (MO) Git installation.

    > [Git](https://git-scm.com)  
    > [Git download](https://git-scm.com/downloads)  
    > [Git installation guide](https://git-scm.com/book/en/v2/Getting-Started-Installing-Git)  
    
- (MO) Maven installation. The recommended version is Maven 3.0.5. Later versions might lead to build errors due to plugin incompatibilities.

    > [Maven](https://maven.apache.org)  
    > [Maven download](https://maven.apache.org/download.cgi)  
    > [Maven installation guide](https://maven.apache.org/install.html)

#### Procedure

1. On the slave machine, create an OS user `jenkins`. This is the user name used throughout this example; however you can also use any other OS user that runs the Jenkins builds on the slave.

2. Log in as user `jenkins`.

3. If it does not yet exist, create the file `.ssh/authorized_keys` in the `jenkins` home directory.

4. Copy the contents of the public SSH key file (`id_rsa.pub`) that was created on the Jenkins master (see the section above) as a new line in the `authorized_keys` file. This allows user `jenkins` of the Jenkins master to open an ssh login to `jenkins` on the slave machine.

5. Create a root directory for Jenkins, which is where the build job workspaces will be located. This example uses `/data/jenkins`.

6. Connect the Jenkins slave to the master. Open the Jenkins master front end and select **Manage Jenkins > Manage Nodes > New Node**.

7. Enter an appropriate node name (for example, the host name) and select **Dumb Slave**.

8. Configure the node using the following values:

    Field                  | Value 
    :--------------------- | :-------------------------------------- 
    Name                   | `<Any logical name, for example the hostname>`
    Description            | `<Any description>`
    Number of executors    | `<Number of parallel builds that are executable on the node. This depends on the load your build jobs produces and the available resources on the machine. Start with 1 or 2 and try to increase it.>`
    Remote root directory  | `/data/jenkins` 
    Labels                 | `builds`
    Usage                  | `Utilize this node as much as possible`
    Launch method          | `Launch slave agents on UNIX machines via SSH`
    Host                   | `<DNS name of the Jenkins slave>`
    Credentials            | `jenkins`.
    
    In the case that you have installed a newer Credentials Binding Plugin version and the credential `jenkins` is not yet defined, press **Add**, choose the **Jenkins** provider and enter the following values:
    
    Field                  | Value 
    :--------------------- | :-------------------------------------- 
    Domain                 | `Global credentials (unrestricted)`
    Kind                   | `SSH Username with private key`
    Username               | `jenkins`
    Private key            | `From the Jenkins master ~/.ssh`
    
    Press **Add**
    
9. Choose the **Advanced** button and continue with the configuration.

    Field                  | Value 
    :--------------------- | :-------------------------------------- 
    Java Path              | `<Path to the Java executable used by the slave process. This may differ from the SAP JVM to be used for the builds.>`
    Availability           | `Keep this slave on-line as much as possible`

10. **Save**.

11. (MO) Register the SAP JVM on the master. Open the Jenkins master front end and go to the JDK section **Manage Jenkins > Configure System**. Enter **JDK installations...** and choose **Add JDK**.

    - Enter an appropriate symbolic name for the SAP JVM 7.1 installation that is used for builds.
    - Deselect **Install automatically**.
    - Enter the `JAVA_HOME` directory of the SAP JVM 7.1 installation.

12. (MO) Declare the Maven installation path: go to **Manage Jenkins > Configure System** and enter:

    Field                 | Value |
    :-------------------- | :-------------------------------------  
    Maven installations   | Click **Add Maven**
    Name                  | `mvn`
    Install automatically | `unchecked`
    `MAVEN_HOME`          | `<Maven home path of the Maven installation directory on the slave or master (in the master-only scenario)>`
    
13. Either the slave connects automatically or you must explicitly select the **Connect** button. You see the executors readily waiting for jobs.

#### Further Enhancements

- As for the Jenkins master, you may want to use a firewall to guarantee the isolation of the builds to ensure reproducible build results independent of any side effects.
- When you work with many slaves, you have to create many identical copies of the Maven `settings.xml` file. You may also want to implement a publication mechanism to do it automatically.


> The content of this document is for guidance purposes only. No warranty or guarantees are provided.

## Next Steps
 
  - [Artifact Repository](https://www.sap.com/developer/tutorials/ci-best-practices-artifacts.html)
  - [Back to the Navigator](https://www.sap.com/developer/tutorials/ci-best-practices-intro.html)
