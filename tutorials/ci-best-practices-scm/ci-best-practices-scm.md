---

title: CI Best Practices Guide: Source Code Versioning System
description: Part 3.1: Setting up a Git/Gerrit Instance.
tags: [  tutorial>intermediate, tutorial:type/project ]

---

## Prerequisites

  - **Proficiency:** Intermediate

## Next Steps
 
  - [Build Scheduler](http://go.sap.com/developer/tutorials/ci-best-practices-build.html)
  - [Back to the Navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)

---


What we offer in this part is not more than a basic recipe to set up a minimum installation including only those components on Linux that we consider as absolutely necessary to run a CI/CD process for development with SAP. However, the setup best suited to your concrete requirements cannot be part of this document because it highly depends on your concrete local situation, the network setup, the overall landscape into which the CI/CD process will be embedded, and so on. Therefore, we will restrict ourselves here to showing only the principles and the core elements.

If you are interested in other examples of CI/CD processes with a focus on specific needs and local conditions, just follow the references we provide to the official documentation for the described components. For each component, we provide hints to how the described setup could be improved. This concerns the professional and reliable operation, for example, further security activities and operational refinements.

Note: This document is restricted to the description of component installation on Linux. For installation on Windows, we refer to the installation documentation on the web.



The interface between local (single developer) and collaborative (team) development is the source code versioning tool together with a code review system. The source code versioning tool is as well the interface to any sequel step, most importantly, the build step in the overall CI process.

There are many different possibilities to implement a source code versioning and review system. In this document, we show
how to use Git as SCM tool and Gerrit as Git repository server and code review tool.
These tools are very frequently used.

### Git/Gerrit

Gerrit is available as open source and comes with a built-in installation of Git.

> Homepage: https://code.google.com/p/gerrit  
> Downloads: http://gerrit-releases.storage.googleapis.com/index.html  
> Installation guide: https://gerrit.googlecode.com/svn/documentation/2.0/install.html  

#### Prerequisites

  - Java JDK 1.7.0 or higher is installed on the machine.

#### Procedure

  1. On the hosting machine, create an OS user `gerrit`.
  2. Create a Gerrit installation directory owned by `gerrit`. 
     In this document, we call it `/data/gerrit`, but any other directory that follows the respective conventions will work as well. 
     Ensure that the file partition of the directory is big enough to store all the Gerrit data, especially the database.
  3. Download the Gerrit installation (`gerrit.war` file) from the download site and save it to `/data/gerrit`.
  4. Log on as `gerrit`.
  5. Execute the following commands to do the installation and to prepare the Gerrit database:
     ```
     cd /data/gerrit
     java -jar gerrit.war init -d /data/gerrit
     java -jar gerrit.war reindex -d /data/gerrit
     ```
     During the installation procedure, you may approve any configuration proposal with "Enter".
  6. To get the server up quickly with the ability to administer it, open the Gerrit configuration file `/data/gerrit/etc/gerrit.config` and set the authentication method to:
     ```
     [auth]
             type = DEVELOPMENT_BECOME_ANY_ACCOUNT
     ```
     For security reasons, you should change this to an appropriate authentication method as soon as possible.
  7. Start the Gerrit daemon.
     ```
     cd /data/gerrit/bin
     ./gerrit.sh start
     ```
  8. The Gerrit application should now be accessible with a browser on port 8080.
  9. In the Gerrit front end, click on "Become" and sign up. 
     This first account has admin rights by default.

#### Further Enhancements

  - The `gerrit.war` package includes a standalone Jerry servlet container. Deployment on other JEE run times is possible.
  - Choose an appropriate authentication method.
  - The default database installation used by Gerrit is H2. For larger installations, you can use MySQL or Postgres.
  - Encapsulate the `gerrit.sh` into startup scripts in a way that Gerrit starts automatically with machine reboot.


## Next Steps
 
  - [Build Scheduler](http://go.sap.com/developer/tutorials/ci-best-practices-build.html)
  - [Back to the Navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)
