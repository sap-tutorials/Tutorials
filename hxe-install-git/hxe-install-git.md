---
title: How to install and setup git on HANA, express edition.
description: A How-To that shows how to install and setup the git source code management system on a HANA, express edition virtual machine.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, tutorial>how-to ]
---
## Prerequisites  
 - Proficiency: beginner
 - Setup: `HANA, express edition` must be running.

## Next Steps
 - Complete other tutorials.

## How-To Details
The HANA, express edition virtual machine uses SuSE Linux Enterprise Server (SLES) 12.1 as the guest operating system. The HANA, express edition release of SLES includes the Zypper package manager. This tutorial shows how to use the Zypper package manager to download packages needed for setting up a `git` repository. At the end of this tutorial, your HANA, express edition virtual machine should be configured so you can clone and use `git` repositories on your HANA, express edition virtual machine.

### Time to Complete
**20 Min**.

---

## Setting Up Proxies

1. If you are inside a firewall, you may need to configure HANA, express edition virtual machine proxies to be able to reach the internet. If you are not inside a firewall or you have already set up proxies on your virtual machine, skip ahead to `Setting Up Repositories`.

2. Open up the SuSE configuration tool `yast`. `yast` must be run as the root user.

    `sudo yast`

    This will open up the Yast Control Center.

3. Select `Network Services` in the left hand pane using the arrow keys. This will list all the `Network Services` in the right hand pane.

4. Tab over to the right hand pane.

5. Navigate to the `Proxy` service and hit the Enter key to select it. This will open the `Proxy Configuration` pane.

6. Select `Enable Proxy`.

7. Under `HTTP Proxy URL` enter your http proxy including the port number if necessary. For example, `http://myproxy.mydomain.com:8080`. Enter the appropriate values for the other protocols. You may want to select `Use the Same Proxy for All Protocols` if you use the same proxy for all protocols. Be sure that you your `No Proxy Domains` includes `localhost,127.0.0.1`.

8. Press `F10` key to save your changes.

9. Press `F9` to exit `yast`.

10. Exit the shell to make sure that your changes are picked up by the current shell.

## Setting Up Software Repositories and Installing Git

The HANA, express edition SuSE Linux Enterprise operating system includes the `ZYpp` system management library (`libzypp`) for managing software packages and the `zypper` command line tool for interfacing with `libzypp`. `Libzypp` stores packages in software repositories. The following instructions show how to configure the repositories needed by `libzypp` to install `git`. `Git` requires some packages from the `perl` repository on the `download.opensuse.org` site and several packages from the Source Code Management `scm` repository.

1. Add the Perl repository, and name it `PerlRepository`:

    `sudo zypper addrepo http://download.opensuse.org/repositories/devel:/languages:/perl/SLE_12_SP2/ PerlRepository`

    When prompted to trust the key to the `PerlRepository`, you can choose to trust it temporarily or always depending on your security requirements.

2. Add the Source Code Management repository and name it `SCMRepository`:

    `sudo zypper addrepo http://download.opensuse.org/repositories/devel:/tools:/scm/SLE_12_SP2/ SCMRepository`

    When prompted to trust the key to the `SCMRepository`, you can choose to trust it temporarily or always depending on your security requirements.

3. Install `git` using the `zypper` install command:

    `sudo zypper install git`

     When prompted to continue with package installation, enter 'y'. `Git` and all its dependent packages will be installed.

## Verifying the Git Installation

Verify your `git` installation by cloning a git repository to your HANA, express edition virtual machine. You can do this using any `git` repository to which you have access. For this example, you will clone the `PyHDB` repository from <www.github.com>. Feel free to clone a different repository.

1. Make a directory to store your `git` repositories:

    `mkdir mygitprojects`

2. Navigate to `mygitprojects`:

    `cd mygitprojects`

3. Clone the repository:

    `git clone https://github.com/SAP/PyHDB.git`

    You will see the message "Cloning into 'PyHDB'..."" followed by information on the number of repository objects being cloned. When the cloning is complete, you will see a line such as "Resolving deltas: 100% (738/738), done." ### Note: the number of deltas may vary depending on the release version of `PyHDB`.

    When the clone completes you will be ready to use git for source code management on your HANA, express edition virtual machine


## Next Steps
 - [View similar How-Tos](http://www.sap.com/developer/tutorials.html) or [View all How-Tos](http://www.sap.com/developer/tutorials.html)
 - [Complete the Django tutorials](https://docs.djangoproject.com/en/1.10/intro/tutorial01/)
