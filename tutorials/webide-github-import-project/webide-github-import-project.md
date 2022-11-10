---
parser: v2
primary_tag: products>sap-web-ide
tags: [ tutorial>intermediate, topic>cloud, products>sap-hana, products>sap-web-ide, products>sap-cloud-platform, tutorial>license ]
time: 15
---


# Create a Project in SAP Web IDE and Push into GitHub
<!-- description --> Create a project in SAP Web IDE and push it into GitHub to support collaborative development using GitHub and SAP Web IDE.

## Prerequisites
- **IMPORTANT**: This tutorial cannot be completed on a trial account


## You will learn  
  - How to import an existing SAP Web IDE project into your previously created GitHub repository

---


### Log into your SAP Cloud Platform cockpit


Go to [https://account.hanatrial.ondemand.com](https://account.hanatrial.ondemand.com) and log into your SAP Cloud Platform cockpit.


### Open SAP Web IDE


1. Click **Neo Trial**, and then open the **Services** tab.

2. Scroll down and click the **SAP Web IDE** tile to open the SAP Web IDE service page.

    ![Find SAP Web IDE](p3_2.png)

3. Click **Go to Service** to launch SAP Web IDE.

    ![Open SAP Web IDE](p3_3.png)


### Open your project


Open a project in SAP Web IDE. Either:

  - Use an existing project.
  - Import an existing project (`.zip` file).
  - Create a new one by going to **File > New > Project from Template**, and then use, for example, the **SAPUI Application** template.


### Initialize local repository


Right-click on your project folder and click **Git > Initialize Local Repository**.

![Create local Git repository](p3_5.png)


### Set the remote repository


You need to connect the local Git repository in SAP Web IDE with your repository on GitHub.

1. Right-click on your project folder and click **Git > Set Remote**.

2. In the **Configure Git Repository** dialog box, enter your GitHub organization repository's URL.

    Get the remote repository URL by heading over to GitHub organization, and open your repository. Click the **Clone or Download** button and then select the icon to copy the URL to the clipboard.

    ![Select remote repository URL](p3_6a.png)

    Paste this URL into the remote repository textbox.

    ![Paste remote repository URL](p3_6b.png)

3. Click **OK**.

This automatically executes a fetch from the remote repository, and you can view the branches/commits that will be fetched. Click **OK**.

![Set remote](setRemoteFetch.png)


### Verify Git repository


Your project is now initialized with a Git repository and linked to the GitHub platform. You can open the Git pane on the right sidebar to verify this.

![Git repository initialized](p3_7.png)


### Stage and commit files


Commit the project files to the local Git repository by doing the following:

1. Make sure the files are staged (the **Stage** checkbox is checked).

2. Enter a commit message.

3. Click **Commit**.

![Git changes committed](p3_8.png)


### Pull from GitHub


To synchronize the GitHub repository and local repository changes, click **Pull**. Enter your GitHub credentials.

![Pull GitHub changes](p3_9.png)


### View downloaded README


Once the pull is complete, you'll notice the `README.md` file that was created on GitHub is now available in the SAP Web IDE project workspace.

![Pull complete](p3_10.png)


### Push your local changes to GitHub


Click **Push** and select **Remote Branch**

![Push to GitHub](p3_11a.png)

Select **origin/master** as the remote branch and click **OK**.

![Select GitHub remote](p3_11b.png)

Enter your GitHub credentials and click **OK**.

Once the push is complete, you will see a notification message on the top-right corner.

![Push completed](p3_11d.png)


### Check published changes


Go to your organization on GitHub and you will notice that the changes are now published.

