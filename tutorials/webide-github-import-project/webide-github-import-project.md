---
title: Creating a project in SAP Web IDE and importing it into Github
description: Part 3 of 6, Collaborative Development Using Github and SAP Web IDE
tags: [ tutorial>intermediate, topic>github, topic>cloud, products>sap-hana, products>sap-web-ide, products>sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Tutorials:** 
  - Create a repo within the Organization
  - [Build an app from an SAP Web IDE template](http://go.sap.com/developer/tutorials/hcp-template-mobile-web-app.html)

## Next Steps
 - Using issues, setting milestones and assigning issues to collaborators

## Details
### You will learn  
In this tutorial, you'll learn how to import an existing SAP Web IDE project into your previously created Github repository.

### Time to Complete
**15 Min**.

---

1. Go to [https://account.hanatrial.ondemand.com](https://account.hanatrial.ondemand.com) and log in to your SAP HANA Cloud Platform cockpit.

2. To open SAP Web IDE, click on the **Services** tab in the navigation bar, scroll down and then click the **SAP Web IDE** tile to open the SAP Web IDE service page.

    ![Find SAP Web IDE](p3_2.png)

3. Click on **Open SAP Web IDE** to launch SAP Web IDE.

    ![Open SAP Web IDE](p3_3.png)

4. Open your existing project in SAP Web IDE by clicking on **File** &rarr; **Import**.

5. Once your project is open, right-click on your Project folder and click on **Git** &rarr; **Initialize Local Repository**.

    ![Create local Git repository](p3_5.png)

6. In the **Configure Git Repository** dialog box, enter your Github Organization repository's URL

    a. Fetch the Remote repository URL by heading over to the Github Organization URL. Click on the **Clone or Download** button and then select the icon to copy URL to clipboard selecting the Repository.

    ![Select remote repository URL](p3_6a.png)

    b. Paste this URL into the remote repository textbox.

    ![Paste remote repository URL](p3_6b.png)

7. Your project is now initialized with a Git repository and linked to the Github platform. You can open the Git pane on the right sidebar to verify this.

    ![Git repository initialized](p3_7.png)

8. Next, commit the project files to the local Git repository by clicking on **Stage all** checkbox, enter a commit description and then click on the **Commit** button.

    ![Git changes committed](p3_8.png)
  
9. Next to synchronize Github repository and local repository changes, click on the **Pull** button. Enter your Github credentials.

    ![Pull Github changes](p3_9.png)

10. Once the pull is complete you'll notice the README file created on Github is now available in Web IDE project workspace.

    ![Pull complete](p3_10.png)

11. To push your local changes to Github.

    a. Click on the **Push** button and select **Remote Branch**

    ![Push to Github](p3_11a.png)

    b. Select **origin/master** as the remote branch and click on **OK**.

    ![Select Github remote](p3_11b.png)

    c. Enter your Github credentials and click on **OK**.

    d. Once push is completed you will see a notification message on the top right corner.

    ![Push completed](p3_11d.png)

12. Go to the Organization Github URL and you will notice that the changes are now published.


## Next Steps
 - Using issues, setting milestones and assigning issues to collaborators