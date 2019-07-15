---
title: Work with Branches in GitHub and SAP Web IDE
description: Create branches in GitHub, switch branches and update application in a different branch in SAP Web IDE.
primary_tag: products>sap-web-ide
tags: [ tutorial>intermediate, topic>cloud, products>sap-hana, products>sap-web-ide, products>sap-cloud-platform ]
time: 15
---
## Prerequisites  
- **Tutorials:** [Create Issues and Assign to Collaborators](webide-github-issues-milestones)

## Details
### You will learn  
  - How your collaborators can create branches in GitHub
  - How to switch to the other branch in SAP Web IDE
  - How to make changes and commit the changes to the other branch

>This tutorial is written from a **collaborator's** point of view.


---

[ACCORDION-BEGIN [Step 1: ](Log into your SAP Cloud Platform cockpit)]

Go to [https://account.hanatrial.ondemand.com](https://account.hanatrial.ondemand.com) and log in to your SAP Cloud Platform cockpit.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open SAP Web IDE)]

To open SAP Web IDE, click the **Services** tab in the navigation bar, scroll down and click the **SAP Web IDE** tile to open the SAP Web IDE service page.

![Find SAP Web IDE](p5_2.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Clone Git repository)]

You'll have to clone the GitHub organization repository in SAP Web IDE. To do this

From the File menu in SAP Web IDE, click on **Git > Clone repository**.

![Clone repository](p5_3a.png)

Enter your GitHub account credentials and post the repository URL.

![Repository details](p5_3b.png)

The repository is now cloned.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create a branch)]

Create a branch to which you will make the changes.

Click the Git pane on the right-hand side and then click the **+** icon next to the branch label to create a branch.

![Create a branch](p5_4a.png)

Give a suitable name for the branch and then click **OK**.

![Branch details](p5_4b.png)

The branch is created and the branch that you are working with is displayed in the Git pane.

![Branch created](p5_4c.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Commit and push changes)]

Since this branch is dedicated to milestone `m1` milestone changes which has 2 issues to be targeted, you will implement the changes for each of these in separate commits.

For `issue#1` you will do changes in `S3_PurchaseOrderDetails.view.xml` to remove the `OrderedByName` entry.

Notice that when you save the changes, the Git pane shows the file that was changed. To commit the changes to the repository, click the **Stage** checkbox, enter a change description, and then click **Commit**.

> GitHub will automatically close an issue if your commit message includes [certain keywords](https://help.github.com/articles/closing-issues-via-commit-messages/) once this change is merged into the master branch. You will see this in the next tutorial.


![Add change description](p5_5b.png)

Click **Commit**  to commit changes to the repository and then **Push** and select **Remote branch** to push the changes to GitHub.


![Push to GitHub remote branch](p5_5c.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Check branch on GitHub)]

Now if you check the `m1` branch on the GitHub organization repository, you will see that the push has been completed and the changes are seen on GitHub.

![Push to GitHub remote branch is successful](p5_6.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Commit and push additional changes)]

Similarly you can address other remaining issues targeted for `m1` milestone and commit and push the changes.


[DONE]
[ACCORDION-END]
