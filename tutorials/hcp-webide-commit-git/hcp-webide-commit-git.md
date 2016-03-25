---
title: Commit your project files to your HCP Git respository
description: Learn how to commit and push your project files to the Git repository built into your HCP account.
tags: [tutorial:product/hcp, tutorial:product/sapui5_web_ide, tutorial:product/mobile, tutorial:product/sap_ui5]
---

## Prerequisites
 - **Proficiency:** Intermediate
 - **Tutorials:** [Calculate and display a new field in an SAPUI5 app](http://go.sap.com/developer/tutorials/hcp-webide-calculate-new-field.html)

## Next Steps
 - [Localizing your SAPUI5 app](http://go.sap.com/developer/tutorials/hcp-webide-localizing-app.html)

## Details

### You will learn
[Git](https://try.github.io/levels/1/challenges/1) is a widely used source code management system for software development, and is built into your SAP HANA Cloud Platform account, and integrated with SAP Web IDE.

There are many reasons why you might want to use a source code management tool. Some obvious ones are:

 - You made a mistake during development and want to return to a previous version of a file (or all)
 - Capture the state of code for a deployed application
 - Work in a development team and need to merge changes from individual "branches" of the code into the "main line" of the code streamIn this tutorial, you will learn how to commit and "push" your project files to your Git repository. In addition to being a good idea, your project files will need to be in Git for the next tutorial where you will [Localize your SAPUI5 app](http://go.sap.com/developer/tutorials/hcp-webide-localizing-app.html)

When you deployed your application to SAP HANA Cloud Platform, you checked the **Connect to the SAP HANA Cloud Platform Git repository** checkbox. After deployment, you may have noticed the Git "decorations" or icons indicating Git status started to appear next to your files and folders. There are six different states which are indicated as shown in the table below. As you work through this tutorial, watch for the decorations to change from New file > Modified and Staged > Committed. After a commit, and changes in a file will be indicated with one asterisk (indicating that the file is out of sync with the version in Git).

Decoration                                                   | Meaning
:--------------------------------------------------------:   | :-------------
![committed](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_git_committed.png)                     | Committed file (version in your project matches that in Git)
![modified not staged](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_git_mod_not_staged.png)      | Modified file that has not been staged
![modified staged](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_git_mod_staged.png)              | Modified file that has been staged
![committed](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_git_new_file.png)                      | New file
![deleted files](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_git_folder_with_deleted_files.png) | Folder containing deleted files
![conflicts](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_git_file_merge_conflicts.png)          | File with merge conflicts

 
### Time to Complete
**5 min**

---

1. Open SAP Web IDE in a browser window, and open your project folder (`northwind`). Note the **New file** decorations.

    ![project files with Git status](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_1.png)

2. Click on the **Git pane** icon on the right hand side of the SAP Web IDE window.

    If your project does not appear in the **Git pane**, keep the Git pane open and click on the project folder again. 

    ![Git pane](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_2.png)

3. You may select individual files to commit, but to commit all project files, click the **Stage All** checkbox.

    ![staging all files](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_3.png)
    
    Notice the change in the decorations for your files and folders.
    
    ![staged decorations](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_3a.png)

4. Scroll down in the **Git pane** and enter a description (required for any commits). A description like `Initial commit of northwind app` would work well.

    ![commit description](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_4.png)

5. Click on the **Commit and Push** button, and select **origin/master**, from the pop up menu. This will add your files to the master branch (in other source control systems this is sometimes referred to as the "trunk").

    ![commit description](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_5.png)

6. Log in with your user ID and account password and click **OK**.

    ![commit description](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_6.png)

7. You will see a confirmation alert appear briefly in the top-right corner of the Web IDE window (just below **Logout**), and you will see the decorations change to the **green circles** indicating that the files have been committed (and the version in your project matches the version in the Git repository). 

    ![commit description](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hcp-webide-commit-git/mob_3_2a_7.png)

## Next Steps
 - [Localizing your SAPUI5 app](http://go.sap.com/developer/tutorials/hcp-webide-localizing-app.html)