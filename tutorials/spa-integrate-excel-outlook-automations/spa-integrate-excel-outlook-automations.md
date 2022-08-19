---
author_name: Ramakrishnan Raghuraman
author_profile: https://github.com/r3ksk
title: Integrate Excel & Outlook SDK based automations to single project
description: In this tutorial, we will customise our excel and outlook sdk based projects, so that they can be integrated together as a single business process.
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

## Details
SAP Process Automation has native integration to several Microsoft Office products including Outlook, Excel SDK. In this tutorial, you will integrate our automations to a single business process.

 We will create a new automation that includes both of our Outlook and Excel automations
 We will use a combination of environment variables and input/output variables to pass information across the project and in this way, processing will be dynamic and can work for any email or an excel file.

## Prerequisites
- Complete the tutorial: [Extend Outlook SDK based automation](spa-extend-outlook-automation)
- Complete the tutorial: [Build your first automation using Excel SDK of SAP Process Automation](spa-create-excel-automation)

### You will learn

  - How one automation in a project can be embedded into another automation project
  - How one automation can call another automation,
  - How to pass values one one automation to another automation
  - Finally in this process, how you can build an integrated business processes using SAP Process Automation

---

[ACCORDION-BEGIN [Step 1: ](Customise Outlook Automation)]

1. As a first step, let us add `baseFolderPath` as an environment variable
  !![Create Base Folder as Environment Variable](01-CreateEnvironmentVariable.png)

2. We will use this new path in **Save All Mail Attachments**
  !![Use the environment variable in Save Mail Attachments](02-SaveAsMailAttachmentNewFilePath.png)

3. To reuse this automation we will release the in preview project to released project. You can do so in two ways. (1) Either you can do so from Lobby by pressing **(...)** or from within the project you can do so by clicking the button **Release**
  !![Release Automation Project from SPA Lobby](03-ReleasefromSPALobby.png)

4. Another way to release the project
  !![Release Project from within Automation Project](04-ReleaseFromWithinTheProject.png)

5. At release time, it will ask you to specify a version number, you can accept the default suggestion
  !![Release Project Dialog](05-ReleaseVersionDialog.png)

6. Notice the name change from Editable to (version number) Released
  !![Change in Release status](06-ChangeinReleaseStatus.png)


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Customise Excel Automation)]

1. As a first step let us add **Get Folder Collection** to the `baseFolderPath` environment variable. This step will return all the sub folder names within a given folder. In our case, it will give each timestamp based folders fetched from each email from the outlook. Rename the output parameter to `folderList`
  !![Get Folder Collection](07-GetFolderCollection.png)

2. You can see the folder name by using a **Log Message**
  !![Log result of get folder collection](08-LogGetFolderCollectionsResult.png)

3. We will loop through each folder. for this will add **For Each** loop to the flow
  !![Add First For Each on Folder Collection Result](09-AddForEach.png)

4. We will add all the required steps into the folder. the idea is on each file, a certain set of actions needs to be performed
  !![Move Excel actions inside the loop](10-MoveExcelActionsInsideForEachLoop.png)

5. Within each index of **For Each**,  we can fetch list of excel files in each folder. for this, we will add **Get File Collection** to the loop
  !![Add Get File within a folder](11-AddGetFIleCollection.png)

6. the value for **Get File Collection** is as given in the image
  !![Log output of Get File List Collection](12-GetFileListValue.png)

7. We will add one more **For Each** to loop through each excel file in that sub folder. Here to distinguish both the loop variables, you can rename it appropriately. Also include the required excel actions within this sub **For Each** loop.
  !![Add a second ForEach](13-AddSecondForEach.png)

8. first create a string variable to generate from the loop variable full path to the excel file
  !![Add a File Path Variable](14-AddFilePathVariable.png)

9. Adjust the **Get File Name** to take the output variable
  !![get the file name based on these changes](15-GetFileName.png)

10. We have hard coded ranges in **Filter Range**. when we generalise the functionality, we may wanted to calculate this at run time. For this, we will be adding `Get UsedRange Row` and Get `UsedRange` Column activities to find out the start and end cell values.
![Amend Filter Range with Dynamic Calculations](16-AddStarRangeStarActivities.png)

11. We will now use a custom script activity to calculate complete range.
    !![Add a custom script to calculate range of excel](17-AddCustomScript.png)

    ```JavaScript
    var startColumn = 1;
    var startCell = String.fromCharCode(startColumn + 64) + startColumn;
    var endCell = String.fromCharCode(columnRange + 64) + rowRange;
    return startCell + ":" + endCell;
    ```

12. just for demonstration purpose, we will filter against first column and look for cell values containing a
  !![Apply Filter Range on First column](18-FilterRangeDynamically.png)

13. We will use similar approach to find out range of values after filtering to be used with **Copy Range**
  !![Similarly use dynamic calculations on Copy Range](19-DynamicRangeCalculation.png)

14. We will append "\_new" to the new CSV file name in **Save As Workbook**
  !![Use the loop variable for Save as Workbook](20-SaveAsWorkbookPath.png)

15. Like Outlook sample, you need to release the package and publish to the library. You can do so either from the SAP Process Automation Lobby or from within the project.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](New Integration Automation Project)]

1. Let us create a brand new Automation project **Main Automation**
  !![Create a new Automation Project](21-CreateANewProject.png)

2. Include both Excel and Outlook projects as dependency. Here I am adding Excel Project. Similarly you have to add outlook sample project
  !![Add Excel and Outlook projects as dependency](22-AddExcelSampleAsDependency.png)

3. this is the result after adding both projects as dependency
  !![final dependent projects added](23-DependentProjectsAdded.png)

4. We will call this new project as **Main Automation** project
  !![project creation](24-NewAutomationCreation.png)

5. Add both outlook sample and excel sample projects (i named it as `Excel Cloud Link`) to the flow
  !![add 2 automations to the flow](25-AddAutoamtions2TheMain.png)

6. Save Project and test the project to see the result by yourself

7. In the end, when this tutorial was tested, we used this below source data
  !![Source Excel Data](26-OriginalExcelData.png)

8. After Automaton is executed, it has filtered the data (Town starting with a), hide a column (Column F with title `storey_range`) and resultant excel will look like this
  !![Final Excel Data](27-AfterAutomationProcessing.png)


[VALIDATE_1]
[ACCORDION-END]
