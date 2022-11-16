---
parser: v2
author_name: Ramakrishnan Raghuraman
author_profile: https://github.com/r3ksk
auto_validation: true
time: 20
tags: [ tutorial>beginner, software-product>sap-business-technology-platform, tutorial>free-tier]
primary_tag: software-product>sap-process-automation
---

# Integrate Excel and Outlook SDK Based Automations To A Single Project
<!-- description --> In this tutorial, you will customise the Excel and Outlook SDK based projects, so that they can be integrated together as a single business process.

## Prerequisites
- Complete the tutorial: [Extend Outlook SDK based automation](spa-extend-outlook-automation)
- Complete the tutorial: [Build Your First Automation Using Excel SDK of SAP Process Automation](spa-create-excel-automation)

## You will learn
  - How one automation in a project can be embedded into another automation project
  - How one automation can call another automation,
  - How to pass values one one automation to another automation
  - How you can build an integrated business processes using SAP Process Automation

## Intro
SAP Process Automation has native integration to several Microsoft Office products including Outlook, Excel SDK. In this tutorial, you will integrate the automations to a single business process. Create a new automation that includes both the Outlook and Excel automations. Use a combination of environment variables and input/output variables to pass information across the project and in this way, processing will be dynamic and can work for any email or an Excel file.

---

### Customise Outlook Automation


1. In your **Outlook Sample** project, add `baseFolderPath` as an environment variable
  <!-- border -->![Create Base Folder as Environment Variable](01-CreateEnvironmentVariable.png)

2. Use this new path in **Save All Mail Attachments**
  <!-- border -->![Use the environment variable in Save Mail Attachments](02-SaveAsMailAttachmentNewFilePath.png)

3. To reuse this automation, release the **in preview** project. You can do so in two ways.
    - Either you can do so from Lobby by pressing **(...)**
    - From within the project you can do so by clicking the button **Release**
  <!-- border -->![Release Automation Project from SPA Lobby](03-ReleasefromSPALobby.png)

4. Another way to release the project
  <!-- border -->![Release Project from within Automation Project](04-ReleaseFromWithinTheProject.png)

5. At release time, it will ask you to specify a version number, you can accept the default suggestion
  <!-- border -->![Release Project Dialog](05-ReleaseVersionDialog.png)

6. Notice the name change from Editable to (version number) Released
  <!-- border -->![Change in Release status](06-ChangeinReleaseStatus.png)



### Customise Excel Automation


1. In your Excel project, add **Get Folder Collection** to the `baseFolderPath` environment variable. This step will return all the sub folder names within a given folder. In this case, it will give each timestamp based folders fetched from each email from the Outlook. Rename the output parameter to `folderList`
  <!-- border -->![Get Folder Collection](07-GetFolderCollection.png)

2. You can see the folder name by using a **Log Message**
  <!-- border -->![Log result of get folder collection](08-LogGetFolderCollectionsResult.png)

3. Loop through each folder. for this will add **For Each** loop to the flow
  <!-- border -->![Add First For Each on Folder Collection Result](09-AddForEach.png)

4. Add all the required steps into the folder. The idea is on each file, a certain set of actions needs to be performed
  <!-- border -->![Move Excel actions inside the loop](10-MoveExcelActionsInsideForEachLoop.png)

5. Within each index of **For Each**, fetch list of Excel files in each folder. for this, Add **Get File Collection** to the loop
  <!-- border -->![Add Get File within a folder](11-AddGetFIleCollection.png)

6. The value for **Get File Collection** is as given in the image
  <!-- border -->![Log output of Get File List Collection](12-GetFileListValue.png)

7. Add one more **For Each** to loop through each Excel file in that sub folder. Here to distinguish both the loop variables, you can rename it appropriately. Also include the required Excel actions within this sub **For Each** loop.
  <!-- border -->![Add a second ForEach](13-AddSecondForEach.png)

8. First create a string variable and assign the loop variable's full path to the Excel file
  <!-- border -->![Add a File Path Variable](14-AddFilePathVariable.png)

9. Adjust the **Get File Name** to take the output variable
  <!-- border -->![get the file name based on these changes](15-GetFileName.png)

10. This tutorial uses hard coded ranges in **Filter Range**. When you generalise the logic to your needs, you may wanted to calculate this at run time. For example, add `Get UsedRange Row` and Get `UsedRange` Column activities to find out the start and end cell values.
![Amend Filter Range with Dynamic Calculations](16-AddStarRangeStarActivities.png)

11. Now use a custom script activity to calculate complete range.
    <!-- border -->![Add a custom script to calculate range of Excel](17-AddCustomScript.png)

    ```JavaScript
    var startColumn = 1;
    var startCell = String.fromCharCode(startColumn + 64) + startColumn;
    var endCell = String.fromCharCode(columnRange + 64) + rowRange;
    return startCell + ":" + endCell;
    ```

12. Just for demonstration purpose, filter against the first column and look for cell values containing a
  <!-- border -->![Apply Filter Range on First column](18-FilterRangeDynamically.png)

13. Use similar approach to find out range of values after filtering to be used with **Copy Range**
  <!-- border -->![Similarly use dynamic calculations on Copy Range](19-DynamicRangeCalculation.png)

14. Append **"\_new"** to the new CSV file name in **Save As Workbook**
  <!-- border -->![Use the loop variable for Save as Workbook](20-SaveAsWorkbookPath.png)

15. Like the Outlook sample, you need to release the package and publish to the library. You can do so either from the SAP Process Automation Lobby or from within the project.


### Create Integrated Automation Project


1. Create a brand new Automation project **Main Automation**
  <!-- border -->![Create a new Automation Project](21-CreateANewProject.png)

2. Include both Excel and Outlook projects as dependency. Here I am adding Excel Project. Similarly you have to add Outlook based project.
  <!-- border -->![Add Excel and Outlook projects as dependency](22-AddExcelSampleAsDependency.png)

3. This is the result after adding both project's packages as dependency
  <!-- border -->![final dependent projects added](23-DependentProjectsAdded.png)

4. Call this new project as **Main Automation** project
  <!-- border -->![project creation](24-NewAutomationCreation.png)

5. Add automations from both Outlook sample and Excel sample projects to the flow.
   <!-- border -->![add 2 automations to the flow](25-AddAutoamtions2TheMain.png)

    > Don't be confused with the name. There are Package/Project Name(s) and Automation name(s). In this step, we will be using automations

6. Save and test the project.

7. In the end, when this tutorial was tested, the below source data was used.
  <!-- border -->![Source Excel Data](26-OriginalExcelData.png)

8. After Automaton is executed, it has filtered the data (Town starting with a), hide a column (Column F with title `storey_range`) and resultant Excel will look like this
  <!-- border -->![Final Excel Data](27-AfterAutomationProcessing.png)
