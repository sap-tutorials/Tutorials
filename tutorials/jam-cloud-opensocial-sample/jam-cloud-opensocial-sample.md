---
title: Use Open Social in a SAP Cloud Platform HTML5 application to integrate Lumira
description: Build and run a Lumira Open Social Gadget in a SAP Cloud Platform HTML5 application
primary_tag: products>sap-jam-collaboration
tags: [  tutorial>beginner, products>sap-jam, products>sap-cloud-platform, topic>cloud ]
---

## Prerequisites  
 - **Proficiency:** Intermediate

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
### You will learn  
In this tutorial you will learn how to build and run a Lumira Open Social Gadget in a SAP Cloud Platform HTML5 application.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download the OpenSocial SAP Cloud Platform Lumira Gadget Sample Code)]

In this first procedure, download and extract the Lumira Open Social Gadget for SAP Cloud Platform sample code.

1.  Download the gadget source code from our official SAP GitHub repository at:
    ```
    https://github.com/SAP/SAPJamSampleCode
    ```
2.  Extract the file.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create an HTML5 application on SAP Cloud Platform)]

1.  Switch to your SAP Cloud Platform account.
2.  Click **HTML5 Applications**.
3.  Click **New Application**.
4.  Enter **SAP Cloud Platform gadget** in the **Application Name** field.
5.  Click **SAP Cloud Platform gadget**.
6.  Click **Versioning**.
7.  Click **Edit Online**.
8.  Enter your _username_ (C,I,P Number) in the **User** field.
9.  Enter your _password_ in the **Password** field.
10. Select **Remember Me**.
11. Click **OK**.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Add the Open Social Gadget files to the HTML5 application)]

1.  Select the `hanacloudplatformgadget` folder.
2.  Select ![Start of the navigation path](images/navstart.gif) **File** ![Next navigationstep](images/navstep.gif) **Import** ![Nextnavigation step](images/navstep.gif) **From File System** ![End of the navigation path](images/navend.gif).
3.  Click **Browse**.
4.  Go to the `\\OpenSocial\\Gadget\\HCP\_Lumira` folder in your local repository.
5.  Double click `HCP\_Lumira.html`.
6.  Click **OK**.
7.  Select ![Start of the navigation path](images/navstart.gif) **File** ![Next navigationstep](images/navstep.gif) **Import** ![Nextnavigation step](images/navstep.gif) **From File System** ![End of the navigation path](images/navend.gif).
8.  Click **Browse**.
9.  Double click `HCP\_Lumira.xml`.
10. Click **OK**.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Create the Application Descriptor File)]

1.  Select ![Start of the navigation path](images/navstart.gif) **File** ![Next navigation step](images/navstep.gif) **New** ![Next navigation step](images/navstep.gif) **File** ![End of the navigation path](images/navend.gif).
2.  Enter `neo-app.json` in the **File Name** field.
3.  Copy and paste the following lines into your `neo-app.json` file and set authentication method to `none`:
    ```
    {
        "authenticationMethod": "none"
    }
    ```
4.  Click **Save**.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Publish the HTML5 Application)]

1.  Right-click `HCP\_Lumira.xml` and select ![Start of the navigationpath](images/navstart.gif) **Deploy** ![Next navigationstep](images/navstep.gif) **Deploy to SAP Cloud Platform** ![End of the navigation path](images/navend.gif).

2.  Enter your _password_ in the **Password** field.

3.  Click **Deploy**.

4.  Click **Open the active version of the application**. The error message, "HTTP Status 404 - Resource not found" is displayed.

5.  Add `/HCP\_Lumira.xml` to the end of the URL.

6.  Load the page with this new URL. The contents of the XML gadget is displayed.

7.  Copy the gadget URL.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Use your gadget in SAP Jam Collaboration)]

1.  [Register](https://help.sap.com/viewer/u_collaboration_dev_help/0526a42d4e0d418186055384e46721f6.html) the URL of the `HCP\_Lumira.xml` file with SAP Jam.
2.  Create a SAP Jam group.
3.  Add this gadget to your SAP Jam group.

You should now see the gadget in your SAP Jam group. Try using the gadget, add comments, send notifications, and collaborate\!

Feel free to modify, upload, and run this source code as much as you like. For more information about Open Social gadgets please refer to the [Open Social Gadgets](https://help.sap.com/viewer/u_collaboration_dev_help/df70ff966aa641aea2424b261ba7c34f.html) section of the SAP Jam Collaboration Developer Guide.


[ACCORDION-END]


---


## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
