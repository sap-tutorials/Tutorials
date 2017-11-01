---
title: Use Open Social in a SAP Cloud Platform HTML5 application to integrate YouTube
description: Build and run the YouTube OpenSocial URL Gadget in a SAP Cloud Platform HTML5 application
primary_tag: products>sap-jam-collaboration
tags: [  tutorial>beginner, products>sap-jam, products>sap-cloud-platform, topic>cloud ]
---

## Prerequisites  
 - **Proficiency:** Intermediate

## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)

## Details
### You will learn  
In this tutorial you will learn how to build and run the YouTube OpenSocial URL Gadget in a SAP Cloud Platform HTML5 application.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: ](Download the YouTube OpenSocial URL Gadget Sample Code)]

Download and extract the YouTube OpenSocial URL Gadget for SAP Cloud Platform sample code.

1.  Download the gadget source code from our official SAP GitHub repository at:
    ```
    https://github.com/SAP/SAPJamSampleCode
    ```
2.  Extract the file.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Create your HTML5 application on SAP Cloud Platform)]

Create your HTML5 application on SAP Cloud Platform to provide hosting for your gadget.

1.  Switch to your SAP Cloud Platform account.
2.  Click  **Applications** > **HTML5 Applications**
3.  Click **New Application**.
4.  Enter `youtubeurlgadget` in the **Application Name** field.
5.  Click **Save**. Your HTML5 application is created.
6.  Click **Versioning**.
7.  Click **Edit Online**. The _Clone Git Repository_ dialog box appears.
8.  Click **Clone**. The _Git Ignore System Files_ dialog box appears.
9.  Click **Commit and Push**.

Your HTML5 application is now ready to use.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Upload sample code files to your HTML5 application)]

Upload the gadget (`url.\*`) and Cloud Platform application descriptor (`neo-app.json`) so they can be hosted by your HTML5 application.

1.  Select the **youtubeurlgadget** folder.
2.  Select **File** > **Import** > **From File System**. The _Import_ dialog box appears.
3.  Click **Browse**.
4.  Go to the `\OpenSocial\Gadget\youtubeurlgadget` folder in your local repository.
5.  Double click **neo-app.json**.
6.  Click **OK**.
7.  Select **File** > **Import** > **From File System**.
8.  Click **Browse**.
9.  Double click **url.html**.
10. Click **OK**.
11. Select **File** > **Import** > **From File System**.
12. Click **Browse**.
13. Double click **url.xml**.
14. Click **OK**.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Publish your HTML5 Application)]

Publish your HTML5 application to provide a publicly accessible URL that integrates with SAP Jam Collaboration.

1.  Right-click your **youtubeurlgadget** and select **Deploy** > **Deploy to SAP Cloud Platform**. The **Deploy Application to SAP Cloud Platform** dialog box appears.
2.  Click **Deploy**. The _Successfully Deployed_ dialog box appears.
3.  Click **Open the application's page in the SAP Cloud Platform cockpit**. A _SAP Cloud Platform_ tab appears.
4.  Click the **Application URL** link. The error message, `HTTP Status 404 - Resource not found` appears in a new tab.
5.  Replace `?hc_reset` with `url.xml` at the end of the URL. Your URL should appear as follows:
```
https://youtubeurlgadget-{YOUR_CLOUD_PLATFORM_ID}.dispatcher.hanatrial.ondemand.com/url.xml
```
6.  Reload the page with this new URL. The contents of the XML gadget appears.
7.  Copy the gadget URL.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Use your gadget in SAP Jam Collaboration)]

Use your gadget in a group in SAP Jam Collaboration.

1.  [Register](https://help.sap.com/viewer/u_collaboration_dev_help/0526a42d4e0d418186055384e46721f6.html) the URL of the `url.xml` file with SAP Jam Collaboration.
2.  Create a group in SAP Jam Collaboration.
3.  Add this gadget to your group.

You should now see the gadget in your group. Try using the gadget, add comments, send notifications, and collaborate\!

Feel free to modify, upload, and run this source code as much as you like. For more information about Open Social gadgets please refer to the [Open Social Gadgets](https://help.sap.com/viewer/u_collaboration_dev_help/df70ff966aa641aea2424b261ba7c34f.html) section of the  [SAP Jam Collaboration Developer Guide](https://help.sap.com/viewer/u_collaboration_dev_help).


[ACCORDION-END]


---


## Next Steps
- Select a tutorial from the [Tutorial Navigator](http://www.sap.com/developer/tutorial-navigator.html) or the [Tutorial Catalog](http://www.sap.com/developer/tutorials.html)
