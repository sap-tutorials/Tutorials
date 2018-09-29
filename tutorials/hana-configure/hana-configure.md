---
title: SAP HANA XS Classic, Getting started with SAP HANA and connecting the Web Workbench
description: Access your first data in a native SAP HANA Application.
primary_tag: products>sap-hana
tags: [ products>sap-hana, products>sap-hana-studio, products>sap-cloud-platform, topic>sql, topic>big-data, tutorial>beginner]
time: 20
---

## Next Steps
- [Develop your first SAP HANA XSC application](https://www.sap.com/developer/tutorials/hana-web-development-workbench.html)


## Details
### You will learn  
  - How to use SAP HANA Studio Perspectives
  - How to create a connection to the SAP HANA backend
  - How to get started with the SAP HANA web-based development workbench

---

[ACCORDION-BEGIN [Step 1: ](Host Configuration)]


Access your HANA instance that was created in ["How to create an SAP HANA Developer Edition in the Cloud"](https://www.sap.com/developer/tutorials/hana-setup-cloud.html).

Chose Notepad as the Open with Editor.

Replace the current IP address in front of the hostname `hanapm` with the specific IP address for this workshop which was supplied by your instructor.

Save the content and exit Notepad.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](HANA Studio Configuration)]

To add the HANA Studio Perspectives, open the HANA Studio via the Windows Start menu or via the Icon on your Start bar.

If your HANA Studio opens to the following Overview screen, simply press **Workbench** to return to the full Studio tooling:

![Studio Overview](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/4.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](SAP HANA Development perspective)]

To support the new developer centric workflow, there are two additional Eclipse Perspectives which have been added to SAP HANA Studio. These are not displayed by default.

In the upper right corner of your SAP HANA Studio, there is an add Perspectives button. Press this:

![Open Perspective](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/5.png)

Add the SAP HANA Development perspective. This is the perspective you should be using for almost this entire tutorial:

![HANA Development](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/6.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Add the Debug perspective)]

Repeat the step and add the Debug perspective:

![Debug](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/7.png)

After adding these two new perspectives, you may also still see the initial SAP HANA Administration Console perspective as well. If so, you can right mouse click and choose Close as we will not be using this perspective:

![Close](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/8.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Create a connection to the HANA server)]

Make sure you are in the SAP HANA Development perspective by clicking on the button:

![Add System](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/9.png)

Click on the **Systems** view.  Right click in the white space below this tab and choose **Add System...**.

![Properties](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/10.png)

- Input the server hostname: `hanapm`

- Input the instance number: `00`

- Enter a meaningful description of your choice. Press the **Next** button:

![Description](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/11.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Enter credentials for service)]

Enter the user id and password. The user id should be `WORKSHOP_<Group Number>`. Your group number is `01`. For this tutorial, your user id is `WORKSHOP_01`.

Enter the password: `HANARocks2015`

Click Store user name and password in secure storage.

Click **Finish**:

![Password](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/12.png)

You should now have a new connection with your specific user id for the HANA system. Please make sure to use this connection for the rest of the exercise.

> ### Note
>The System ID and users shown in these screen shots might be different than the ones you are working with and will often be blurred to avoid any confusion.

![System View](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/13.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Create a Repository Workspace)]


Switch to the **SAP HANA Repositories** view. You should see the system entry you created in the previous step and a default Workspace. Right mouse click and choose **Create Repository Workspace**:

![Workspace](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/14.png)

Confirm the file system location on your local machine which will hold the local copy of this Workspace. Click **Finish**:

![Finish](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/15.png)

You should now see the local workspace mapped to the remote workspace in the SAP HANA Repositories view:

![Repositories](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/16.png)

> ### Note
>Your System ID, User ID, Hostname, and System Numbers will be different than those displayed in subsequent screen shots. For this reason we have often blurred this information in screen shots to avoid confusion.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](View your work)]

To view what you have just created, deployed and activated you can view the URL on the server via your web browser.


![Web view](https://raw.githubusercontent.com/SAPDocuments/Tutorials/master/tutorials/hana-configure/39.png)

You have completed the exercise! You are now able to:
- Create a connection to the HANA server
- Launch the SAP HANA Web-based Development Workbench
- Create a schema
- Create access control files
- Grant access to the schema
- Create an `index.html` file to test your setup


[ACCORDION-END]


### Optional - Getting Help
If you need addition help resources beyond this document, we would suggest the following content:

* The [Online Help](https://help.sap.com/hana/SAP_HANA_Developer_Guide_en.pdf)
* The integrated help within SAP HANA Studio (content identical to the above mentioned online help)
* SAPUI5 SDK (installed on your HANA Server) `/sap/ui5/1/sdk/index.html#content/Overview.html`
