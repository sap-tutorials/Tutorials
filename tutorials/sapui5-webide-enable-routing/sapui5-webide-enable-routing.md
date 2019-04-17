---
title: Enable Routing
description: Enable routing to embed one view into another
time: 5
primary_tag: topic>sapui5
author_name: Marius Obert
author_profile: https://github.com/iobert
tags: [  tutorial>intermediate, topic>html5, topic>sapui5, products>sap-cloud-platform, products>sap-cloud-platform-for-the-cloud-foundry-environment, products>sap-web-ide ]

---

## Details
### You will learn  
Embed one view into another one with routing to prepare for later tutorials. Edit the routing configuration, which is stored within the app descriptor `manifest.json`.

---


[ACCORDION-BEGIN [Step : ](Create a new SAPUI5 View)]
1.  Create a new SAPUI5 View called `App`.  

    To create a new SAPUI5 View, right click in the **`webapp`** folder, and then choose **New > SAPUI5 View**.

    > **IMPORTANT!** Be sure to right click in the **`webapp`** folder.  If you choose the wrong folder, the files will be created in the wrong place.  If this happens, delete the folders and start over.

    ![right click on webapp and select New - SAPUI5 View](1.png)

2.  In the dialog box, change the **View Name** to `App`.  Then click on **Next**.  

    In the next page, click on **Finish**.  (You do not need to overwrite the existing file.)

    ![enter file name](1b.png)

3.  Open the `mta_app/app/webapp/view/App.view.xml` file, and replace the `<App>` tag with the following code:

    ```XML
	   <App id="app"/>
    ```

    ![Add code to App.view.xml](2.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Rename the new route)]
1.  Open the `Mainfest.json` file again.  Click on the **Routing** tab at the top of the screen.



    ![routing](3.png)

2.  Scroll down and re-name the existing route to **`home`** and clear the **Pattern** field. Also change the **View Level** of the **TargetView1** to **`1`**.

    ![Add a new target](4.png)

3.  **Save** the file.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step : ](Change the root view)]
1. In the same `manifest.json` file, we will switch to the **Code Editor** tab, which is on the bottom of the screen.

2. Scroll to the `rootView` section, and *Change* the following lines:

    ```JavaScript
    "viewName": "sapcp.cf.tutorial.app.view.App",
    ```

    ```JavaScript
    "id": "App"
    ```

    ![Update manifest.json to use new view](5.png)

> After this tutorial, you should be able to run your application again, but you will not see any changes.  All of the routing work occurs "behind the scenes" and is not visible to the user.   


[DONE]
[ACCORDION-END]

----
