---
parser: v2
auto_validation: true
time: 15
tags: [ tutorial>beginner, topic>Cloud, software-product>sap-business-technology-platform, tutorial>license]
primary_tag: software-product>sap-hana-service-for-sap-btp  
author_name: Sylvia Hördt
author_profile: https://github.com/sapsy
---

# Create an SAP HANA XS Classic Hello World Application Using SAP HANA Studio
<!-- description --> Create and test a simple SAP HANA XS classic application that displays the "Hello World" message.

## Prerequisites
 - You have a subaccount in the SAP BTP, Neo environment.
 - You're assigned the Administrator role for the subaccount.
 - Make sure the SAP HANA tenant database you want to use is deployed in your subaccount before you begin with this tutorial. For more information, see [Install Database Systems](https://help.sap.com/viewer/d4790b2de2f4429db6f3dff54e4d7b3a/Cloud/en-US/1261e6b87e174c05b774ea38fa3c8c51.html).
 - You've installed the tools as described in [Install SAP HANA Tools for Eclipse](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/b0e351ada628458cb8906f55bcac4755.html) to follow the steps described in this tutorial.

## You will learn
  - How to create and test a simple SAP HANA XS classic application using an SAP HANA tenant database system (MDC)

---

### Create a database user


Create a new database user in the SAP HANA cockpit and assign the user the required permissions for working with the SAP HANA Web-based Development Workbench.

You'll perform all subsequent activities with this new user.

> You've specified a password for the SYSTEM user when you created an SAP HANA tenant database. You now use the SYSTEM user to log on to the SAP HANA cockpit and create your own database administration user.

> **Caution:** You should not use the SYSTEM user for day-to-day activities. Instead, use this user to create dedicated database users for administrative tasks and to assign privileges to these users.

1.  In the SAP BTP cockpit, navigate to a subaccount. For more information, see [Navigate in the Cockpit](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/fdeff7e68f64496eb8a1fb31f6a08b73.html).

2.  Choose **SAP HANA / SAP ASE** > **Databases & Schemas** in the navigation area.

    All databases available in the selected account are listed with their ID, type, version, and related database system.

    > **Tip:** To view the details of a database, for example, its state and the number of existing bindings, select a database in the list and click the link on its name. On the overview of the database, you can perform further actions, for example, delete the database.

3.  Proceed as follows:

    1.  Select the relevant SAP HANA tenant database in the list.

    2.  In the overview that is shown in the lower part of the screen, open the **SAP HANA Web-based Development Workbench** link under **Development Tools**.

    3.  Provide the following details:

        **Enter Username**: **`SYSTEM`**

        **Enter Password**: Enter the password you determined for the SYSTEM user.

        You're now logged on to the SAP HANA cockpit.

    6.  Open the **Security** tool of the SAP HANA Web-based Development Workbench.

    7.  Expand the **Security** node.

    8.  Open the context menu for the **Users node** and choose **New User**.

    9.  On the **User** tab, provide a name for the new user.

        The user name always appears in upper case letters.

    10.  In the **Authentication** section, make sure the **Password** checkbox is selected and enter a password.

        > The password must start with a letter and only contain uppercase and lowercase letters ('a' - 'z', 'A' - 'Z'), and numbers ('0' - '9').

    11. To create the database user, in the menu bar click the  **Save** icon.

        The new database user is displayed as a new node under the **Users** node.

    12. To assign your user the roles with the required permissions for working with the SAP HANA Web-based Development Workbench, go to the **Granted Roles** section and choose **+ (Add)**.

    13. Type **`ide`** in the search field and select all roles in the result list.

    14. Choose **OK**.

        The roles are added on the **Granted Roles** tab.

    15. To assign the `CONTENT_ADMIN` role to the user, repeat the steps in the **Granted Roles** section, searching for `CONTENT_ADMIN`.

        For more information about the `CONTENT_ADMIN` role, see [Predefined Database (Catalog) Roles](https://help.sap.com/viewer/b3ee5778bc2e4a089d3299b82ec762a7/2.0.06/en-US/de421861bb571014846288086be76719.html).

    16. Save your changes.

    17. Before you continue to work with the SAP HANA Web-based Development Workbench, you log out first and log on again with your new database user.

        > **Caution:** At this point, you're still logged on with the SYSTEM user. You can only use your new database user to work with the SAP HANA Web-based Development Workbench by logging out from the SAP HANA cockpit first. Otherwise, you would automatically log in to the SAP HANA Web-based Development Workbench with the SYSTEM user instead of your new database user.

        > Therefore, choose the **Logout** button before you continue to work with the SAP HANA Web-based Development Workbench, where you need to log on again with the new database user.


### Add a repository workspace


**Prerequisites**

-	You've installed the latest SAP JVM version. For more information, see [(Optional) Install SAP JVM](https://help.sap.com/viewer/ea72206b834e4ace9cd834feed6c0e09/Cloud/en-US/76137f42711e1014839a8273b0e91070.html).
-	You've opened a tunnel to your database and make sure that it's open until you complete the tutorial. For more information, see [Open Database Tunnels](https://help.sap.com/viewer/d4790b2de2f4429db6f3dff54e4d7b3a/Cloud/en-US/6930850a8f9a40489c01ed1aa381946d.html).
-	You've installed and set up all the necessary tools. We recommend that you use the latest SAP HANA version. For more information, see [Install SAP HANA Tools for Eclipse](https://help.sap.com/viewer/d4790b2de2f4429db6f3dff54e4d7b3a/Cloud/en-US/b0e351ada628458cb8906f55bcac4755.html).
-	You've connected to the tenant database system via the Eclipse IDE. For more information, see [Connect to SAP HANA Databases via the Eclipse IDE](https://help.sap.com/viewer/d4790b2de2f4429db6f3dff54e4d7b3a/Cloud/en-US/4efc124a0ccc42b3b502ad3a3908d23d.html)

After you add the SAP HANA system hosting the repository that stores your application-development files, you must specify a repository workspace, which is the location in your file system where you save and work on the development files.

1.  In the Eclipse IDE, go to **Window** > **Perspective** >  **Open Perspective** >  **Other**.

2.  Select **SAP HANA Development** and choose **Open**.

3.  In the **Repositories** > view, choose **File** >  **New** > **Repository Workspace**.

    -	SAP HANA system:

      Choose the same system you just added for this tutorial.

      To add your database system to Eclipse, follow the steps in [Connect to SAP HANA Databases via the Eclipse IDE](https://help.sap.com/viewer/d4790b2de2f4429db6f3dff54e4d7b3a/Cloud/en-US/4efc124a0ccc42b3b502ad3a3908d23d.html).

    > **Remember:** Make sure that the tunnel is open until you complete the tutorial.

    -	Workspace Name:

      If a default workspace exists, uncheck the **Default workspace** option and enter a workspace name.

      A folder with the name you type is created below the **Workspace Root**.

    -	Workspace root:

      The **Workspace Root** is a folder that contains the workspace you create in this step. The **Workspace Root** can be anywhere on your local file system.

4.  Choose **Finish**.

### Results

In the **Repositories** view, you see your workspace, which enables you to browse the repository of the system tied to this workspace. The repository packages are displayed as folders.

At the same time, a folder will be added to your file system to hold all your development files.



### Add an XS application project


After you set up a development environment for the chosen SAP HANA system, you can add a project to contain all the development objects you want to create as part of the application-development process.

There are a variety of project types for different types of development objects. Generally, a project type ensures that only the necessary libraries are imported to enable you to work with development objects that are specific to a project type. In this tutorial, you create an XS project.

1.  In the **SAP HANA Development** perspective in the Eclipse IDE, choose **File** > **New > XS Project**.

2.  Make sure the **Share project in SAP repository** option is selected.

3.  Enter a project name.

4.  Choose **Next**.

5.  Select the repository workspace you created in the previous step.

6.  Choose **Finish** without doing any further changes.

### Results

The **Project Explorer** view in the **SAP HANA Development** perspective in Eclipse displays the new project.

> **Tip:** If your XS Project isn't visible in the **Project Explorer**, choose the **`︙`** button in menu bar for the **Project Explorer** view to change the project presentation from **Hierarchical** to **Flat**.

The system information in brackets to the right of the project node name in the **Project Explorer** view indicates that the project has been shared. Shared projects are regularly synchronized with the repository hosted on the SAP HANA system you're connected to.


### Write server-side JavaScript


SAP HANA Extended Application Services (SAP HANA XS) supports server-side application programming in JavaScript. In this step, you add some simple JavaScript code that generates a page which displays the words `Hello, World!`.

1.  In the **Project Explorer** view in the **SAP HANA Development** perspective in Eclipse, right-click your XS project, and choose **New** > **Other**.

    > **Tip:** If your XS Project isn't visible in the **Project Explorer**, choose the **`︙`** button in menu bar for the **Project Explorer** view to change the project presentation from **Hierarchical** to **Flat**.

2.  In the **Select a wizard** dialog, choose **SAP HANA** > **Application Development** > **XS JavaScript File**.

3.  In the **New XS JavaScript File** dialog, enter **`MyFirstSourceFile.xsjs`** as file name.

4.  Choose **Finish**.

5.  In the `MyFirstSourceFile.xsjs` file, enter the following code and save the file:

    `$.response.contentType = "text/html";`

    `$.response.setBody( "Hello, World !");`

    > By default, saving the file automatically commits the saved version of the file to the repository.

    The example code shows how to use the SAP HANA XS JavaScript API's **response** object to write HTML. By typing **`$.`**, you have access to the API's objects.

6.  Check that the application descriptor files (`.xsapp` and `.xsaccess`) are present in the root package of your new XS JavaScript application.

    The application descriptors are mandatory and describe the framework in which an SAP HANA XS classic application runs.

    The `.xsapp` file indicates the root point in the package hierarchy where content is to be served to client requests; the `.xsaccess` file defines who has access to the exposed content and how.

    > By default, the project-creation Wizard creates the application descriptors automatically.  
     If they're not present, you see a 404 error message in the Web Browser when you call the XS JavaScript service. In this case, you need to create the application descriptors manually. For more information, see the [SAP HANA Developer Guide for SAP HANA Studio](https://help.sap.com/viewer/product/SAP_HANA_PLATFORM/2.0.06/en-US?task=discover_task).

7.  Open the context menu for the new file and select **Team** > **Activate**.

  The activate operation publishes your work and creates the corresponding catalog objects. You can now test it.


### Test your application


Check if your application is working and if the `Hello, World!` message is displayed.

1.  In the SAP BTP cockpit, choose **SAP HANA / SAP ASE** > **Databases & Schemas**.

2.  Select the relevant SAP HANA tenant database in the list.

3. Open the **SAP HANA Web-based Development Workbench** link under **Development Tools**.

4. When the web editor opens, copy the first part of the URL up to and including the **"com"**.

5. To form your target URL, add a **/** followed by the project name.

6. Then add another **/** followed by the name of the file we just added in the previous step.

    **Example**

    **URL that you need to adapt:**

    `<https://mydadb01wfddybp1mp.int.sap.eu2.hana.ondemand.com/sap/hana/ide/>`

    **Project name:** hana-one

    **File name:** MyFirstSourceFile.xsjs

    **Target URL:**

    `<https://mydadb01wfddybp1mp.int.sap.eu2.hana.ondemand.com/hana-one/MyFirstSourceFile.xsjs>`

7. Place the target URL in your web browser address bar and press **`ENTER`** to see the response.

### Results

The following text should be displayed:

**`Hello, World !`**


### Retrieve data from SAP HANA


To extract data from the database, you use your JavaScript code to open a connection to the database and then prepare and run an SQL statement. The results are added to the **`Hello, World !`** response. You use the following SQL statement to extract data from the database:

`select * from DUMMY`

The SQL statement returns one row with one field called **DUMMY**, whose value is **X**.

1.  In the **Repositories** view in the **SAP HANA Development** perspective in Eclipse, open the `MyFirstSourceFile.xsjs` file in the embedded JavaScript editor.

2.  In the `MyFirstSourceFile.xsjs` file, replace your existing code with the following code:

    ``` JavaScript

    $.response.contentType = "text/html";
    var output = "Hello, World !";

    var conn = $.db.getConnection();
    var pstmt = conn.prepareStatement( "select * from DUMMY" );
    var rs = pstmt.executeQuery();

    if (!rs.next()) {
      $.response.setBody( "Failed to retrieve data" );
      $.response.status =  $.net.http.INTERNAL_SERVER_ERROR;
    } else {
      output = output + "This is the response from my SQL: " + rs.getString(1);
    }
    rs.close();
    pstmt.close();
    conn.close();

    $.response.setBody(output);
    ```

3. Save the `MyFirstSourceFile.xsjs` file.

4.  Open the context menu of the `MyFirstSourceFile.xsjs` file and choose **Team** > **Activate All**.



### Test your application again


Check if your application is retrieving data from your SAP HANA database.

1.  In the SAP BTP cockpit, choose **SAP HANA / SAP ASE** > **Databases & Schemas**.

2.  Select the relevant SAP HANA tenant database in the list.

3. Open the **SAP HANA Web-based Development Workbench** link under **Development Tools**.

4. When the web editor opens, copy the first part of the URL up to and including the **"com"**.

5. To form your target URL, add a **/** followed by the project name.

6. Then add another **/** followed by the name of the file we just added in the previous step.

    **Example**

    **URL that you need to adapt:**

    `<https://mydadb01wfddybp1mp.int.sap.eu2.hana.ondemand.com/sap/hana/ide/>`

    **Project name:** hana-one

    **File name:** MyFirstSourceFile.xsjs

    **Target URL:**

    `<https://mydadb01wfddybp1mp.int.sap.eu2.hana.ondemand.com/hana-one/MyFirstSourceFile.xsjs>`

7. Place the target URL in your web browser address bar and press **`ENTER`** to see the response.

### Results

The following text should be displayed:

`Hello, World!This is the reponse from my SQL: X`


### Test yourself



---
