---
title: Adding a Custom Login Screen to your HXE system
description: See how quickly and easily you can customize your HXE Server Only image and add a custom login screen.
primary_tag: products>sap-hana\,-express-edition
tags: [  tutorial>how-to, tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition  ]
time: 10
---
## Prerequisites  
 - **Systems used:** SAP HANA, express edition


## Details
&nbsp;
> **DEPRECATED:** SAP HANA XS Classic is deprecated as of SPS02. Please use XS Advanced, and learn about how to get started with the new mission [Get Started with XS Advanced Development](https://www.sap.com/developer/missions/xsa-get-started.html).

&nbsp;


The following steps will show you how to customize the login screen of your individual instance. Why might you want to do that? Well for starters if you have multiple systems this would help you distinguish between them or perhaps you simply want to make your system unique and your own.


---

1. Either using the web based tools, like this tutorial will show you or using the SAP HANA Studio you will need to create a new application. The following steps will show the web based and assumes you will be able to use your skills with the SAP HANA Studio if you choose to use that. First step will be to open the IDE.

	![IDE](2.png)
	![IDE](3.png)

2. Now you will right click on the top level `content` and choose a new application. In this example the application will be called `codejam.mylocation`

	![Right click](4.png)
	![new app](5.png)

3. Now create a new package under the new application called `images`

	![new package](6.png)

4. Next if you click on the package `images` you will get a screen that allows for multiple file uploads.

	![uploads](7.png)

5. Here you will upload your login screen image or images. I have prepared 2 images both are 1920 wide but the height varies. Both are also `JPG` format.

	![new files](8.png)

6. Now open the `.xsaccess` and modify the `authentification` line and change it to `null`

	![file changes](9.png)

	```
	{
    "exposed": true,
    "authentication": null,
    "mime_mapping": [{
        "extension": "jpg",
        "mimetype": "image/jpeg"
    }],
    "force_ssl": false,
    "enable_etags": true,
    "prevent_xsrf": true,
    "anonymous_connection": null,
    "cors": [{
        "enabled": false
    }],
    "cache_control": "no-cache, no-store",
    "default_file": "index.html"
	}
	```

7. The next step will be to jump over to the `Catalog` editor and open an SQL console window. Execute the following SQL statement.

	![statement](10.png)

	```
	ALTER SYSTEM ALTER CONFIGURATION ('xsengine.ini', 'SYSTEM') SET ('httpserver', 'login_screen_background_image') = '/codejam/mysystem/images/back.jpg';
	```

	![statement](11.png)

8. Now Open the `Security` editor and create a new user called `_USS` and assign and save that user the role `sap.hana.xs.selfService.user.roles::USSExecutor`

	![role](12.png)

9. Now that the configuration has been set, content has been uploaded and the user has been created you will need to open a new site - change your URL to match `http://server:8000/sap/hana/xs/admin/#/package/sap.hana.xs.selfService.user/sqlcc/selfService` and then activate it (bottom right)

	![activate](13.png)
	![activate](14.png)

10. Now when you go to log in to your system you should notice a difference right off.

	![login](15.png)

11. If you modify the configuration again you will notice we have something specifically for today, Halloween.

	![halloween](16.png)
