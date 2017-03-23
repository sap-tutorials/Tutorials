---
title: SAP HANA Studio, Setup a new project
description: Setup a new project using the SAP HANA Studio
primary_tag: products>sap-hana
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana\,-express-edition, products>sap-hana-studio ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [How to download and install the HANA Eclipse plugin](http://www.sap.com/developer/how-tos/2016/09/hxe-howto-eclipse.html)

## Next Steps
 - [SAP HANA Studio, Deploy your project](http://www.sap.com/developer/tutorials/studio-deploy-project.html)

## Details
### You will learn  
Setup your first project with SAP HANA Studio for the SAP HANA, express edition.

### Time to Complete
**10 Min**.

---

1. From the previous tutorial you should now have your SAP HANA Studio connected to your SAP HANA, express edition. Therefore, the next step is to create a link to the SAP HANA repository. 

	![connect repo](0.png)

    Be sure to switch your perspective to the `SAP HANA DEVELOPMENT` one. To make the next steps work properly.

	![connect repo](1a.png) 

2. Now that you have the repository linked you can create your first project. This initial project will be the basis for a series of tutorials.

	![new project](1.png)

3. The first step to create your new repository will be select "File" then "New" and from the drop down you will select a "Project" under the "General" section.

	![project name](2.png)

4. The project name will be `mylocation` as the idea behind this project will be a simple SAP HANA application around geographical location.

	![project](3.png)

5. Once the project is created you will need to create your initial `.xsapp`file to signify that it is an XS application followed by an `.xsaccess` file to define settings for the application. You can use either the wizard to select the `XS Application Descriptor File` and `XS Application Access File` 

	![new files](7.png)
	
	or simply the "File" - "New" option and manually type the name `.xsapp` and `.xsacess` being sure to incude the `.` first.
	
	![new files](4.png)

6. The content of your `.xsaccess` file is quite simple.

	```
	{
		"exposed": true
	}
	```
	
	The content of the `.xsapp` can actually be empty.

## Next Steps
- [SAP HANA Studio, Deploy your project](http://www.sap.com/developer/tutorials/studio-deploy-project.html)
