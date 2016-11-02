---
title: SAP HANA Studio, Setup a new project
description: Setup a new project using the SAP HANA Studio
tags: [  tutorial>beginner, products>sap-hana, products>sap-hana,-express-edition, products>sap-hana-studio ]
---
## Prerequisites  
 - **Proficiency:** Beginner
 - **Tutorials:** [How to download and install the HANA Eclipse plugin](http://go.sap.com/developer/how-tos/hxe-howto-eclipse.html)

## Next Steps
 - [SAP HANA Studio, Deploy your project](http://go.sap.com/developer/tutorials/studio-deploy-project.html)

## Details
### You will learn  
Setup your first project with SAP HANA Studio for the SAP HANA, express edition.

### Time to Complete
**10 Min**.

---

1. From the previous tutorial you should now have your SAP HANA Studio connected to your SAP HANA, express edition. Therefore, the next step is to create a link to the SAP HANA repository.
	
	![connect repo](0.png)
	
2. Now that you have the repository linked you can create your first project. This initial project will be the basis for a series of tutorials.

	![new project](1.png)

3. The first step to create your new repository will be select "File" then "New" and from the drop down you will select a "Project" under the "General" section.

	![project name](2.png)

4. The project name will be `mylocation` as the idea behind this project will be a simple SAP HANA application around geographical location.

	![project](3.png)

5. Once the project is created you will need to create your initial `.xsapp`file to signify that it is an XS application followed by an `.xsaccess` file to define settings for the application.

	![new files](4.png)

6. The content of your `.xsaccess` file is quite simple.

	```
	{
		"exposed": true
	}
	```

## Next Steps
- [SAP HANA Studio, Deploy your project](http://go.sap.com/developer/tutorials/studio-deploy-project.html)
