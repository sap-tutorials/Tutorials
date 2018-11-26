---
title: Google App Engine Setup
description: Setup Google App Engine
primary_tag: products>sap-hana
tags: [  tutorial>beginner, topic>cloud, topic>odata, products>sap-hana, products>sap-hana\,-express-edition  ]
---

## Prerequisites  
 - **Proficiency:** Beginner


## Next Steps
 - [Google App Engine install HDB](https://www.sap.com/developer/tutorials/gae-nodehdb.html)


## Details
### You will learn  
Setup your first Google App Engine account and application. This tutorial will walk you through the setup of a new project and repository that will become a Node.js application running on Google App Engine.

### Time to Complete
**15 Min**

---

[ACCORDION-BEGIN [Step 1: Create a new Project](Create a new Project)]

Your first step after signing into the [Google Cloud Platform](https://cloud.google.com/nodejs/) and creating your account will be to create a new project.

![create project](1.png)

You can name the project whatever you like, in this tutorial the name `sapdevs-sample-1` was used.

![name project](2.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: Begin Development](Begin Development)]

![created project](3.png)

With the project now created and associated with a billing account you are now able to begin development.

![development](4.png)

In the top left corner select development in the menu.


[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: Repository Menu](Repository Menu)]

![repository](5.png)

From the development menu you will need to create a repository for your code.

![create repository](6.png)

In this tutorial the repository was named to match the project name, `sapdevs-sample-1`

![link repository](7.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: Link Repository](Link Repository)]

Now that the new repository is created it is still empty. The next step is to link the repository, clone or push code to it.

In this particular case, the choice has been to use the "mirror" option and to accomplish that means that a new repository will need to be created in GitHub or Bitbucket first.

![link repository](8.png)

With this repository created, back in the Google Console you will be able to link the two together.

![link account](9.png)

In this case as the example uses GitHub then the GitHub account is what is linked and then a list of available repositories will be listed.

![repository lists](10.png)

Once successfully linked and mirrored then the initially created content will be available.

![linked repository](11.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: Open Cloud Shell](Open Cloud Shell)]

In the top right you will find the option to launch the Cloud Shell.

![cloud shell](12.png)

From the shell you will need to switch to the `src` directory.

![list and change directory](13.png)



[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: Clone your repository](Clone your repository)]

In the `src` directory you will now need to clone your repository.

![copy your repository git link](14.png)

![copy your repository git link](15.png)

`git clone xxxxxxxxxx`

Once it is cloned you will see the code in your directory.

![code](16.png)


[ACCORDION-END]


