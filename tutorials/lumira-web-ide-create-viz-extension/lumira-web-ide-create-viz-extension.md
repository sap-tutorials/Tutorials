---
title: Creating a new viz extension project
description: A step-by-step guide on how to get started with building a visualization extension for SAP Lumira using SAP Web IDE
primary_tag: products>sap-lumira
tags: [  tutorial>beginner, products>sap-lumira, products>sap-cloud-platform, products>sap-web-ide ]
time: 15
---

## Prerequisites  
  - [Sign up for an account on SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html)
  - [Install and Deploy SAP Lumira extensions](https://www.sap.com/developer/tutorials/lumira-extensions-intro.html)

## Next Steps
- Adapt a D3 Chart for Lumira with SAP Web IDE (coming soon)

## Details
### You will learn  
  - How to start creating your own visualization extension for SAP Lumira, using SAP Web IDE and the `VizPacker` plugin

#### What is SAP Web IDE?
SAP Web IDE is the development environment for building various applications, including visualization extensions for SAP Lumira.

#### What is `VizPacker`?
`VizPacker` is a plugin that allows you to visualize your visualization rendering code while still in development. In this way, `VizPacker` provides a real-time visual preview of what you are building and allows you to test your work before packaging it and using it in SAP Lumira and other SAP products.

This tutorial will cover:

- Setting up your development environment in SAP Web IDE and `VizPacker`
- Creating your first visualization extension project
- Walking through the structure and the important components of a visualization extension project
- Testing and packaging a finished extension.


---


[ACCORDION-BEGIN [Step 1: ](set up SAP Cloud Platform account)] ￼

First, you need to set up your development environment in SAP Web IDE. For this, you need to set up your SAP Cloud Platform (HCP) account so that you can use SAP Web IDE. Please refer to the tutorial [Sign up for an account on SAP Cloud Platform](https://www.sap.com/developer/tutorials/hcp-create-trial-account.html) for a step-by-step guide on how to do so.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Open SAP Cloud Platform cockpit)] ￼

After setting up your SAP Cloud Platform account, you should be on this page, which is your SAP Cloud Platform cockpit

![SAP Cloud Platform cockpit](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_2.png)   


[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Click Web IDE link)] ￼

In the left menu panel, go to **Subscriptions**. Under **Subscribed HTML5 Applications** you will see the **`sapwebide`** subscription with the **`webide`** link in the next column. Click on this link.

![SAP Cloud PlatformSubscriptions](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_3.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Get application URL)] ￼

You will be directed to this screen. Click on the **Application URL** to access your SAP Web IDE account and save or bookmark this link. From now on, you will use this link to access your SAP Web IDE account without having to go to the SAP Cloud Platform Cockpit.

![SAP Web IDE URL](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_4.png)

> ### Optional
> Alternatively (in case the SAP Web IDE URL does not work) go to **Services** and scroll down to the **Dev & Ops** section and select **SAP Web IDE**.
> ![SAP Web IDE URL](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_4a.png)
> In case it is not enabled, click on the **Not Enabled** widget.
> ![SAP Web IDE URL enable](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_4b.png)

> Both of the steps above will lead you to the SAP Web IDE Overview (Enable SAP Web IDE if it has not already been enabled). Once you're on this screen and have enabled SAP Web IDE, go to **Open SAP Web IDE**. This will open the link to your SAP Web IDE account. Again, remember to save or bookmark this link for future use.   


[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Go to welcome screen)] ￼

Before proceeding with this step, make sure that you have the link to your SAP Web IDE account, i.e. your application URL that you derived from the previous steps. Once you go to your application URL, you should have this screen, which is your Welcome Screen

![SAP Web IDE application URL](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_5.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Enable VizPacker)] ￼

Next, to enable the `VizPacker`, on the left menu panel, select the bottom icon called **Preferences**, and go to **Plugins** and go to the **`VizPacker plugin(com.sap.webide.vizpacker)`** and enabled it. Then click **Save** at the bottom.

![SAP Web IDE Enable VizPacker](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_6.png)

You will be asked to refresh your browser for the changes to be effective. Select **Refresh**.

![SAP Web IDE Refresh browser](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_6b.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Check for VizPacker)] ￼

You will now be directed to your workspace which you can also access by clicking on the **Development** icon on the left sidebar.
Go to the right sidebar and click on the top most icon that looks like a column chart. If you do not have `VizPacker` enabled, this icon will not be visible.

![SAP Web IDE Development](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_7.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Create a new visualization extension project)] ￼

Now you will create a new visualization extension project. Go to **File -- New -- Project from Template** which you can also do with `Ctrl+Alt+Shift+O`

![SAP Web IDE Lumira extension create new project](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_8.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Select a template)] ￼

You will be asked to select a template. If you do not see the **Visualization Extension** template, click on the drop down box and select **Visualization Extension Project**.

![SAP Web IDE Lumira extension visualization template](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_9.png)

You will now be able to see the **Visualization Extension** template. Select it and click **Next**.

![SAP Web IDE Lumira extension visualization template](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_9b.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 10: ](Name the project)] ￼

Choose a project name. Remember that white space and special characters are not allowed. As an example, call it `MyFirstVizExtension` or any other name you like, and then click **Next**

![SAP Web IDE Lumira extension](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_10.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 11: ](Add project details)] ￼

In this step, you will complete the profile of your visualization extension. Please follow the naming conventions described below since the profile details will appear in Lumira as shown in **Note** below.

- For your **Extension Name**, enter a title for your extension. Let's call it `My First Viz Extension`. There are no syntax rules for this field.

- **Extension ID** needs to be in the format `company_name.viz.ext.extension_name` Let's call it `com.viz.ext.myfirstviz`   
- Enter the **Version** number: **1.0.0**
- **Company Name** should be your name, i.e. the name of the developer.
- You can add a **URL** like a blog post or an online website related to the extension if you like.
- Add a **Description** to show users in what context you have created the extension.
- Make sure that you check **Enable Compatible Mode** and click **Next**

    ![SAP Web IDE Lumira extension profile](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_11.png)

> ### Note
> All of these fields are relevant when you use the extension in Lumira.
> ![SAP Web IDE Lumira extension profile fields](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_11b.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 12: ](Configure the layout of your extension)] ￼

Next you will configure the layout of your extension. Most charts are drawn using `D3.js` (which is in-built in `VizPacker` and SAP Web IDE), which uses SVG to draw chart elements. Using DIV is another option. For this example, we will select **SVG**. It is recommend that you also keep the **Title** and **Legend** checked. If you wish to remove them, you can remove from your project later. However, it is more difficult to add them later to the project if you haven't configured them first. You can also choose where you want to position your title and legend in your chart. Select **Next**.

![SAP Web IDE Lumira extension layout configuration](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_12.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 13: ](Load the dataset)] ￼

Choose the sample dataset provided, `data.csv` and load it on to your project as the reference data that the extension should be built on. Make sure to define your measures and dimensions. In this case, the **City** and **Year** columns are the dimensions, while the rest of the numeric columns are measures. Hit **Next** once you're done.

![SAP Web IDE Lumira extension load data](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_13.png)

> ### Note
> In case you want to change certain columns to a different type, for example, **Year** from being a measure to a dimension, simply click on it. You will see an option to select what type of data column it should be.
> ![SAP Web IDE Lumira extension data](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_13b.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 14: ](Configure measures and dimensions)] ￼

Now you will configure the way your data will work in your extension. Based on your measures and dimensions, you will define the **Measure Set** and **Dimension Set**.

For each set, click on the **+** icon and add your data measures and dimensions respectively.

![SAP Web IDE Lumira extension data measures and dimensions](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_14a.png)

Rename your measure sets from the default **Untitled Measure Set 1** to **Y Axis**. Rename your dimension sets from the default **Untitled Dimension Set 1** to **X Axis**. Click on Next

![SAP Web IDE Lumira extension rename sets](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_14b.png)

On the next screen, click on **Finish**

![SAP Web IDE Lumira extension finish create project](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_14c.png)

> ### Optional
> In the previous screen where you configured your measure and dimension sets, you can alternatively click Finish there as well.


[ACCORDION-END]

[ACCORDION-BEGIN [Step 15: ](Review project components)] ￼

You should now have your visualization extension project created and ready. You will see that your workspace is now busier, with your project folder tree structure on the left pane, your code in the middle section, and your visualization preview on your right pane.

The main components of the project that are of concern are `render.js` and `default.css`

- `render.js` is where all of your visualization rendering code will go. This file can be found under `MyFirstVizExtension --> bundles --> com --> viz --> ext --> myfirstviz --> com_viz_ext_myfirstviz-src --> js --> render.js`
- `default.css` is where all of your SVG and CSS styling goes. This file can be found under `MyFirstVizExtension --> bundles --> com --> viz --> ext --> myfirstviz --> com_viz_ext_myfirstviz-src --> style --> default.css`

    ![SAP Web IDE Lumira extension main project components](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_15.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 16: ](Review render.js)] ￼

The `render.js` file is the main JavaScript file, which comprises of a top-level **define()** function, which contains a **render()** function. All of your visualization implementation code should go here.

![SAP Web IDE Lumira extension render](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_16.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 17: ](Review default.css)] ￼

The `default.css` file contains all the styling code for the SVG elements that you render in `render.js`
> ### Important
> When assigning CSS classes to chart elements, please make sure that they have unique identifiers. When extensions are used in Lumira, they all become part of one single DOM structure and hence, overlapping of class names can take place.
> For example, if there is an extension with an element having the class name `.axis`, which is also present in another extension with the same class name, both those extensions will have visualization errors due to overlap of class names.
> It is recommended to use unique identifiers as prefixes to class names. For example, if your extension is called `MyExtension`, and you want to assign a class to your axis, use `sap_viz_ext_myextension_axis` for the class name.

![SAP Web IDE Lumira extension style](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_17.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 18: ](Display the Data Table)] ￼

In the visualization pane(`VizPacker` quick preview) on the right, click on the icon highlighted below for displaying the Data Table (data that was loaded when the project was created)

![SAP Web IDE Lumira extension data table](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_18.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 19: ](Review data binding)] ￼

Click on the adjacent icon (highlighted below), to display how the Data Binding takes place between the data values with respect to the measures and dimensions and how they are being used in the chart.

![SAP Web IDE Lumira extension data binding](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_19.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 20: ](Test and preview extension)] ￼

Test and preview your extension in the visualization pane. For now, you will only see a legend and nothing else, since you haven't added any visualization rendering code yet (This tutorial does not cover coding. Adding code will be part of subsequent tutorials. For now, we will just have a blank sample with a legend on the right)

![SAP Web IDE Lumira extension preview](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_20.png)

You can also have a full screen preview by clicking on the **Run** button at the top.

![SAP Web IDE Lumira extension test](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_20b.png)

This will "run" your chart in a new browser tab in full screen so that you can test and debug it using your browser developer tools.

![SAP Web IDE Lumira extension debug](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_20c.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 21: ](Package extension for use in Lumira)] ￼

Once you have finished developing and testing your extension, you can now package it for use in Lumira. Click on the **Pack** button

![SAP Web IDE Lumira extension package](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_21.png)

Your extension will be packaged as an installable .zip file which you can install into Lumira. You will see the following pop up. Click **OK** on the pop up

![SAP Web IDE Lumira extension download](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_21b.png)


[ACCORDION-END]

[ACCORDION-BEGIN [Step 22: ](Save the extension)] ￼

Save the extension in your computer. You can then use the extension in Lumira as explained in the tutorial: [Install and Deploy SAP Lumira extensions](https://www.sap.com/developer/tutorials/lumira-extensions-intro.html). Download the CSV file from your Web IDE project by right-clicking on it and selecting **Export**.

![SAP Web IDE Lumira extension install](https://raw.githubusercontent.com/AnnieSuantak/Tutorials/master/tutorials/lumira-web-ide-create-viz-extension/lumira4_22.png)


[ACCORDION-END]

