---
title: Download and Run the Default YaaS Storefront
description: The YaaS Storefront is a fully customizable, feature-rich, default shopping web site, from which you can create your very own online shop.  YaaS looks after all the tricky details: security, authorization, payment and cart workflows, etc., so that you can focus on more interesting topics, like personalization and overall coolness of your online shop.  In this tutorial, you will download, run and explore the default YaaS Storefront.
tags: [ tutorial>intermediate, products>sap-hybris-as-a-service-on-sap-hana-cloud-platform>sap-hybris-as-a-service-on-sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Background Knowledge:** The YaaS Storefront is based on [Node.js](https://www.youtube.com/watch?v=pU9Q6oiQNd0),  [Angular](https://docs.angularjs.org/guide/directive) and [Restangular](https://github.com/mgonto/restangular#starter-guide). If you are not already familiar with these, and would like to dive into the code, you can follow the provided links to start learning.
 - **Software Requirements:** Git and Node, which we will be downloading in this tutorial.
 - **Tutorials**
    - [Getting Started with YaaS](http://go.sap.com/developer/tutorials/yaas-getting-started.html)

## Next Steps
 - [Extend the YaaS Storefront's Functionality](http://go.sap.com/developer/tutorials/yaas-extend-storefront-functionality-webservice.html)
 - [Use Your Own YaaS Project to Back your YaaS Storefront](http://go.sap.com/developer/tutorials/yaas-create-project-backing-storefront.html)

## Details
### You will learn  
In this tutorial you will learn how to create your own e-commerce shop in minutes with the YaaS Storefront.

### Time to Complete
**10 Min**.

---

1. Confirm that you have **Node.js** and **Git** installed:
    - run the command `node -v` to see if you have **Node.js** installed.  If not, download it from <https://nodejs.org/en/download/>, and confirm that the command `node -v` runs okay.
    - run the command `git`to see if you have **Git** installed.  If not, download it from <https://git-scm.com/book/en/v2/Getting-Started-Installing-Git>, and confirm that the command `git` runs okay.

2. Clone the YaaS StoreFront:
    - Create a folder which we will refer to as `(1)` in the following steps, and `cd` into it.
    - Run `git clone https://github.com/SAP/yaas-storefront.git` to clone the StoreFront into `(1)`
    - Confirm you now see the folder `yaas-storefront`	inside `(1)`, and `cd` into it.

3. Use Node's package manager to download the dependencies:
    - The YaaS StoreFront includes `npm`, **Node.js**'s package manager, for downloading dependent packages.
    - Run `npm install` to download all the packages the Storefront depends on.  The console should show `npm` downloading many dependencies; confirm this is the case.

4. Start a local web server
    - Run the command `npm start` to start a local web server.  If your command prompt looks like the following image, you will know that the server is running.

    ![Server Running](local-web-server-running.PNG)

5. Open your own default Storefront
    - Open a browser (we like Chrome) and go to `http://localhost:9000` where you should see your own StoreFront, ready for your pimping skills.
    - Verify you can see and browse products in the Storefront.
    ![Verify Browse](verify-browse-products.PNG)

### Summary
You now have seen a default YaaS Storefront running locally on your machine. The next step is to customize and personalize it.  In the next tutorial you will add a feature to the StoreFront called "Mr Tip".  When browsing products, the undecided purchaser can click on "Mr Tip" to get advice, with helpful responses such as "Oh My God, like totally yes!!" and "One word: No".

## Next Steps
 - [Extend the YaaS Storefront's Functionality](http://go.sap.com/developer/tutorials/yaas-extend-storefront-functionality-webservice.html)
 - [Use Your Own YaaS Project to Back your YaaS Storefront](http://go.sap.com/developer/tutorials/yaas-create-project-backing-storefront.html)
