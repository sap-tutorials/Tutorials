---
title: Use Your Own YaaS Project to Back your Storefront
description: By default, your StoreFront communicates with a default YaaS Project in the backend (created by the StoreFront's own Team). This contains the products that you have seen, banner images and much more.  In this step you will point your StoreFront to your own YaaS Project instead, giving you more control of back end logistics.
tags: [  tutorial>intermediate, products>sap-hybris-as-a-service-on-sap-hana-cloud-platform>sap-hybris-as-a-service-on-sap-hana-cloud-platform ]
---
## Prerequisites  
 - **Proficiency:** Intermediate
 - **Background Knowledge:** If you are not already familiar with [Node.js](https://www.youtube.com/watch?v=pU9Q6oiQNd0), follow this link to learn more. You will use it in this tutorial. The YaaS Storefront is based on  [Angular](https://docs.angularjs.org/guide/directive) and [Restangular](https://github.com/mgonto/restangular#starter-guide). If you are not already familiar with them and would like to dive into the code, you can follow the provided links to start learning.
 - **Tutorials:**
    - [Getting Started with YaaS](http://www.sap.com/developer/tutorials/yaas-getting-started.html)
    - [Download and Run the Default YaaS Storefront](http://www.sap.com/developer/tutorials/yaas-download-run-default-storefront.html)
    - [Use Your Own YaaS Project to Back your YaaS Storefront](http://www.sap.com/developer/tutorials/yaas-create-project-backing-storefront.html)

## Next Steps
 - [Deploy a Micro Service Built on YaaS on the SAP HCP](http://www.sap.com/developer/tutorials/yaas-deploy-run-microservice-cloud.html)

## Details
### You will learn  
In this tutorial you will use your own YaaS Project which you created earlier to serve as the backend of your StoreFront, rather than the default YaaS Project.  You will then have complete control over the contents of your Storefront, allowing you to populate its products, its banners, and many more features.

### Time to Complete
**20 Min**.

---

1. If you have not completed the Getting Started with YaaS tutorial, please do it now. This is necessary in order to follow the next steps.   

2. Open your YaaS Project in the YaaS Builder. Take note of the YaaS **Project's identifier** and the YaaS **Client's identifier** and place these as the `PROJECT_ID` and `CLIENT_ID` values in the `gruntfile.js` that already exists in the top level of your storefront `folder(1)` directory which you created in the previous tutorials. This is the root directory of your `yaas-storefront` project.
    - You Can find **Project's identifier** in the **Builder** under the **Administration** tab when you open your project.

    ![Project Identifier](project-identifier.PNG)

    - You can find the client identifier by selecting your client from the list of **Clients**.

    ![Client Credentials](client-credentials.PNG)

    - In the **Client Authorization** section, use the **SHOW** button. Now you should be able to see the **Client ID**.

    ![Show Credentials](show-credentials.PNG)

3. Your `Gruntfile.js` should now contain lines similar to `PROJECT_ID = 'yb0160913',` and `CLIENT_ID = 'vBji2FMBBvKVYlGn8ZZ2b4E6xFKpHhNi',`. Note the commas and the single quotes around each variable. Rebuild the Storefront by running the `grunt build` command from `folder(1)` directory.

4. Rerun your Storefront by running the `npm start` command from `folder(1)` directory. The client will now be talking with your YaaS Project.

5. Add some products to start populating your Storefront with material.
    - Go back to the builder and click on product / new product.
    - Add a name, an identifier, a price, a description and check active.

    ![Add Product](add-product.PNG)

    - Click on save.
    - (Optional) Add an image, click on save.

    ![Add Image](add-image-to-product.PNG)

    - Follow the same steps if you want to create additional products.
    - In the end if you reload your storefront, you will be able to see the products you added.

    > NOTE: It is important to set your products as **active**, otherwise they will not appear in your online store.

### Summary
In this tutorial, you modified your storefront to talk with your own YaaS Project in the backend rather than the default YaaS Project. This gave you complete control over the content of the Storefront, such as its products and banners, and much more that you have yet to explore.   

## Next Steps
 - [Deploy a Micro Service Built on YaaS on the SAP HCP](http://www.sap.com/developer/tutorials/yaas-deploy-run-microservice-cloud.html)

### Optional
6. Create a category and assign your products to it.
    - Go back to the Builder and open Categories.
    - Create a category and you can add an image for it.
    - Add sub categories to your category.
    - Assign products to categories
