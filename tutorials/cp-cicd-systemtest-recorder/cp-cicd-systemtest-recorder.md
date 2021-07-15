---
title: Add Automated System Tests with the SAPUI5 Test Recorder to Your CI/CD Pipeline
description: Use the SAPUI5 Test Recorder to create system tests with UIVeri5 against the user interface of an SAPUI5/SAP Fiori application. Add the tests to a continuous integration and delivery pipeline to run them automatically.
auto_validation: true
time: 60
tags: [ tutorial>beginner, topic>cloud, topic>sapui5, products>sap-fiori]
primary_tag: products>sap-business-technology-platform
author_name: Sarah Lendle
author_profile: https://github.com/SarahLendle
---

## Prerequisites
- You use [SAPUI5](https://sapui5.hana.ondemand.com/#/topic/2535ef9272064cb6bd6b44e5402d531d) in version 1.74 or higher.
-	You have installed [Node JS](https://nodejs.org/en/) in version 8.0 or higher.
-	You have installed [Visual Studio Code](https://code.visualstudio.com/).
-	You have installed UIVeri5 using the following command:
    ```
    npm install @ui5/uiveri5 -g
    ```
-	You have installed [Yeoman](https://yeoman.io/) and [generator-easy-ui5](https://github.com/SAP/generator-easy-ui5) using the following command:
    ```
    npm install -g yo generator-easy-ui5
    ```
- Your Google Chrome version is up to date. See [Update Google Chrome](https://support.google.com/chrome/answer/95414?co=GENIE.Platform%3DDesktop&hl=en).
-	You have a [Jenkins](https://jenkins.io/) instance that is preconfigured for using project "Piper". See [Configuration](https://sap.github.io/jenkins-library/configuration/).
-	You have an account and a repository in [GitHub](https://github.com/).

## Details
### You will learn
- How to create system tests with UIVeri5 using the UI5 Test Recorder
- How to create a CI/CD pipeline with project "Piper"
- How to add system tests as automated steps to your CI/CD pipeline

### What Is This Tutorial About?

In this tutorial, you'll create and run automated system tests with UIVeri5 against a simple shopping app for electronic devices. Your test application has basic functions such as a product catalog sorted by categories, a search option, and an *add to cart* function.

The tutorial consists of three main stages:

![Main Stages of Tutorial](tutorial-steps.png)

1. Set up your project and manually go through your test scenario before starting to code it.

2. Create and run system tests with UIVeri5 to check the home screen of your application, its product search, and the navigation to a product.

3. Automate your system tests by integrating them into a CI/CD pipeline.

### About System Tests with UIVeri5

UIVeri5 is an SAP open-source JavaScript testing framework for SAPUI5 applications. It drives a real browser for your deployed app and simulates authentic user scenarios. System tests check both front-end and back-end and make sure that all pieces of an application work well together.

The following graphic shows the positioning of system tests with UIVeri5 compared to other testing methods and tools. The arrow shape illustrates the granularity of the methods: Compared to unit, component, or integration tests, system tests examine less details and focus on crucial workflows, instead.

![Test Comparison](test-comparison.png)

>For more information about testing with UIVeri5, have a look at these blogs:

>- [UIVeri5: More Stable System Tests for UI5 Applications](https://blogs.sap.com/2019/01/28/uiveri5-more-stable-system-tests-for-ui5-applications/)
>- [UIVeri5 for E2E testing of UI5 apps](https://blogs.sap.com/2019/01/29/uiveri5-for-e2e-testing-of-ui5-apps/)

### About the SAPUI5 Test Recorder

The SAPUI5 Test Recorder is a tool that helps you create integration and system tests. You can use it in any SAPUI5 application to inspect its user interface, view the control properties, and get code snippets for OPA5 and UIVeri5 tests. As of version 1.74, it is part of the SAPUI5 framework.

> For more information about the SAPUI5 Test Recorder, see [Test Recorder](https://sapui5.hana.ondemand.com/#/topic/2535ef9272064cb6bd6b44e5402d531d).

### About the Yeoman Generator for UIVeri5

Yeoman is a popular open-source command-line tool to create a scaffolding for either a complete project or parts of it. As of version 2.3.0., the UIVeri5 Yeoman generator is a sub-generator of the project `generator-easy-ui5`. It helps you kickstart your test suite and quickly write new tests with your own action and assertion logic for scenarios that are relevant for your use case.

>For more information, see [Yeoman generators for OPA5 and UIVeri5](https://blogs.sap.com/2020/11/30/yeoman-generators-for-opa5-and-uiveri5/).

### About CI/CD with Project "Piper"

Project "Piper" is one of SAP's solutions for continuous integration and delivery. It provides pre-configured Jenkins pipelines, which you can use in your own Jenkins master infrastructure and adapt according your needs. Project "Piper" consists of two different parts:

- A [shared library](https://sap.github.io/jenkins-library/), which contains the description of steps, scenarios, and utilities that are required to use Jenkins pipelines
- A [set of Docker images](https://github.com/SAP/devops-docker-images) that can be used to implement best practice processes

> For more information about SAP solutions for CI/CD, see:

> - [Continuous Integration and Delivery by SAP](https://help.sap.com/viewer/product/CICD_OVERVIEW/Cloud/en-US?task=discover_task)
> - [SAP Solutions for Continuous Integration and Delivery](https://help.sap.com/viewer/8cacec64ed854b2a88e9a0973e0f97a2/Cloud/en-US/e9fa320181124fa9808d4446a1bf69dd.html)
---

[ACCORDION-BEGIN [Step 1: ](Set up your test project)]

In Visual Studio Code, set up your UIVeri5 test project.

1. In Visual Studio Code, open a new terminal.

2. Navigate to the folder to which you want to add your UIVeri5 project.

3. Use the following command to call the generator for UIVeri5 tests:

    ```Shell/Bash
    yo easy-ui5 project uiveri5
    ```

    Now, you're asked a couple of questions that help the generator create your test structure.

4. Answer the questions as follows:

    | Question | Answer |
    | ----------- | ----------- |
    | Please enter the name of your project | `uiveri5` |
    | URL to the app under test | `https://sapui5.hana.ondemand.com/test-resources/sap/m/demokit/cart/webapp/index.html`|
    | Choose authentication | `none` |
    | Choose additional reporters | `JUNIT` |
    | Do you want to add a page object | `Y` |
    | Do you want to add a spec | `Y` |
    | Page object name | `Home` |
    | Add action with name | Press **Enter** to skip |
    | Add assertion with name | Press **Enter** to skip |
    | Name for the suite (describe block) | `teched` |
    | Name for the spec (it block) | `Should validate the home screen` |

5. Press **Enter**. Now, your project structure is generated.

    ![Questions for generating the project structure](yeoman-uiveri5.png)

6. In Visual Studio Code, open the folder `uiveri5`. As a result, the project `uiveri5` is loaded into the **EXPLORER** pane and you can see its resources in the outline:

    ![Resources in the EXPLORER pane](explorer-pane-yeoman.png)

    Like all system tests with UIVeri5, you'll define your tests through different files:

    - (a) The `conf.js` file:

        In this file, you can define, for example, the browser and reporter that are used, the base URL, and the credentials for your login dialog.

    -	(b) The `spec.js` file (in this case, it's called `teched.spec.js`):

        In this file, you'll define your test scenario, which comprises steps that are triggered one after the other. Within the test scenario, you'll refer to your page objects.

    -	(c) Page objects (in this case, you have `home.js` for the home screen and `product.js` for the detail view of a product):

        Page objects are design patterns that represent a significant part of an app, for example, a view. They group two kinds of elements:

           - Actions, for example, entering a specific text in a search field and triggering the search
           - Assertions, for example, getting a search result that matches the previously entered text

        Page objects use locators to identify specific elements on the screen. Thereby, they allow test runners to see and do anything a real user would. Page objects reside in the `pages` folder of your project.

        >If you want to set up a project for an application that is protected by credentials, you need to add authentication configurations. See [Authentication](https://github.com/SAP/ui5-uiveri5/blob/master/docs/config/authentication.md).

7. To check if the test execution works, choose **Terminal** **&rarr;** **New Terminal** in the root folder of your project, and enter the following command:

    ```Shell/Bash
    uiVeri5
    ```

    As a result, the browser briefly opens to execute the tests. However, as you haven't defined any tests, yet, the application doesn't load.

8. In the terminal response, check if the test execution has been successful:

    ![Terminal Response: Successful](status-passed.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Walk through the test scenario)]

Manually, familiarize yourself with your test scenario before starting to code it. Later, you'll automate the following steps so that they are automatically executed during your system tests.

1. In Google Chrome, use the following URL to access the home screen of your shopping application:

    ```
    https://sapui5.hana.ondemand.com/test-resources/sap/m/demokit/cart/webapp/index.html
    ```

2.    Check how many product categories are shown in the **Product Catalog**:

      !![Product Categories in the Product Catalog](shopping-app.png)

3. In the **Product Catalog**, search for **`Watch`** and check if the displayed results match your request:

    !![Comparison between Search Term and Search Result](search-results.png)

4. Choose **Flat Watch HD32** and check if the product appears in the detailed view:

    !![Detail View of Flat Watch HD32](detailed-view.png)

5. Check if at the bottom right of the detailed view, there is an **Add to Cart** button:

    !![Add to Cart Button](cart-button.png)

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Test the home screen)]

Implement a test that checks if in the product catalog on the home screen of your application, all categories are shown. To verify this, the expected number of product categories are compared with the displayed ones.

1. From the **EXPLORER** pane in Visual Studio Code, open `teched.spec.js`.

    In this file, you'll define the steps of your test scenario and within them, refer to your page objects.

2. Implement the `it` function by adding the skeleton of the home screen test:

    ```
    it("Should validate the home screen", function () {

    });
    ```

    Your code should now look as follows:

    ![Skeleton of Home Screen Test](homescreen-skeleton.png)

3. To specify what you want to check, add the following line to the test skeleton:

    ```
    Then.onTheHomePage.iShouldSeeAllCategories();
    ```

    `iShouldSeeAllCategories` is a reference to the test function that you'll define in the following.

    Now, your first test scenario is complete. Make sure that it looks as follows and choose **File** **&rarr;** **Save**.

    ![First Scenario of Home Screen Test](homescreen-test.png)

4. From the **EXPLORER** pane, open `pages` **&rarr;** `home.js`.

    This file represents the page object for your home screen. In page objects, you can define actions that are performed during a test and make assertions. In this specific case, however, you won't implement an action that is executed during the test but only check the home screen. Therefore, leave the `actions` function empty and only make assertions:

    ![Empty Actions Section in Home Screen Test](actions-empty.png)



5. In the Shopping Cart application in Google Chrome, press **CTRL** + **SHIFT** + **ALT** + **T** (if you use a Windows system) or **SHIFT** + **CTRL** + **OPTION** + **T** (if you use a Mac system) to open the Test Recorder in a new browser window.

6. In your sample application, right-click the first entry of the Categories list and choose **Highlight**. As a result, the Test Recorder highlights the entry to indicate its activity:

    !![Highlighting of First Category](highlight-category.png)

    Now, the Test Recorder provides a code snippet for your test:

    > Please make sure that the dialect **UIVeri5** is selected.

    !![Code Snippet for List Item](snippet-category.png)

7. Copy this code snippet into the assertions section of your `home.js` page object.

8. To identify all elements, remove the `bindingPath` and `interaction`:

    ![Remove Binding Path and Interaction](remove-bindingpath-interaction.png)


9. Add a suitable name for your assertion and the following code snippets:

    ![Add Name and Additional Code Snippets for iShouldSeeAllCategories](ishouldseeallcategories.png)


    Now, the page object for your home screen is ready. Make sure that your code looks as follows and choose **File** **&rarr;** **Save**.

    ![Page Object for Home Screen Test](homejs-categories.png)

10. To run the test, execute the following command in the terminal:

    ```
    uiveri5
    ```

    As a result, the browser opens and the home screen is loaded.

11. In the terminal response, check if the test has been passed successfully:

    ![Status: Passed](status-passed.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Test the product search)]

Implement a test that checks if when you search for a product, the search results are displayed accordingly. To verify this, the entered search term is compared with the search result.

1. From the **EXPLORER** pane in Visual Studio Code, open `teched.spec.js`.

2. Into the `describe` function, add the skeleton of the product search test:

    ```
    it("Should search for a product", function () {

    });
    ```

    Your code should now look as follows:

    ![Skeleton of Product Search Test](product-search1.png)

3. Within the test skeleton, add the following interaction, which will be defined later:

    ```
    When.onTheHomePage.iSearchForProduct();
    ```

4. Then, add the expected behavior, namely that the product list is filtered:

    ```
    Then.onTheHomePage.theProductListShouldBeFiltered();
    ```

    Now, your test is complete. Make sure that it looks as follows and choose **File** **&rarr;** **Save**.

    ![Product Search Test](product-search2.png)

5. From the **EXPLORER** pane, open `pages` **&rarr;** `home.js`.

6. In the Shopping Cart application in Google Chrome, press **CTRL** + **SHIFT** + **ALT** + **T** (if you use a Windows system) or **SHIFT** + **CTRL** + **OPTION** + **T** (if you use a Mac system) to open the Test Recorder in a new browser window.

7. In your sample application, right-click the search field and choose **Enter Text**. As a result, the Test Recorder highlights the search field to indicate its activity:

    !![Highlighted Search Field](highlight-search-field.png)

    Now, the Test Recorder provides a code snippet for your test:

    > Please make sure that the dialect UIVeri5 is selected.

    !![Code Snippet for Search Field](snippet-search-field.png)

8. Copy this code snippet into the `actions` section of your `home.js` page object.

9. Add a suitable name for your assertion and define that during your test, the test runner should search for `Watch`:

    ![Name and Additional Snippets for iSearchForProduct](isearchforproduct.png)

    Next, make an assertion about the title of the product that is displayed first in the filtered catalog.

10. After your first assertion, add a comma.

11. In the search field of your sample application, enter `Watch`.

12. Right-click the first product in the filtered catalog and choose **Highlight**. As a result, the Test Recorder highlights the list entry to indicate its activity:

    !![Highlighted List Entry](highlight-product.png)

    Now, the Test Recorder provides a code snippet for your test:

    > Please make sure that the dialect UIVeri5 is selected.

    !![Code Snippet for List Entry](snippet-product.png)

13. Copy this code snippet into the `assertions` section of your `home.js`.

14. Remove the `bindingPath` and `interaction`:

    ![Remove Binding Path and Interaction](remove-snippets-product.png)

15. Add a suitable name for your assertion and the following code snippets:

    ![Page Object for List Entry Test](theproductlistshouldbefiltered.png)

    Now, the page object for your home screen is ready. Make sure that your code looks as follows and choose **File** **&rarr;** **Save**.

    ![Added Code Snippets](homejs-productlist.png)

16. To run the test, execute the following command in the terminal:

    ```Shell/Bash
    uiveri5
    ```

    As a result, the browser opens and you can watch the automated test software execute the actions you have defined.

17. In the terminal response, check if the test has been passed successfully:

    ![Status: Passed](status-passed.png)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test the navigation to a product)]

Check if the following two statements are true:

-	When you select a product from the filtered catalog, it is shown in the detailed view. To verify this, compare the selected product title with the title in the detailed view.
-	In the detailed view, there is an **Add to cart** button.

1. From the **EXPLORER** pane in Visual Studio Code, open `teched.spec.js`.

2. Into the `describe` function, add the skeleton of the product search test:

    ```
    it("Should navigate to the product", function () {

      });
    ```

    Your code should now look as follows:

    ![Skeleton of Product Search Test](product-navigation.png)

3. Within the test skeleton, add the following interaction with reference to your `home.js`:

    ```
    When.onTheHomePage.iSelectTheFirstProduct();
    ```

4. Then, add the first behavior you expect, namely that the product title appears in the detail view:

    ```
    Then.onTheProductPage.theProductTitleIsShown();
    ```

5. Next, add your second expectation, which is that the **Add to cart** button appears:

    ```
    Then.onTheProductPage.theProductCouldBeOrdered();
    ```

    Now, your test is complete. Make sure that it looks as follows and choose **File** **&rarr;** **Save**.

    ![Product Search Test](product-navigation-test.png)

6. From the **EXPLORER** pane, open `pages` **&rarr;** `home.js`.

7. Add a comma after the close bracket of the first action.

8. In the Shopping Cart application in Google Chrome, press **CTRL** + **SHIFT** + **ALT** + **T** (if you use a Windows system) or **SHIFT** + **CTRL** + **OPTION** + **T** (if you use a Mac system) to open the SAPUI5 Test Recorder in a new browser window.

9. In your sample application, right-click the title of the first search result and choose **Press**. As a result, the Test Recorder highlights the title to indicate its activity:

    !![Highlighted Title](highlight-title.png)

    Now, the Test Recorder provides a code snippet for your test:

    > Please make sure that the dialect UIVeri5 is selected.

    !![Code Snippet for Title](snippet-title.png)

10. Copy this code snippet into the `actions` section of your `home.js`.

11. Adapt the name of the action. With this action, you define that during your test, the test runner clicks on the first search result:

    ![Name for Product Selection Test](iselectthefirstproduct.png)

    Now, the page object for your home screen is ready. Make sure that it looks as follows and choose **File** **&rarr;** **Save**.

    ![Page Object for Home Screenshot](homejs-navigation.png)

    Next, add an assertion that is related to the product details.

    As you don't have a page object for the detailed view, yet, use the yeoman sub-generator to generate its scaffolding. The generator `yo easy-ui5:newuiveri5po` creates a new page object and adds it to the existing specs.

12. In the terminal in Visual Studio Code, make sure that you're in the `uiveri5` root folder.

13. Enter the following command:

    ```Shell/Bash
    yo easy-ui5 project newuiveri5po
    ```

14. Answer the questions that help the generator create your new page object as follows:

    | Question | Answer |
    | ----------- | ----------- |
    | Page object name | `Product` |
    | Add action with name | Press **Enter** to skip |
    | Add assertion with name | Press **Enter** to skip |

    Now, your page object is created.

    ![Page object creation by generator](sub-generatorPO.png)

13. From the Explorer pane, open `pages` **&rarr;** `product.js`.

    This file represents the page object for the detail view of your selected product.

14. In the Shopping Cart application in Google Chrome, right-click the header of the detail view and choose **Highlight**. As a result, the Test Recorder highlights the header to indicate its activity:

    !![Highlighted Header](highlight-header.png)

    Now, the Test Recorder provides a code snippet for your test:

    >Please make sure that the dialect UIVeri5 is selected.

    !![Code Snippet for Header](snippet-header.png)

15. Copy this code snippet into the `assertions` section of your `product.js`.

16. Add a suitable name for your action and the following code snippets:

    ![Added Name and Code Snippets](theproducttitleisshown.png)

    Now, your page object should look as follows:

    ![Page Object for Product Navigation](productjs.png)

    To verify if there is an **Add to Cart** button in the detail view of your selected product, add another assertion.

17. Add a comma after your last assertion.

18. In the Shopping Cart application in Google Chrome, right-click the **Add to Cart** button and choose **Highlight**. As a result, the Test Recorder highlights the button to indicate its activity:

    !![Highlighted Button](highlight-button.png)

    Now, the Test Recorder provides a code snippet for your test:

    > Please make sure that the dialect UIVeri5 is selected.

    !![Code Snippet for Button](snippet-button.png)

19. Copy this code snippet into the `assertions` section of your `product.js`.

20. Add a suitable name for your assertion and add the following code snippet to define that you expect the **Add to Cart** button to be displayed:

    ![Added Name and Code Snippets](theproductcouldbeordered.png)

    Now, the page object for the detail view of your selected product is ready. Make sure that it looks as follows and choose **File** **&rarr;** **Save**.

    ![Page Object with Button Test](productjs-finished.png)

21. To run the test, execute the following command in the terminal:

    ```
    uiveri5
    ```

22. In the terminal response, check if the test has been passed successfully:

    ![Status: Passed](status-passed.png)

[VALIDATE_6]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Integrate your system tests into a CI/CD pipeline)]

Automate your system tests by integrating them into a CI/CD pipeline.

1. Execute a Git commit and push the content of your local `TECHED2019-UIVERI5` project into your GitHub repository.

2. In Google Chrome, open and sign in to your Jenkins instance.

3. To create a new Jenkins pipeline, choose **New Item**.

4. As item name, enter `UI-Testing` (or any other name you like), select **Pipeline**, and choose **OK**.

5. Switch to the **Pipeline** tab.

    For your CI/CD pipeline, you'll use the library of project "Piper". It contains all steps you need to automate the tests you have implemented.

6. In the **Script** area, add the skeleton of your pipeline with reference to the project "Piper" library:

    ```
    #!/usr/bin/env groovy
    @Library(['piper-lib-os']) _

    //Setup the skeleton for Jenkins based Runs
    node {
        stage('System Tests') {

        }
    }
    ```

    Make sure that your script looks as follows and choose **Save**:

    !![Screenshot Pipeline Skeleton in Jenkins](pipeline-script.png)

7. To run your newly created pipeline, choose **Build Now**.

    As a result, your build is scheduled.

8. In the **Build History**, choose **#1** to check the results of your first pipeline run.

9. To check the log for failures due to syntax issues, choose **Console Output**. Make sure that the (empty) pipeline has run successfully:

    !![Console Output in Jenkins: Successful](console-output.png)

    Next, implement a pipeline stage for your system tests with UIVeri5.

10. From the sidebar, choose **Back to Project**.

11. Choose **Configure** and switch to the **Pipeline** tab.

12. Add the following content to your `System Tests` stage:

    ```
    deleteDir()

    // Clone code from the system test repository
    git '<YOUR GITHUB REPOSITORY>'

    // checkout the master branch
    sh 'git checkout master'

    // With this next step UIVeri5 tests can be executed.
    uiVeri5ExecuteTests script: this

    // HTML Publisher plugin
    // Publish HTML reports
    // Publish Test Report for UIveri5 on Jenkins  
    publishHTML target: [
        allowMissing: true,
        alwaysLinkToLastBuild: true,
        keepAll: true,
        reportDir: 'target/report/screenshots/',
        reportFiles: "report.html",
        reportName: "UIVeri5 Test Report"
    ]
    ```

13.  In the script, exchange `<YOUR GITHUB REPOSITORY>` with the URL of the GitHub repository into which you have pushed your local `TECHED2019-UIVERI5` project.

    Make sure that your script looks as follows and choose **Save**:

    ![Pipeline Script Part 1](script-1.png)

    ![Pipeline Script Part 2](script-2.png)

14. To run your pipeline, choose **Build Now**.

    As a result, your build is scheduled.

    >The pipeline run might take a few minutes.

15. Choose **#2** to view the results of your second pipeline run. Make sure that it has run successfully:

    !![Console Output in Jenkins: Successful](console-output.png)

16. From the sidebar, choose **UIVeri5 Test Report**.

    Here, you get an overview of all test results together with screenshots from their execution:

    !![UIVeri5 Test Report](test-report-2.png)

[DONE]

**Congratulations!**

 You have successfully created different system tests with UIVeri5 and integrated them into a continuous integration and delivery pipeline.

[ACCORDION-END]

---
