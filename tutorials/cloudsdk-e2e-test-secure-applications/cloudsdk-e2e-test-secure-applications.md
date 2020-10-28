---
author_name: Daniel Kurzynski
author_profile: https://github.com/daniel-kurzynski
title: End to End Test for Secure Applications
description: Learn how to write end to end tests for secured applications.
auto_validation: true
time: 5
tags: [ tutorial>intermediate, products>sap-s-4hana-cloud-sdk]
primary_tag: products>sap-s-4hana-cloud-sdk
---

## Prerequisites
  - [Set up CI/CD](cloudsdk-ci-cd)
  - [End to end test](cloudsdk-e2e-test)
  - [Secure your application](s4sdk-secure-cloudfoundry)


## Details
### You will learn
- How to set up e2e test stage in CI/CD
- How to do add authentication in e2e test
- How to run e2e test

---

[ACCORDION-BEGIN [Step 1: ](Add authentication to E2E tests)]

The basic idea is that the first part of the test is to log in into the application as a user. You created an application which is secured as explained in [Secure your application](s4sdk-secure-cloudfoundry). The first page you see accessing the application is the app-router showing a login form. You adapt the tests in a way that they first visit the login page, enter the credentials and then press the login button.

First, you create the following page object in `e2e-tests/page_objects/login.js`. This page object reads the credentials from the configuration and enters them into the input fields. The selectors of these fields are specified in the elements section. These selectors are working for the standard app-router. For other forms, e.g. on Cloud Platform Neo, or customized forms these selectors need to be updated.

```
"use strict";

const loginCommands = {
  loginWithForm: function () {
    const user = this.api.globals.user;
    const pass = this.api.globals.pass;
    delete this.api.globals.user;
    delete this.api.globals.pass;
    this.waitForElementVisible("@usernameField")
      .setValue("@usernameField", user);
    this.waitForElementVisible("@passwordField")
      .setValue("@passwordField", pass);
    this.waitForElementVisible("@submitButton").click("@submitButton");

    this.expect.element("@usernameField").to.not.be.present;
    return this;
  },
};

module.exports = {
  url: function () {
    return this.api.launchUrl;
  },
  elements: {
    usernameField: "input[name=username]",
    passwordField: "input[name=password]",
    submitButton: "input[type=submit]"
  },
  commands: [loginCommands]
};
```

As a next step, you have to trigger the login before the test execution. This can be configured in the file `e2e-tests/cucumber.conf.js`, which you created in [e2e-test](cloudsdk-e2e-test). The updated content looks as follows:

```
BeforeAll(async () => {
  const options = {
    configFile: __dirname + "/nightwatch.conf.js",
    env: argv.NIGHTWATCH_ENV || 'firefox'
  }
  await startWebDriver(options);
  await createSession(options);
  const loginPage = client.page.login();
  await loginPage.navigate().loginWithForm(false);
});

AfterAll(async () => {
  await closeSession();
  await stopWebDriver();

  setTimeout(() => {
    reporter.generate({
      theme: 'bootstrap',
      jsonFile: reportsDirectory + "/cucumber_report.json",
      output: reportsDirectory + "/cucumber_report.html",
      reportSuiteAsScenarios: true,
      launchReport: false
    });
  }, 0);
});
```
>There are two new lines in the `BeforeAll` block to log into the application.

Furthermore, you have to specify the `username` and `password`. In our case they come from environment variables.
The updated file `e2e-tests/nightwatch.conf.js` from [e2e-test](cloudsdk-e2e-test) looks as follows:

```
const chromedriver = require('chromedriver');
const geckodriver = require('geckodriver');
const argv = require("yargs").argv;

module.exports = {
  output_folder: 's4hana_pipeline/reports/e2e',
  page_objects_path: __dirname + '/page_objects',
  silent: !process.env.NIGHTWATCH_VERBOSE,
  test_settings: {
    default: {
      launch_url : argv.launchUrl,
      webdriver: {
        start_process: true,
        port: 4444
      },
      globals: {
        abortOnAssertionFailure : true,
        retryAssertionTimeout: 10000,
        waitForConditionTimeout: 10000,
        asyncHookTimeout : 10000,
        user: "${e2e_username}",
        pass: "${e2e_password}"
      },
      screenshots: {
        enabled: true,
        path: 's4hana_pipeline/reports/e2e/screenshots'
      }
    },
    chromeHeadless: {
      webdriver: {
        server_path: chromedriver.path,
        cli_args: ['--port=4444']
      },
      desiredCapabilities: {
        browserName: 'chrome',
        javascriptEnabled: true,
        acceptSslCerts: true,
        chromeOptions: {
          args: ['headless', 'window-size=1280,800', 'disable-gpu', 'no-sandbox']
        }
      }
    },
    chrome: {
      webdriver: {
        server_path: chromedriver.path,
        cli_args: ['--port=4444']
      },
      desiredCapabilities: {
        browserName: 'chrome',
        javascriptEnabled: true,
        acceptSslCerts: true,
        chromeOptions: {
          args: ['window-size=1280,900', 'disable-gpu', 'no-sandbox']
        }
      }
    },
    firefox: {
      webdriver: {
        server_path: geckodriver.path,
        cli_args: ['--port', '4444', '--log', 'debug']
      },
      desiredCapabilities: {
        browserName: 'firefox',
        javascriptEnabled: true,
        acceptSslCerts: true,
        marionette: true
      }
    }
  }
};
```

>In global, you define user and pass. `${e2e_username}` means that the value is taken from the environment variable `e2e_username`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Run E2E test locally)]

To run the tests locally you can use the same command as used in [End to End Test](cloudsdk-e2e-test). However, in SAP Cloud Platform Cloud Foundry the application URL `launchUrl` should point to the `app-router`. In the Neo environment, it should point to the `application` because the user is redirected to the login form automatically.

Furthermore, the `username` and `password` environment variables have to be set. On Windows you can use the command set to do that. The final command looks as follows:

```Shell
set e2e_username=myUser
set e2e_password=myPassword
npm install
npm run ci-e2e -- --launchUrl=https://path/to/your/running/app-router
```

>Your password is stored in the history of your terminal. We recommend using a technical user for the E2E tests.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Run E2E tests in the pipeline)]

!![S4SDK Pipeline](e2epipeline.png)

To run the tests in our pipeline you have to adapt the `.pipeline/config.yml`. In [Set up CI/CD](cloudsdk-ci-cd) you learned that this file configures the behavior of the pipeline. To execute the E2E test you have to add a section called `endToEndTests` representing a stage in the section `stages`. The final configuration is shown below.

```
#Project Setup
general:
  productiveBranch: 'master'
  projectName: 'business-partner-manager'

#Stage Specific Configurations
stages:  
  endToEndTests:
    appUrls:
      - url: 'https://approuter-USERNAME.cfapps.eu10.hana.ondemand.com'
        credentialId: e2e-test-user-cf

    cfTargets:
      - space: 'MySpaceName'
        manifest: 'manifest-test.yml'
        org: 'MyOrg'
        appName: 'firstapp'
        credentialsId: 'deployment-cf'
```

The section `endToEndTests` consists of two sections. Before you can run the E2E tests, you first have to deploy our application to the SAP Cloud Platform. As for the productive deployment, you define a section called `cfTargets` or `neoTargets` to configure the deployment. Afterwards in `appUrls` you define a list of URL and credential ids specifying the launch URL for the tests and the credentials used as username and password environment variables.

The tests are executed once per entry in the list. For each entry the URL is passed as `launchUrl` to the test. The `credentialsId` is used to read the corresponding credentials from the credentials store in Jenkins. Thus, you have to create these credentials, as explained in [Set up CI/CD](cloudsdk-ci-cd). The `username` and `password` is read from the credentials store and passed as environment variable to the test.

You can also run the E2E tests after the productive deployment. These tests can be called smoke tests to check that your application is running after the deployment. It is usually a subset of the E2E tests, but can also be the full test suite.
>For the smoke test the command `npm ci-smoke` is used instead of `npm ci-e2e`. Thus, you have to define this command in the file `package.json`. To run the same tests you can just copy the configuration from the command `ci-e2e` in the section scripts as shown below. To run a subset of the tests you can use [tags](https://github.com/cucumber/cucumber-js/blob/master/docs/cli.md#tags).

```Shell
"scripts": {
    "ci-e2e": "cucumber-js e2e-tests/features --require e2e-tests/cucumber.conf.js --require e2e-tests/steps --format json:s4hana_pipeline/reports/e2e/cucumber_report.json",
    "ci-smoke": "cucumber-js e2e-tests/features --require e2e-tests/cucumber.conf.js --require e2e-tests/steps --format json:s4hana_pipeline/reports/e2e/cucumber_report.json"
  },
```

The configuration for the stage `productiveDeployment` in the file `.pipeline/config.yml` has the same structure as the stage `endToEndTests`:

```
#Project Setup
general:
  productiveBranch: 'master'
  projectName: 'business-partner-manager'

#Stage Specific Configurations
stages:  
  productionDeployment:
    appUrls:
      - url: 'https://approuter-USERNAME.cfapps.eu10.hana.ondemand.com'
        credentialId: e2e-test-user-cf

    cfTargets:
      - space: 'MySpaceNameProductive'
        manifest: 'manifest.yml'
        org: 'MyOrg'
        appName: 'firstapp'
        credentialsId: 'deployment-cf'
```

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Questions and troubleshooting)]

Are you facing a development question? Then check out Stack Overflow for SAP Cloud SDK related questions. If you do not find an answer, feel free to post your question and make sure to attach the tag `s4sdk`. Our team, as well as the whole Stack Overflow community, are at your service and will quickly react to your question.

For an overview of SAP Cloud SDK related questions, go to <https://stackoverflow.com/questions/tagged/sap-cloud-sdk>.

You think that you found a bug in one of our Continuous Delivery artifacts? Feel free to open an issue in our Pipeline GitHub repository on <https://github.com/SAP/cloud-s4-sdk-pipeline/issues>.


[DONE]
[ACCORDION-END]



---
