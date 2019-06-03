---
title: Create Tests for Hystrix-based Command
description: Introduce resilience to your application using the SAP Cloud SDK.
auto_validation: true
tags: [ tutorial>intermediate, products>sap-s-4hana-cloud-sdk, products>sap-s-4hana, products>sap-cloud-platform, topic>cloud, topic>java  ]
primary_tag: products>sap-s-4hana-cloud-sdk
time: 15
---

## Details
### You will learn
  - Why you should care about resilience
  - How to make the call to the OData service resilient by using `Hystrix-based` commands
  - How to write Tests for the new `Hystrix-based` command
  - To deploy the application on SAP Cloud Platform Cloud Foundry



---

[ACCORDION-BEGIN [Step 1: ](Resilience)]

Consider the following situation: you are developing a cloud application to provide a service to your customers. In order to keep your customers happy, you're of course interested in achieving the highest possible availability of your application.

However, cloud applications, possibly spanned across multiple services, are inherently complex. So we can assume that something, somewhere will fail at some point in time. For example a call to your database might fail and cause one part of your application to fail. If other parts of your application rely on the part that has failed, these parts will fail as well. So a single failure might cascade through the whole system and break it. This is especially critical for multi-tenancy applications, where a single instance of your application serves multiple customers. A typical S/4HANA multi-tenancy application involves many downstream services, such as on-premise S/4HANA ERP systems.

Let's look at a concrete example: Suppose you have 30 systems your cloud application is dependent on and each system has a "perfect" availability of 99.99%. This means each service is unavailable for 4.32 minutes each month (43200 min * (1 – 0.9999) = 4.32 min).

Now assume failures are cascading, so one service being unavailable means the whole application becomes unavailable. Given the equation used above, the situation now looks like this:

43200 min * (1 – 0.9999^30) = 43200 min * (1 – 0.997) = 129.6 min

So your multi-tenancy application is unavailable for more than two hours every month for every customer!

In order to avoid such scenarios, we need to equip applications with the ability to deal with failure. If an application can deal with failures, we call it **resilient**. So **resilience** is the means by which we achieve **availability**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Hystrix)]

The SAP Cloud SDK builds upon the `Hystrix` library in order to provide resilience for your cloud applications. Or, to put into the words of the creators of `Hystrix`: "`Hystrix` is a latency and fault tolerance library designed to isolate points of access to remote systems, services and 3rd party libraries, stop cascading failure and enable resilience in complex distributed systems where failure is inevitable."

`Hystrix` comes with many interlocking mechanisms to protect your application from failures. The most important are timeouts, thread-pools and circuit-breakers.

- **Timeouts:** `Hystrix` allows setting custom timeout durations for every remote service. If the response time of a remote service exceeds the specified timeout duration, the remote service call is considered as failure. This value should be adapted according to the mean response time of the remote service.

- **Thread-pools:** By default, every command has a separate thread-pool from which it can requests threads to execute the remote service call in. This has multiple benefits: every command is isolated from your application, so whatever happens in these threads will not affect the performance of your application. Also, the usage of threads allows `Hystrix` to perform remote service calls asynchronously and concurrently. These threads are non-container-managed, so regardless of how many threads are used by your `Hystrix` commands, they do not interfere with your runtime container.

- **Circuit breaker:** `Hystrix` uses the circuit breaker pattern to determine whether a remote service is currently available. Breakers are closed by default. If a remote service call fails too many times, `Hystrix` will open/trip the breaker. This means that any further calls that should be made to the same remote service, are automatically stopped. `Hystrix` will periodically check if the service is available again, and close the open breaker again accordingly. For more information on the circuit breaker pattern, check [this article by Martin Fowler](https://martinfowler.com/bliki/CircuitBreaker.html).

Additionally, `Hystrix` enables you to simply provide a fallback solution. So if a call fails, for example because the thread-pool is depleted or the circuit breaker is open/tripped, `Hystrix` will check whether a fallback is implemented and call it automatically. So even if a service is unavailable we can still provide some useful result, e.g. by serving cached data.

If you want to gain a deeper understanding of the inner workings, checkout the [`Hystrix` Wiki] (https://martinfowler.com/bliki/CircuitBreaker.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Make your OData call resilient)]

Now that we have covered why resilience is important, it's finally time to introduce it into our application. In the last tutorial we created a simple servlet that uses the SDK's Virtual Data Model (VDM) and other helpful abstractions to retrieve business partners from an `Erp` system. In order to make this call resilient, we have to wrap it in an `Erp` Command. So first we will create the following class:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/GetBusinessPartnersCommand.java`

```
package com.sap.cloud.sdk.tutorial;

import org.slf4j.Logger;

import java.util.Collections;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;

import com.sap.cloud.sdk.s4hana.connectivity.ErpCommand;
import com.sap.cloud.sdk.s4hana.datamodel.odata.helper.Order;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.DefaultBusinessPartnerService;

public class GetBusinessPartnersCommand extends ErpCommand<List<BusinessPartner>> {
    private static final Logger logger = CloudLoggerFactory.getLogger(GetBusinessPartnersCommand.class);

    private static final String CATEGORY_PERSON = "1";

    protected GetBusinessPartnersCommand() {
        super(GetBusinessPartnersCommand.class);
    }

    @Override
    protected List<BusinessPartner> run()
            throws Exception {
        final List<BusinessPartner> businessPartners =
                new DefaultBusinessPartnerService()
                        .getAllBusinessPartner()
                        .select(BusinessPartner.BUSINESS_PARTNER,
                                BusinessPartner.LAST_NAME,
                                BusinessPartner.FIRST_NAME,
                                BusinessPartner.IS_MALE,
                                BusinessPartner.IS_FEMALE,
                                BusinessPartner.CREATION_DATE)
                        .filter(BusinessPartner.BUSINESS_PARTNER_CATEGORY.eq(CATEGORY_PERSON))
                        .orderBy(BusinessPartner.LAST_NAME, Order.ASC)
                        .top(10)
                        .execute();

        return businessPartners;
    }

    @Override
    protected List<BusinessPartner> getFallback() {
        logger.warn("Fallback called because of exception:", getExecutionException());
        return Collections.emptyList();
    }
    }
```
The `GetBusinessPartnersCommand` class inherits from `ErpCommand`, which is the SDK's abstraction to provide easy-to-use `Hystrix` commands. To implement a valid `ErpCommand` we need to do two things: first, we need to provide a constructor. Here we simply add the default constructor. Second, we need to override the run() method. As you might have noticed already, we can simply use the `VDM-based` code we used to call our OData service from the previous blog post, and put it into the run() method. No changes needed!

Additionally, by overriding the `getFallback` () method, we can provide a fallback if the remote service call should fail. In this case we simply return an empty list after logging the exception that led to the fallback being called. We could also serve static data or check whether we have already cached a response to this call.

`Hystrix` comes with a default configuration, so you don't need to perform any configuration on your own. In most cases the default configuration will suffice. However, if you need to change the configuration, you can find more information on this topic in [`Hystrix` wiki] (https://github.com/Netflix/Hystrix/wiki/Configuration).

In the following example we set the thread-pool size to 20 and the timeout to 10000 milliseconds.

```
protected GetBusinessPartnersCommand() {
    super(HystrixUtil
            .getDefaultErpCommandSetter(
                    GetBusinessPartnersCommand.class,
                    HystrixUtil.getDefaultErpCommandProperties().withExecutionTimeoutInMilliseconds(10000))
            .andThreadPoolPropertiesDefaults(HystrixThreadPoolProperties.Setter().withCoreSize(20)));
}
```
Now that we have a working command, we need to adapt our `BusinessPartnerServlet`:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/BusinessPartnerServlet.java`

```java
package com.sap.cloud.sdk.tutorial;

import com.google.gson.Gson;
import org.slf4j.Logger;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;

import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;

@WebServlet("/businesspartners")
public class BusinessPartnerServlet extends HttpServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = CloudLoggerFactory.getLogger(BusinessPartnerServlet.class);

    @Override
    protected void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        try {
            final List<BusinessPartner> businessPartners =
                    new GetBusinessPartnersCommand().execute();

            response.setContentType("application/json");
            response.getWriter().write(new Gson().toJson(businessPartners));

        } catch (final Exception e) {
            logger.error(e.getMessage(), e);
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().write(e.getMessage());
        }
    }
}
```
Thanks to our new `GetBusinessPartnersCommand`, we can now simply create a new command and execute it. As before, we get a list of business partners as result. But now we can be sure that our application will not stop working all-together if the OData service is temporarily unavailable for any tenant.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Write tests for the Hystrix command)]
There are two things we need to address in order to properly test our code: we need to provide our tests with an ERP endpoint and a `Hystrix` request context.


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Credentials for SAP S/4HANA system)]
We provide the ERP destination as explained in the [previous blog post] (https://blogs.sap.com/2017/05/21/step-4-with-sap-s4hana-cloud-sdk-calling-an-odata-service/) as a `systems.json` or `systems.yml` file. In addition, you may provide a `credentials.yml` file to reuse your SAP S/4HANA login configuration as described in the Appendix of the previous tutorial step.

To provide credentials in this manner, create the following `credentials.yml` file in a safe location (e.g., like storing your ssh keys in ~/.ssh), i.e., not in the source code repository.

`/secure/local/path/credentials.yml`
```
---
credentials:
- alias: "ERP_TEST_SYSTEM"
  username: "user"
  password: "pass"
```
Afterwards you can pass the `credentials.yml` when running tests. Make sure to pass the absolute path to the file:

```
mvn test -Dtest.credentials=/secure/local/path/credentials.yml
```

### `Hystrix` request context

Since `Hystrix` commands run in non-container-managed threads, they cannot use `ThreadLocal` to access request specific information. Therefore `Hystrix` provides a solution in the form of request contexts. Usually these are created by a servlet filter whenever a request is being made. Since there are no requests in our tests, we need a different way to provide a `Hystrix` request context.

The `MockUtil` class of the SAP Cloud SDK, introduced in the previous blog post, is our friend once again. Using `MockUtil's` `requestContextExecutor`() method we can wrap the execution of the `GetBusinessPartnersCommand` in a request context.

Now let's have a look at the code, to be placed in a file integration-tests/`src`/test/java/com/sap/cloud/`sdk`/tutorial/`GetBusinessPartnersCommandTest.java`:

```

package com.sap.cloud.sdk.tutorial;

import org.junit.Before;
import org.junit.Test;

import java.net.URI;
import java.util.Collections;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.servlet.Executable;
import com.sap.cloud.sdk.testutil.MockUtil;

import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;

import static org.assertj.core.api.Assertions.assertThat;

public class GetBusinessPartnersCommandTest {

    private MockUtil mockUtil;

    @Before
    public void beforeClass() {
        mockUtil = new MockUtil();
        mockUtil.mockDefaults();
    }

    private List<BusinessPartner> getBusinessPartners() {
        return new GetBusinessPartnersCommand().execute();
    }

    @Test
    public void testWithSuccess() throws Exception {
        mockUtil.mockErpDestination();
        new RequestContextExecutor().execute(new Executable() {
            @Override
            public void execute() throws Exception {
                assertThat(getBusinessPartners()).isNotEmpty();
            }
        });
    }

    @Test
    public void testWithFallback() throws Exception {
        mockUtil.mockDestination("ErpQueryEndpoint", new URI("http://localhost"));
        new RequestContextExecutor().execute(new Executable() {
            @Override
            public void execute() throws Exception {
                assertThat(getBusinessPartners()).isEqualTo(Collections.emptyList());
            }
        });
    }

}
```
We use JUnit's @Before annotation to setup a fresh `MockUtil` for each test. Then in the tests (@Test) we do the following: first we create a new request context using `mockUtil.requestContextExecutor` and provide it with a new Executable. Then we override the Executable's execute() method, where we finally put the code that we actually want to test together with the corresponding assertions.

For `testWithSuccess`(), we use the default ERP destination information using `mockUtil.mockErpDestination` so that the endpoint points to the SAP S/4HANA system configured in your `systems.json` file. For the sake of simplicity we simply assert that the response is not empty.

For `testWithFallback`(), we intentionally provide a destination (localhost) that does not provide the called OData service in order to make the command fail. Since we implemented a fallback for our command that returns an empty list, we assert that we actually receive an empty list as response.

Simply run  `mvn clean install` as in the previous tutorials to test and build your application. Consider the following before deploying to Cloud Foundry.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Deploy application to Cloud Foundry)]
The SDK integrates the circuit breakers and other resilience features of `Hystrix` with the SAP Cloud Platform, specifically with the tenant and user information. For example, circuit breakers are maintained per tenant to ensure that they are properly isolated. As of now, our application is not multitenant as far as user authentication is concerned – we will come to this in the next steps of the tutorial series (see [Step 7 for Cloud Foundry](https://blogs.sap.com/2017/07/18/step-7-with-sap-s4hana-cloud-sdk-secure-your-application-on-sap-cloud-platform-cloudfoundry/) and [Step 8 on Neo](https://blogs.sap.com/2017/07/18/step-8-with-sap-s4hana-cloud-sdk-secure-your-application-on-sap-cloud-platform-neo/)).

When running such a non-multitenant application with caching, the required tenant and user information needs to be supplied separately, or be mocked. The (local) Neo environment  handles this out-of-the-box and you do not need to do anything when developing on Neo. On Cloud Foundry, the SDK provides a workaround for testing purposes, but you need to enable this workaround explicitly for security considerations. To do so, follow the step outlined in the following, but be aware of the security implications explained below.

Supply your Cloud Foundry application with an additional environment variable as follows:


```
cf set-env firstapp ALLOW_MOCKED_AUTH_HEADER true
```
When the variable ```ALLOW_MOCKED_AUTH_HEADER``` is explicitly set to true, the SDK will fall back to providing mock tenant and user information when no actual tenant information is available. This setting must never be enabled in productive environments. It is only meant to make testing easier if you do not yet implement the authentication mechanisms of Cloud Foundry. Delete the environment variable as soon as it is no longer required, for example, because you implemented Step 7 of this tutorial series (```cf unset-env firstapp ALLOW_MOCKED_AUTH_HEADER```).

Afterwards, simply run ```mvn clean install``` as in the previous tutorials to test and build your application and then run ```cf push``` to deploy your updated application to `CloudFoundry`!

If you want to run your application locally with `mvn` `tomee`: run (in folder application/), you need to similarly set the environment variable ```ALLOW_MOCKED_AUTH_HEADER=true``` on your local machine before starting the local server, in addition to supplying the destinations (as described in [Step 4](https://blogs.sap.com/2017/05/21/step-4-with-sap-s4hana-cloud-sdk-calling-an-odata-service/)).



This wraps up the tutorial on making our sample application resilient using `Hystrix` and the SAP Cloud SDK. Continue with the next tutorial [Step 6: Caching](https://blogs.sap.com/2017/07/12/step-6-with-sap-s4hana-cloud-sdk-caching/) and also explore other tutorial posts on topics like [security](https://blogs.sap.com/2017/07/18/step-7-with-sap-s4hana-cloud-sdk-secure-your-application-on-sap-cloud-platform-cloudfoundry/)!

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_2]

[ACCORDION-END]
