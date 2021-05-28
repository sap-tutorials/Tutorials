---
title: Introduce Resilience to your Application
description: Introduce resilience to your application using the SAP Cloud SDK.
auto_validation: true
tags: [ tutorial>intermediate, products>sap-cloud-sdk, products>sap-s-4hana, products>sap-business-technology-platform, topic>cloud, topic>java  ]
primary_tag: products>sap-cloud-sdk
time: 15
---

## Details
### You will learn
  - Why you should care about resilience
  - How to make the call to the OData service resilient by using resilience decorators
  - How to write Tests for code decorated with the new resilience decorators
  - To deploy the application on SAP Cloud Platform Cloud Foundry
---

[ACCORDION-BEGIN [Step 1: ](Resilience)]

Consider the following situation: you are developing a cloud application to provide a service to your customers. In order to keep your customers happy, you're of course interested in achieving the highest possible availability of your application.

However, cloud applications, possibly spanned across multiple services, are inherently complex. So it is safe to assume that something, somewhere will fail at some point in time. For example a call to your database might fail and cause one part of your application to fail. If other parts of your application rely on the part that has failed, these parts will fail as well. So a single failure might cascade through the whole system and break it. This is especially critical for multi-tenancy applications, where a single instance of your application serves multiple customers. A typical S/4HANA multi-tenancy application involves many downstream services, such as on-premise S/4HANA ERP systems.

Let's look at a concrete example: Suppose you have 30 systems your cloud application is dependent on and each system has a "perfect" availability of 99.99%. This means each service is unavailable for 4.32 minutes each month (43200 min * (1 – 0.9999) = 4.32 min).

Now assume failures are cascading, so one service being unavailable means the whole application becomes unavailable. Given the equation used above, the situation now looks like this:

43200 min * (1 – 0.9999^30) = 43200 min * (1 – 0.997) = 129.6 min

So your multi-tenancy application is unavailable for more than two hours every month for every customer!

In order to avoid such scenarios, we need to equip applications with the ability to deal with failure. If an application can deal with failures, it is called **resilient**. So **resilience** is the means by which we achieve **availability**.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Resilience4j)]

The SAP Cloud SDK now builds upon the `Resilience4j` library in order to provide resilience for your cloud applications. In previous versions 2.x we used the `Hystrix` library, which has been in maintenance mode for some time now.

`Resilience4j` comes with many modules to protect your application from failures. The most important ones are timeouts, bulkheads, and circuit breakers.

- **Timeouts:** `Resilience4j` allows setting custom timeout durations for every remote service. If the response time of a remote service exceeds the specified timeout duration, the remote service call is considered as failure. This value should be adapted according to the mean response time of the remote service.

- **Bulkheads:** These allow for restricting the number of concurrent requests to a remote service. If the number of concurrent incoming requests exceed the configured threshold, the bulkhead is said to be saturated. In this case, further requests are automatically stopped until existing requests are completed.

- **Circuit Breakers:** `Resilience4j` uses the circuit breaker pattern to determine whether a remote service is currently available. Breakers are closed by default. If a remote service call fails too many times, `Resilience4j` will open/trip the breaker. This means that any further calls that should be made to the same remote service are automatically stopped. `Resilience4j` will periodically check if the service is available again, and close the open breaker again accordingly. For more information on the circuit breaker pattern, check [this article by Martin Fowler](https://martinfowler.com/bliki/CircuitBreaker.html).

Additionally, the SAP Cloud SDK enables you to provide fallback functions. So if a call fails, for example because the bulkhead is saturated or the circuit breaker is open/tripped, the SDK will check whether a fallback is implemented and call it automatically. So even if a service is unavailable you can still provide some useful result, e.g. by serving cached data.

If you want to gain a deeper understanding of the inner workings, checkout the [`Resilience4j` User Guide] (https://resilience4j.readme.io).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Make your OData call resilient)]

Now that we have covered why resilience is important, it's finally time to introduce it into your application. In the last tutorial you created a simple servlet that uses the SDK's Virtual Data Model (VDM) and other helpful abstractions to retrieve business partners from an ERP system. In order to make this VDM call resilient, you have to wrap the code using the `ResilienceDecorator` class provided by the SAP Cloud SDK.

At the same time you will also separate the VDM call itself into another class for better readability and easier maintenance in future tutorials. Note that starting with version 3 of the SAP Cloud SDK, a separate class is no longer required to implement resilience. You could have also added resilience directly to the existing `BusinessPartnerServlet` class.

So first create the following class:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/GetBusinessPartnersCommand.java`

```Java
package com.sap.cloud.sdk.tutorial;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.time.Duration;
import java.util.Collections;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceDecorator;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceIsolationMode;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceRuntimeException;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;
import com.sap.cloud.sdk.datamodel.odata.helper.Order;

import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestination;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.AddressEmailAddress;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartnerAddress;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.BusinessPartnerService;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.DefaultBusinessPartnerService;

public class GetBusinessPartnersCommand {
    private static final Logger logger = LoggerFactory.getLogger(GetBusinessPartnersCommand.class);
    // TODO: uncomment the lines below and insert your API key, if you are using the sandbox service
    // private static final String APIKEY_HEADER = "apikey";
    // private static final String SANDBOX_APIKEY = "";
    private static final String CATEGORY_PERSON = "1";
    private final ErpHttpDestination destination;

    private final BusinessPartnerService businessPartnerService;
    private final ResilienceConfiguration myResilienceConfig;

    public GetBusinessPartnersCommand(ErpHttpDestination destination) {
        this(destination, new DefaultBusinessPartnerService());
    }

    public GetBusinessPartnersCommand(ErpHttpDestination destination, BusinessPartnerService service) {
        this.destination = destination;
        businessPartnerService = service;

        myResilienceConfig = ResilienceConfiguration.of(BusinessPartnerService.class)
                .isolationMode(ResilienceIsolationMode.TENANT_AND_USER_OPTIONAL)
                .timeLimiterConfiguration(
                        ResilienceConfiguration.TimeLimiterConfiguration.of()
                                .timeoutDuration(Duration.ofMillis(10000)))
                .bulkheadConfiguration(
                        ResilienceConfiguration.BulkheadConfiguration.of()
                                .maxConcurrentCalls(20));
    }

    public List<BusinessPartner> execute() {
        return ResilienceDecorator.executeSupplier(this::run, myResilienceConfig, e -> {
            logger.warn("Fallback called because of exception.", e);
            return Collections.emptyList();
        });
    }

    private List<BusinessPartner> run() {
        try {
            return businessPartnerService
                    .getAllBusinessPartner()
                    .select(BusinessPartner.BUSINESS_PARTNER,
                            BusinessPartner.LAST_NAME,
                            BusinessPartner.FIRST_NAME,
                            BusinessPartner.IS_MALE,
                            BusinessPartner.IS_FEMALE,
                            BusinessPartner.CREATION_DATE,
                            BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS
                                    .select(BusinessPartnerAddress.CITY_NAME,
                                            BusinessPartnerAddress.COUNTRY,
                                            BusinessPartnerAddress.TO_EMAIL_ADDRESS
                                                    .select(AddressEmailAddress.EMAIL_ADDRESS)
                                    )
                    )
                    .filter(BusinessPartner.BUSINESS_PARTNER_CATEGORY.eq(CATEGORY_PERSON))
                    .orderBy(BusinessPartner.LAST_NAME, Order.ASC)
                    .top(200)
                    // TODO: uncomment the line below, if you are using the sandbox service
                    // .withHeader(APIKEY_HEADER, SANDBOX_APIKEY)
                    .executeRequest(destination);
        } catch (ODataException e) {
            throw new ResilienceRuntimeException(e);
        }
    }
}
```

To use the `ResilienceDecorator` you need at least two things:

1. The code you want to execute in a resilient manner. It can be either a `Supplier`, `Callable`, `Supplier<Future>`, method reference, or a simple lambda function. As you might have noticed already, the example simply takes the VDM-based code that calls the OData service from the previous tutorial, and puts it into a separate run() method. The `ResilienceDecorator` offers methods that simply wrap the provided function and returns a new function (`decorateSupplier`, `decorateCallable`, etc.), plus methods that also start execution the function immediately (`executeSupplier`, `executeCallable`, etc.). Here `executeSupplier` is used with a method reference to the VDM-based code.

2. An instance of `ResilienceConfiguration` with identifier parameter set. Here the example uses the class reference, but a string identifier can also be used. Besides the mandatory identifier parameter, the SAP Cloud SDK comes with a default resilience configuration, so you don't need to perform any other configuration on your own. In most cases the default configuration will suffice. However, if you need to change the resilience configuration, you can find more information on this topic in [SAP Cloud SDK Javadoc](https://help.sap.com/doc/ae45330c443b42c5a54bde85dd70aec9/1.0/en-US/com/sap/cloud/sdk/cloudplatform/resilience/ResilienceConfiguration.html)

Here is an example of a custom resilience configuration. Here the isolation mode is set to optional tenant + user, the bulkhead maximum concurrent calls to 20, and the execution timeout to 10000 milliseconds.

```Java
myResilienceConfig = ResilienceConfiguration.of(BusinessPartnerService.class)
        .isolationMode(ResilienceIsolationMode.TENANT_AND_USER_OPTIONAL)
        .timeLimiterConfiguration(
                ResilienceConfiguration.TimeLimiterConfiguration.of()
                        .timeoutDuration(Duration.ofMillis(10000)))
        .bulkheadConfiguration(
                ResilienceConfiguration.BulkheadConfiguration.of()
                        .maxConcurrentCalls(20));
```

Additionally, the `decorate...` and `execute...` methods of `ResilienceDecorator` support an optional third parameter for a fallback function, in case the remote service call should fail. In this case a lambda function that returns an empty list is used. You could also serve static data or check whether a response to this call has already been cached. Best practice is to at least log the provided `Throwable`.


Update your resilience configuration to match the above configuration. Now that you have a working command, you need to adapt your `BusinessPartnerServlet` to use the newly created command:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/BusinessPartnerServlet.java`

```Java
package com.sap.cloud.sdk.tutorial;

import com.google.gson.Gson;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.connectivity.DestinationAccessor;

import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestination;
import com.sap.cloud.sdk.s4hana.connectivity.DefaultErpHttpDestination;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;

@WebServlet("/businesspartners")
public class BusinessPartnerServlet extends HttpServlet {

    private static final long serialVersionUID = 1L;
    private static final Logger logger = LoggerFactory.getLogger(BusinessPartnerServlet.class);

    private static final String DESTINATION_NAME = "MyErpSystem";

    @Override
    protected void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        try {
            final ErpHttpDestination destination = DestinationAccessor.getDestination(DESTINATION_NAME)
                    .asHttp().decorate(DefaultErpHttpDestination::new);
            final List<BusinessPartner> businessPartners =
                    new GetBusinessPartnersCommand(destination).execute();
            response.setContentType("application/json");
            response.getWriter().write(new Gson().toJson(businessPartners));
        } catch (final Exception e) {
            logger.error(e.getMessage(), e);
            response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            response.getWriter().write(e.getMessage());
            e.printStackTrace(response.getWriter());
        }
    }
}
```

Thanks to your new `GetBusinessPartnersCommand`, you can now simply create a new command and execute it. As before, you'll get a list of business partners as result. But now you can be sure that your application will not stop working all-together if the OData service is temporarily unavailable for any tenant.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 4: ](Write tests for the resilient command)]

There is one thing you need to address in order to properly test your code: you need to provide your tests with an ERP endpoint.

Now let's adapt the code in your integration test to check, if your fallback is working correctly:

 `integration-tests/src/test/java/com/sap/cloud/sdk/tutorial/BusinessPartnerServletTest.java`:

```Java
package com.sap.cloud.sdk.tutorial;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import io.restassured.module.jsv.JsonSchemaValidator;
import io.vavr.control.Try;
import org.hamcrest.Matchers;
import org.jboss.arquillian.container.test.api.Deployment;
import org.jboss.arquillian.junit.Arquillian;
import org.jboss.arquillian.test.api.ArquillianResource;
import org.jboss.shrinkwrap.api.spec.WebArchive;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import java.net.URI;
import java.net.URL;

import com.sap.cloud.sdk.cloudplatform.connectivity.DefaultDestination;
import com.sap.cloud.sdk.cloudplatform.connectivity.Destination;
import com.sap.cloud.sdk.cloudplatform.connectivity.DestinationAccessor;
import com.sap.cloud.sdk.testutil.MockDestination;
import com.sap.cloud.sdk.testutil.MockUtil;

import static io.restassured.RestAssured.when;

@RunWith(Arquillian.class)
public class BusinessPartnerServletTest {
    private static final MockUtil mockUtil = new MockUtil();
    private static final JsonSchemaValidator jsonValidator_List = JsonSchemaValidator
            .matchesJsonSchemaInClasspath("businesspartners-schema.json");

    private static final String DESTINATION_NAME = "MyErpSystem";
    private static final Destination dummyDestination = DefaultDestination.builder().property("name", DESTINATION_NAME).property("URL", "foo").build();

    @ArquillianResource
    private URL baseUrl;

    @Deployment
    public static WebArchive createDeployment() {
        return TestUtil.createDeployment(BusinessPartnerServlet.class);
    }

    @BeforeClass
    public static void beforeClass() {
        mockUtil.mockDefaults();
    }

    @Before
    public void before() {
        RestAssured.baseURI = baseUrl.toExternalForm();
    }

    @Test
    public void testService() {
        // TODO: insert your service URL down below
        mockUtil.mockDestination(MockDestination.builder(DESTINATION_NAME, URI.create("https://URL")).build());
        // HTTP GET response OK, JSON header and valid schema
        when()
                .get("/businesspartners")
                .then()
                .statusCode(200)
                .contentType(ContentType.JSON)
                .body(jsonValidator_List);
    }

    @Test
    public void testWithFallback() {
        // Simulate a failed VDM call with non-existent destination
        DestinationAccessor.setLoader((n, o) -> Try.success(dummyDestination));

        // Assure an empty list is returned as fallback
        when()
                .get("/businesspartners")
                .then()
                .statusCode(200)
                .contentType(ContentType.JSON)
                .body("", Matchers.hasSize(0));
    }
}
```

Make sure to replace the URL in line 58 with the one of your service (e.g. `http://localhost:3000` for a locally deployed mock server), or otherwise the test will fail.

>If you are using a service other than the SAP Business Hub sandbox service or the mock server (see steps 1 and 10 of the [previous tutorial](s4sdk-odata-service-cloud-foundry)), i.e., you stored your system information and your credentials in the `systems.yml` and `credentials.yml` files, change your test code like this:
```Java
@Test
public void testService() {
    mockUtil.mockDestination(DESTINATION_NAME, "ERP_001");
    // HTTP GET response OK, JSON header and valid schema
    when()
            .get("/businesspartners")
            .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body(jsonValidator_List);
}
```

With `testWithFallback()` you added a test to test your resilience. The example intentionally provides a destination (localhost) that does not provide the called OData service in order to make the command fail. Since you implemented a fallback for your command that returns an empty list, you can assert that the response actually contains an empty list.

Simply run  `mvn clean install` as in the previous tutorials to test and build your application.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Deploy application to Cloud Foundry)]

Simply run `mvn clean install` as in the previous tutorials to test and build your application and then run `cf push` to deploy your updated application to Cloud Foundry!

This wraps up the tutorial on making our sample application resilient using `Resilience4j` and the SAP Cloud SDK. Continue with the next tutorial [Step 6: Caching](https://blogs.sap.com/2017/07/12/step-6-with-sap-s4hana-cloud-sdk-caching/) and also explore other tutorial posts on topics like [security](https://blogs.sap.com/2017/07/18/step-7-with-sap-s4hana-cloud-sdk-secure-your-application-on-sap-cloud-platform-cloudfoundry/)!

[DONE]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_2]

[ACCORDION-END]
