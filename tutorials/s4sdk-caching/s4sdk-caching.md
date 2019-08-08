---
title: Introduce caching to your application
description: Introduce caching to your application using the SAP Cloud SDK.
auto_validation: true
primary_tag: products>sap-s-4hana-cloud-sdk
tags: [ tutorial>intermediate, products>sap-s-4hana-cloud-sdk, products>sap-cloud-sdk, products>sap-cloud-platform, topic>cloud, topic>java ]
time: 15
---


## Details
### You will learn
  - What caching is and why you should care about it
  - How a cache works
  - Uses of a caching command
  - How to cache your OData Call
  - How to do the configuration and make `parameterized` calls


---

[ACCORDION-BEGIN [Step 1: ](Caches)]

Sometimes service calls from your application to external servers turn out to be quite expensive in terms of performance and latency. Further evaluation of the queried data can make things even worse once critical response times are reached for the clients and customers.

To improve responsiveness to the users, the data requested internally by your application can often be stored for subsequent calls. This can be achieved in such a way, that for every request the information previously stored by the application can be re-used. This general behavior is called a cache. A cache stores copies of information passing through it. Besides serving improved responsiveness, the technical goal is to reduce the overall required bandwidth and processing requirements, thus eventually lessening the server load and perceived lag. This way the amount of information, that needs to be transmitted across networks, can be reduced.

Caches are very important in a wide variety of use cases. It is one of the reasons for the advancements of our modern internet experience, like on-demand multimedia streaming and persistent cloud storage. Unlike web caches, which save whole request and response documents, an internal application cache serves the purpose of persisting interim data for multiple intended uses. Whenever information is expensive to compute or retrieve, and its value on a certain input is needed more than once, a cache should be considered.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](How does it work?)]
A cache generally works by the action of requesting information to a given subject, called a key. If an information to a given key was previously requested, since stored and now available to read, a so called "cache hit" occurs: the data can be found and will be loaded. A "cache miss" occurs when it cannot.

The most important aspects of caches are their size and their item life time. Both should be limited with regards to the use case, to avoid an outdated state or disproportionate memory consumption in the application. The biggest effect of using a cache can be perceived, when the application is repetitively reading larger chunks of data from external sources. Then the fraction of bandwidth required for transmitting information will be reduced effectively.

Caching is applicable whenever:

- You are willing to spend some memory to improve speed.
- You expect that keys will sometimes get queried more than once.
- Your cache will not need to store more data than what would fit in RAM. (By default, the cache is local to a single run of your application. It does not store data in files, or on outside servers.)


If each of these options apply to your use case, then we highly recommend the caching features provided by SAP Cloud SDK.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Caching Command with SAP Cloud SDK)]

The command cache allows parallel execution and asynchronous computation for efficient programming practices. Stored information is organized as a local key-value store. For each unique key the same response is expected in case of a "cache hit". Otherwise the cache response will be computed.

As underlying technology `JCache` (`JSR 107`) is being used. In this tutorial we will use a `JCache` adapter [Caffeine] (https://github.com/ben-manes/caffeine), which lets us specify the cache duration. 

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Cache your OData Call)]

Now that we have covered why caching is important and how it can help us improve performance and responsiveness, it's finally time to introduce it into our application. In the last tutorial we created a simple `ErpCommand` that uses the SDK's OData abstractions to retrieve business partners from an ERP system while guaranteeing resilience provided by `Hystrix`. In order to make this call cacheable, we have to start with a new type extending `CachingErpCommand`. So first we will create the following class:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/GetCachedBusinessPartnersCommand.java`

```
package com.sap.cloud.sdk.tutorial;

import org.slf4j.Logger;

import java.time.Duration;
import java.util.Collections;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;

import com.sap.cloud.sdk.cloudplatform.connectivity.DestinationAccessor;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceDecorator;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceIsolationMode;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceRuntimeException;

import com.sap.cloud.sdk.odatav2.connectivity.ODataException;

import com.sap.cloud.sdk.s4hana.connectivity.DefaultErpHttpDestination;
import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestination;

import com.sap.cloud.sdk.datamodel.odata.helper.Order;

import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.BusinessPartnerService;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.DefaultBusinessPartnerService;

public class GetCachedBusinessPartnersCommand {
    private static final Logger logger = CloudLoggerFactory.getLogger(GetCachedBusinessPartnersByCategoryCommand.class);
    
    private final ErpHttpDestination destination;
    
    private final BusinessPartnerService businessPartnerService = new DefaultBusinessPartnerService();
    private final ResilienceConfiguration resilienceConfiguration;
    
    public GetCachedBusinessPartnersCommand(final String erpHttpDestination, BusinessPartnerService service) {
    
        private static final String CATEGORY_PERSON = "1";
        
        destination = DestinationAccessor
                          .getDestination(erpHttpDestination)
                          .asHttp()
                          .decorate(DefaultErpHttpDestination::new);
        
        businessPartnerService = service;
                          
        final ResilienceConfiguration.CacheConfiguration cacheConfig = 
                ResilienceConfiguration.CacheConfiguration
                        .of(Duration.ofSeconds(10))
                        .withoutParameters();
                        
        resilienceConfiguration = ResilienceConfiguration
                .of(BusinessPartnerService.class)
                .isolationMode(ResilienceIsolationMode.TENANT_AND_USER_REQUIRED)
                .timeLimiterConfiguration(ResilienceConfiguration.TimeLimiterConfiguration
                        .of().timeoutDuration(Duration.ofMillies(1000)))
                .bulkHeadConfiguration(ResilienceConfiguration.BulkheadConfiguration
                        .of().maxConcurrentCalls(20))
                .cacheConfiguration(cacheConfig);                         
    }

    public List<BusinessPartner> execute() {
            return ResilienceDecorator.executeSupplier(this::run, myResilienceConfig, e -> Collections.emptyList());
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
                                    .select(
                                            BusinessPartnerAddress.CITY_NAME,
                                            BusinessPartnerAddress.COUNTRY,
                                            BusinessPartnerAddress.TO_EMAIL_ADDRESS
                                                    .select(AddressEmailAddress.EMAIL_ADDRESS)
                                    )
                    )
                    .filter(BusinessPartner.BUSINESS_PARTNER_CATEGORY.eq(category))
                    .orderBy(BusinessPartner.LAST_NAME, Order.ASC)
                    .top(200)
                    .execute(destination);
        } catch (ODataException e) {
            throw new ResilienceRuntimeException(e);
        }
    }

    @Override
    protected List<BusinessPartner> getFallback() {
        logger.warn("Fallback called because of exception:", getExecutionException());
        return Collections.emptyList();
    }
}

```

The `GetCachedBusinessPartnersCommand` class uses the `ResilienceConfiguration` class for caching. The cache is configured by providing an instance of Class `CacheConfiguration` is provided to method `cacheConfiguration`. 

Now that we have a working command with enabled cache features, we can adapt our `BusinessPartnerServlet`.

```
package com.sap.cloud.sdk.tutorial;

import com.google.gson.Gson;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
    
    private static final String DESTINATION_NAME = "MyErpSystem";

    @Override
    protected void doGet(final HttpServletRequest request, final HttpServletResponse response)
            throws ServletException, IOException {
        try {
            final List<BusinessPartner> businessPartners =
                    new GetCachedBusinessPartnersCommand(DESTINATION_NAME, new DefaultBusinessPartnerService ).execute();

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
Here, we simply create a new command and execute it. As before, we get a list of business partners as result. The application will not go down if the OData service is temporarily unavailable. 

With caching in place, when we make multiple calls in succession, it is only the first time that the actual request is made. For the subsequent ones, the cache is used, improving the response times.

**Note**: Instead of "execute()" you are now also able to run `queue()` and `observe()` for asynchronous evaluation.


- `These methods have been overridden:`
- `getCommandCacheKey()`
- It does append the provided category to the `cacheKey`, thus making it distinguishable from the same service calls but with different category parameter
- `getFallback()`
- As usual, we have also provided `getFallback()` method. It the procedure fails in any way, an empty list object is returned. 

This wraps up the tutorial. Stay tuned for more tutorials on the SAP Cloud SDK on topics like UI5 and security!

See the next tutorial in the series here: [Step 7 with SAP Cloud SDK: Secure your Application on SAP Cloud Platform, `CloudFoundry`] (https://blogs.sap.com/2017/07/18/step-7-with-sap-s4hana-cloud-sdk-secure-your-application-on-sap-cloud-platform-cloudfoundry/).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test yourself)]

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]

[VALIDATE_3]

[ACCORDION-END]

---
