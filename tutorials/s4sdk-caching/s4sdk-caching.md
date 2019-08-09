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
A cache generally works by the action of requesting information to a given subject, called a key. If an information to a given key was previously requested, was stored at the time of request, and is now available to read, a so called "cache hit" occurs: the data can be found and will be loaded. A "cache miss" occurs when it cannot.

The most important aspects of a cache is its size and the life time of its items. Both should be limited with regards
 to the use case, to avoid an outdated state or disproportionate memory consumption in the application. The biggest
 effect of using a cache can be witnessed, when the application is repetitively reading larger chunks of data from
 external sources. In such cases, using caches significantly reduce the bandwidth required for transmitting
 information.

Caching is applicable whenever:

- You are willing to spend some memory to improve speed.
- You expect that keys will sometimes get queried more than once.
- Your cache will not need to store more data than what would fit in RAM. (By default, the cache is local to a single run of your application. It does not store data in files, or on outside servers.)


If each of these options apply to your use case, then we highly recommend that you use the caching features provided by
SAP Cloud SDK in your application.


[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Caching Command with SAP Cloud SDK)]

The caching command allows parallel execution and asynchronous computation for efficient programming practices.
Stored information is organized as a local key-value store. For each unique key the same response is expected in case
 of a "cache hit". Otherwise the cache response is computed.

In SAP Cloud SDK, `JCache` (`JSR 107`) is used as underlying caching technology. In this tutorial we will
use the `JCache` adapter [Caffeine] (https://github.com/ben-manes/caffeine) for our purpose.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Cache your OData Call)]

Now that we have covered why caching is important and how it can help us improve performance and responsiveness, it's finally time to introduce it into our application.

In [the resilience tutorial] (https://developers.sap.com/tutorials/s4sdk-resilience.html), we introduced resilience
into our application, using `Resilience4j` library. Now, in order to make our OData calls cacheable, we will extend
the use of `ResilienceConfiguration` class used there and configure caching as well.

Add the following lines at the end of the constructor of the GetBusinessPartnerCommand:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/GetBusinessPartnersCommand.java`

```java
final ResilienceConfiguration.CacheConfiguration cacheConfig =
        ResilienceConfiguration.CacheConfiguration
                .of(Duration.ofSeconds(10))
                .withoutParameters();

myResilienceConfig.cacheConfiguration(cacheConfig);                         
```

As mentioned above, we  `ResilienceConfiguration` class for caching. The cache is configured by providing an instance of Class `CacheConfiguration` to method `cacheConfiguration`.

Now that we have a working command with enabled cache features, we can adapt our `BusinessPartnerServlet`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test the Cache)]

```java
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
Here, we simply create a new command and execute it. As before, we get a list of business partners as result. Due to
resilience call, the application will not go down if the OData service is temporarily unavailable.

Furthermore, with caching in place, when we make multiple calls in succession, it is only the first time that the
actual request is made. For the subsequent calls, the cache is used instead, thus improving the response times.

**Note**: Instead of "execute()" you are now also able to run `queue()` and `observe()` for asynchronous evaluation.

This wraps up the tutorial. Stay tuned for more tutorials on the SAP Cloud SDK on topics like UI5 and security!

See the next tutorial in the series here: [Step 7 with SAP Cloud SDK: Secure your Application on SAP Cloud Platform, `CloudFoundry`] (https://developers.sap.com/tutorials/s4sdk-secure-cloudfoundry.html).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Test yourself)]

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test yourself)]

[VALIDATE_3]

[ACCORDION-END]

---
