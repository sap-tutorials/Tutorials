---
title: Introduce caching to your application
description: Introduce caching to your application using the SAP Cloud SDK.
auto_validation: true
primary_tag: products>sap-s-4hana-cloud-sdk
tags: [ tutorial>intermediate, products>sap-s-4hana-cloud-sdk ]
time: 15
---


## Details
### You will learn
  - What caching is and why you should care about it
  - How a cache works
  - Uses of a caching command
  - How to cache your OData Call
  - How to do the configuration and `parameterized` calls


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

As underlying technology [Guava](https://github.com/google/guava) is being used, which gives you options to customize the cache configuration.

- Cache size - Number of items being stored by the cache. If the number of items exceeds this limit, the most outdated value will be removed from the cache
- Item expiration time - Items are checked for their age. When the individual expiration time is exceeded, the value will be removed from the cache
- Concurrency level - Expected number of threads in the application able to change the cache concurrently
- Eviction - Size based and timed eviction (default), reference-based (weak keys, weak values, soft values)

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Cache your OData Call)]

Now that we have covered why caching is important and how it can help us improve performance and responsiveness, it's finally time to introduce it into our application. In the last tutorial we created a simple `ErpCommand` that uses the SDK's OData abstractions to retrieve business partners from an ERP system while guaranteeing resilience provided by `Hystrix`. In order to make this call cacheable, we have to start with a new type extending `CachingErpCommand`. So first we will create the following class:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/GetCachedBusinessPartnersCommand.java`
```
package com.sap.cloud.sdk.tutorial;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import org.slf4j.Logger;

import java.util.Collections;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.cache.CacheKey;
import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;

import com.sap.cloud.sdk.s4hana.connectivity.CachingErpCommand;
import com.sap.cloud.sdk.s4hana.datamodel.odata.helper.Order;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.DefaultBusinessPartnerService;

public class GetCachedBusinessPartnersCommand extends CachingErpCommand<List<BusinessPartner>> {
    private static final Logger logger = CloudLoggerFactory.getLogger(GetCachedBusinessPartnersCommand.class);

    private static final String CATEGORY_PERSON = "1";

    private static final Cache<CacheKey, List<BusinessPartner>> cache =
            CacheBuilder.newBuilder().build();

    public GetCachedBusinessPartnersCommand() {
        super(GetCachedBusinessPartnersCommand.class);
    }

    @Override
    protected Cache<CacheKey, List<BusinessPartner>> getCache() {
        return cache;
    }

    @Override
    protected List<BusinessPartner> runCacheable()
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
The `GetCachedBusinessPartnersCommand` class inherits from `CachingErpCommand`, which is the SDK's abstraction to provide easy to use cacheable `Hystrix` commands. To implement a valid `CachingErpCommand` we need to do the three things:

1. Provide a constructor. Here we simply add the default constructor.
2. Override the `runCacheable`() method. As you might have noticed already, we can simply take the code we used to call our OData service via the Virtual Data Model from the previous blog posts, and put it into the method. You can even specify Java exceptions. So no changes needed!
3. Override the `getCache`() method. Since it might be possible for you to use any cache utility, you need to instantiate the Cache object yourself and provide a method for the internal usage.

Now that we have a working command with enabled cache features, we can adapt our `BusinessPartnerServlet`.

**Note:** With regard to the `Hystrix` example of the previous blog post, the only change will be a different class name of the command.

```
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
                    new GetCachedBusinessPartnersCommand().execute();

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
Just like in the last blog post, we simply create a new command and execute it. As before, we get a list of business partners as result. The application will not go down if the OData service is temporarily unavailable. And now we can have multiple successive calls, but still only request the first one and have the cache to read for the following.

**Note**: Instead of "execute()" you are now also able to run `queue()` and `observe()` for asynchronous evaluation.

When deploying on Cloud Foundry, make sure to follow the corresponding instructions from [Step 5 of this tutorial series] (https://blogs.sap.com/2017/06/23/step-5-resilience-with-hystrix/).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Cache Configuration and Parameterized Calls)]

Now let's take a look into the important features of the cache, the `parameterized` cache calls.

You might be working with queries using result changing parameters, like filters for OData requests. A new class declaration for every possible command variant will be impossible. That's why we encourage developers to pay respect to the cache keys. The cache will dynamically adapt the provided key to your parameter delivery.

For example, given the stated `BusinessPartner` command, it may be useful to have an option to filter `BusinessPartner` items by different categories, especially when having an option to choose from on a website. Applying a cache will subsequently lessen computation costs. Please find the following class:
`./application/src/main/java/com/sap/cloud/sdk/tutorial/GetCachedBusinessPartnersByCategoryCommand.java`

```
package com.sap.cloud.sdk.tutorial;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import org.slf4j.Logger;

import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import com.sap.cloud.sdk.cloudplatform.cache.CacheKey;
import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;

import com.sap.cloud.sdk.s4hana.connectivity.CachingErpCommand;
import com.sap.cloud.sdk.s4hana.datamodel.odata.helper.Order;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.DefaultBusinessPartnerService;

public class GetCachedBusinessPartnersByCategoryCommand extends CachingErpCommand<List<BusinessPartner>> {
    private static final Logger logger = CloudLoggerFactory.getLogger(GetCachedBusinessPartnersByCategoryCommand.class);

    private static final Cache<CacheKey, List<BusinessPartner>> cache =
            CacheBuilder.newBuilder()
                    .maximumSize(100)
                    .expireAfterAccess(5, TimeUnit.MINUTES)
                    .concurrencyLevel(10)
                    .build();

    private final String category;

    public GetCachedBusinessPartnersByCategoryCommand(final String category) {
        super(GetCachedBusinessPartnersByCategoryCommand.class);
        this.category = category;
    }

    @Override
    protected Cache<CacheKey, List<BusinessPartner>> getCache() {
        return cache;
    }

    @Override
    protected CacheKey getCommandCacheKey() {
        return super.getCommandCacheKey().append(category);
    }

    @Override
    protected List<BusinessPartner> runCacheable()
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
                        .filter(BusinessPartner.BUSINESS_PARTNER_CATEGORY.eq(category))
                        .orderBy(BusinessPartner.LAST_NAME, Order.ASC)
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
You see the following changes, compared to the simple cached-command example:

- `The constructor features a parameter, a `String` representation of a category parameter. This parameter is used in the OData query.`
- The cache instance now defines:
- `maximum item size of 100`
- Up to a hundred business partner queries and their responses will be cached
- `expiration after `5 minutes`
- The item associated to any key which is older than five minutes will be re-requested by the command`
- `concurrency level of `10`
- This value provides a hint for the underlying caching API to estimate the number of threads trying to write into the cache at the same time. Concurrent reading access will be unaffected from this setting`



- `These methods have been overridden:`
- `getCommandCacheKey()`
- It does append the provided category to the `cacheKey`, thus making it distinguishable from the same service calls but with different category parameter
- `getFallback()`
- As before, in case the `runCachable` procedure fails in any way, we provide a fallback solution. In this example we return an empty list object

To use the new class in the servlet, just read a parameter from the "`request`" input variable and use it for instantiating this `parameterized` command class. Or you could create different servlet endpoints for persons (category "1"), organizations (2) and groups (3), which supply the corresponding category to the newly created `parameterized` command. Feel free to try it out!

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
