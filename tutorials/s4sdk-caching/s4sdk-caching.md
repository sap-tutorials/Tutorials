---
title: Introduce Caching to Your Application
description: Introduce caching to your application using the SAP Cloud SDK.
auto_validation: true
primary_tag: products>sap-cloud-sdk
tags: [ tutorial>intermediate, products>sap-cloud-sdk, products>sap-business-technology-platform, topic>cloud, topic>java ]
time: 15
---


## Details
### You will learn
  - What caching is and why you should care about it
  - How a cache works
  - How to cache your OData Call
  - How to configure the cache


---

[ACCORDION-BEGIN [Step 1: ](Caches)]

Sometimes service calls from your application to external servers turn out to be quite expensive in terms of performance and latency. Further evaluation of the queried data can make things even worse once critical response times are reached for the clients and customers.

To improve responsiveness to the users, the data requested internally by your application can often be stored for subsequent calls. This can be achieved in such a way, that for every request the information previously stored by the application can be reused. This general behavior is called a cache. A cache stores copies of information passing through it. Besides serving improved responsiveness, the technical goal is to reduce the overall required bandwidth and processing requirements, thus eventually lessening the server load and perceived lag. This way the amount of information, that needs to be transmitted across networks, can be reduced.

Caches are very important in a wide variety of use cases. It is one of the reasons for the advancements of our modern internet experience, like on-demand multimedia streaming and persistent cloud storage. Unlike web caches, which save whole request and response documents, an internal application cache serves the purpose of persisting interim data for multiple intended uses. Whenever information is expensive to compute or retrieve, and its value on a certain input is needed more than once, a cache should be considered.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](How do they work?)]
A cache generally works by the action of requesting information to a given subject, called a key. If an information to a given key was previously requested, was stored at the time of request, and is now available to read, a so called "cache hit" occurs: the data can be found and will be loaded. A "cache miss" occurs when it cannot.

The most important aspects of a cache is its size and the life time of its items. Both should be limited with regards to the use case, to avoid an outdated state or disproportionate memory consumption in the application. The biggest effect of using a cache can be witnessed, when the application is repetitively reading larger chunks of data from external sources. In such cases, using caches significantly reduce the bandwidth required for transmitting information.

Caching is applicable whenever:

- You are willing to spend some memory to improve speed.
- You expect that keys will sometimes get queried more than once.
- Your cache will not need to store more data than what would fit in RAM. (By default, the cache is local to a single run of your application. It does not store data in files, or on outside servers.)


If these requirements apply to your use case, then we highly recommend that you use the caching features provided by the SAP Cloud SDK in your application. Now that you have seen why caching is useful, you will learn what the Cloud SDK provides to enable it.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 3: ](Caching with SAP Cloud SDK)]

The Cloud SDK makes it easy to cache your requests since it handles most of the complexity under the hood. This includes handling of tenant-aware requests, which is essential in a multi-tenant application. The SDK will isolate the cache on a tenant or principal level automatically, if your request requires it.

In SAP Cloud SDK, `JCache` (`JSR 107`) is used as underlying caching technology. In this tutorial, you will use the `JCache` adapter [Caffeine] (https://github.com/ben-manes/caffeine) for this purpose, but you can use any implementation you like. For Caffeine, add the following dependency to your application `pom.xml`:

```XML
<dependency>
  <groupId>com.github.ben-manes.caffeine</groupId>
  <artifactId>jcache</artifactId>
  <scope>runtime</scope>
  <version>2.7.0</version>
</dependency>
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Cache your OData call)]

Now that we have covered why caching is important and how it can help us improve performance and responsiveness, it's finally time to introduce it into your application.

In [the resilience tutorial](s4sdk-resilience), you introduced resilience into your application using `resilience4j`. Now, in order to make our OData calls cacheable, you will enhance the `ResilienceConfiguration` and add a `CacheConfiguration` to it.

Add the following lines at the end of the constructor of the `GetBusinessPartnerCommand`:

`./application/src/main/java/com/sap/cloud/sdk/tutorial/GetBusinessPartnersCommand.java`

```Java
final ResilienceConfiguration.CacheConfiguration cacheConfig =
        ResilienceConfiguration.CacheConfiguration
                .of(Duration.ofSeconds(10))
                .withoutParameters();

myResilienceConfig.cacheConfiguration(cacheConfig);                         
```

As mentioned above, the `ResilienceConfiguration` is used to integrate the caching functionality, which is described in a `CacheConfiguration`. There are two steps to your configuration:

1. Determine how long objects are to be cached
2. Declare the parameters that need to be stored together with the cached data

The first step is obviously necessary since data should be stored for a limited time only. The longer you keep the cached information, the more outdated it will become. How long you want to keep data cached depends on your specific use case. How fast do you expect the information to be outdated? How frequently will the data be accessed? The timeout sets a trade-off between the data is up to date and the application is responsive.

Secondly, you specify the parameters that are to be cached with the data. For your request to retrieve a list of business partners no parameters are necessary, so you can build your cache `wihtoutParameters`. But imagine you want to fetch information about a specific business partner by passing an ID to the system. In order to cache such a request, the cache needs to not only remember the result received, but also the ID that was associated with it. In such a case one can simply pass such parameters by using `.withParameters(param1, param2, ..)`.

Feel free to test that subsequent requests respond faster compared to the first request issued. Deploy your application locally or in the cloud and access the list of business partners a couple of times.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Test the cache)]

Now that you have a working command with caching functionality you also have to adapt your test. Recall the test you prepared to check your resilient command falls back to an empty list in case of failure. Note that this behavior has now changed slightly.

If your servlet got the desired result cached from a previous call, and the ERP system is temporarily not available, your cache will still return the data. But the test expects the result to be empty in that case. In order to account for this behavior and to see if our cache is working as expected let's adapt the test to account for caching. Replace the `testWithFallback` test with the following code:

`integration-tests/src/test/java/com/sap/cloud/sdk/tutorial/BusinessPartnerServletTest.java`:

```Java
@Test
public void testCache() {
    // TODO: insert your service URL down below
    mockUtil.mockDestination(MockDestination.builder(DESTINATION_NAME, URI.create("https://URL")).build());
    when()
            .get("/businesspartners")
            .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body(jsonValidator_List);

    // Simulate a failed VDM call with non-existent destination
    DestinationAccessor.setLoader((n, o) -> Try.success(dummyDestination));
    when()
            .get("/businesspartners")
            .then()
            .statusCode(200)
            .contentType(ContentType.JSON)
            .body(jsonValidator_List);
}
```

Here, the test expects the request still to be successful, even after swapping out the destination for a dummy one.

>If you are using the `systems.yml` and `credentials.yml` files (revisit step 4 of the [previous tutorial](s4sdk-resilience)), mock the destination like this:
```Java
mockUtil.mockDestination(DESTINATION_NAME, "ERP_001");
```


[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](More on testing)]

The introduction of caching has some implications for how you should test your application. In order for tests to validate a single piece of functionality and for them to be reliable they must be independent of each other. The order of execution should not matter and one test may not depend on the successful execution of other tests. So far, this was the case for your integration tests. But now the added cache holds a state that is shared between the tests in your test class, which has to be accounted for.

Take a look back at the test you just replaced:

```Java
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
```

Executed on its own, this test will still pass as it did before. However, if `testService` is run prior to this test the result will be cached and `testWithFallback` will fail since the cached data is returned instead of falling back to an empty list. By changing the test you avoided such issues since your new test does not interfere with other tests. While being sufficient for this tutorial, in a productive setting one should implement a more sophisticated and robust test setup, where caches are invalidated between tests.

This wraps up the tutorial. In the next step of this series you will learn how to secure your application on Cloud Foundry.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Test yourself)]

[VALIDATE_2]

[ACCORDION-END]

[ACCORDION-BEGIN [Step 9: ](Test yourself)]

[VALIDATE_3]

[ACCORDION-END]

---
