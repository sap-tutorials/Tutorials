---
title: Migrate an Existing Application to Version 3.0 of the Cloud SDK
description: Migrate an existing project to version 3.0 of the SAP Cloud SDK.
auto_validation: true
time: 40
tags: [ tutorial>intermediate, products>sap-cloud-sdk, products>sap-business-technology-platform, topic>cloud, topic>java ]
primary_tag: products>sap-cloud-sdk
---

## Prerequisites
 - You already have a basic understanding of how applications with the Cloud SDK are developed. If you are new to the Cloud SDK take a look at [how to create a sample application](group.s4sdk-cloud-foundry).
 - You have the necessary development tools (JDK, maven and git) already installed. Take a look at [how to set up your machine](s4sdk-setup) for details on how to install them.

## Details
### You will learn
  - How to adapt dependencies
  - How to migrate `ErpCommands`
  - How to adapt tests

This tutorial will be based on a sample application to guide you through the fundamental steps of upgrading to version 3. You will get a feeling for the nature of the changes and be able to perform them for your specific application.

Feel free to apply these steps to your application, but be aware that this tutorial does not cover everything in itself. For a comprehensive guide on migrating an app also consult the [migration guide](https://blogs.sap.com/2019/08/01/migrate-to-version-3.0.0-of-the-sap-cloud-sdk-for-java/).

---

[ACCORDION-BEGIN [Step 1: ](Get the V2 application)]

First step is to get an application that uses the SAP Cloud SDK in version 2. For this tutorial we will use the `BookAdressManager`. You can download it from [GitHub](https://github.com/SAP/cloud-s4-sdk-book) or clone the repository with:

```Bash
git clone https://github.com/SAP/cloud-s4-sdk-book
```

Open it as maven project in your favorite IDE and take a moment to familiarize yourself with the structure of the application. It's a simple web app with two `servlets` which rely on `ErpCommand`s to execute OData requests. Also the project contains a few integration and unit tests. Further information on the basic structure of the project is provided in [other tutorials](https://developers.sap.com/tutorials/s4sdk-cloud-foundry-sample-application.html#e55c5249-c70a-47d0-8e16-c97b6e8974a0).

If you want to test the application on your local system follow the instructions of step 2 in the `readme.md` of the project to set up your connection to an S/4HANA system. You can also hit `mvn clean test-compile` to check everything is set up properly and the app builds. In the next steps we will migrate it to version 3 step by step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Update dependencies)]

We'll start by adapting the dependencies of our project. First step is to change the SDK version which is defined in the parent `pom.xml` in your project root. Open it and look for the list of dependencies, where you will find a reference to the `sdk-bom` in version `2.19.0`.

1. Change the `groupId` from `com.sap.cloud.s4hana` to `com.sap.cloud.sdk`.

2. Change the `version` from `2.19.0` to the latest release of the Cloud SDK.

At the time of writing, the latest version of the SDK is `3.3.1`. You can find the current version at [maven central](https://search.maven.org/search?q=g:com.sap.cloud.sdk). Note that we not only increased the version but also changed the group id. With the move to 3.0 some modules have been renamed to better reflect their purpose. To account for this we will now adapt the group ids in our dependencies.

Head to your `application/pom.xml`, `integration-tests/pom.xml` and `unit-tests/pom.xml`. Look for the following dependencies and replace the group and artifact id accordingly:

| Old | | New | |
| `GroupId` | `ArtifactId` | `GroupId` | `ArtifactId` |
|---|---|---|---|
| `com.sap.cloud.s4hana`  | `sdk-bom` | `com.sap.cloud.sdk`  | `sdk-bom` |
| `com.sap.cloud.s4hana`  | `testutil` | `com.sap.cloud.sdk.testutil`  | `testutil-core` |
| `com.sap.cloud.s4hana`  | `s4hana-all` | `com.sap.cloud.sdk.s4hana`  | `s4hana-all` |
| `com.sap.cloud.s4hana.cloudplatform`  | `scp-cf` | `com.sap.cloud.sdk.cloudplatform`  | `scp-cf` |
| `com.sap.cloud.s4hana.quality`  | `listeners-all` | `com.sap.cloud.sdk.quality`  | `listeners-all` |
| `com.sap.cloud.s4hana.plugins`  | `s4sdk-maven-plugin` | `com.sap.cloud.sdk.plugins`  | `usage-analytics-maven-plugin` |

In case of the maven plugin you will also have to increase the version number by hand. Be sure not to specify explicit versions for these dependencies since they are already defined via the `sdk-bom` that we just visited. A full table of changed group IDs is included in the [release notes](https://help.sap.com/doc/6c02295dfa8f47cf9c08a19f2e172901/1.0/en-US/index.html#version-3.0.0-naming). Don't worry if your IDE lights up red in the process, maven might take some time to update. It might be a good idea to restart your IDE.

Also there were small changes as to which transitive dependencies the SDK requires. One library the SDK previously depended upon is `rest-assured`. Now this is no longer the case and projects that use it must reference it explicitly. For this sample app this is necessary, so change the dependency accordingly in `integration-tests/pom.xml`:

```XML
<dependency>
   <groupId>io.rest-assured</groupId>
   <artifactId>rest-assured</artifactId>
   <version>3.2.0</version>
   <scope>test</scope>
   <exclusions>
       <exclusion>
           <groupId>commons-logging</groupId>
           <artifactId>commons-logging</artifactId>
       </exclusion>
   </exclusions>
</dependency>
```

Once you are done, check that all dependencies are resolved correctly by running:

```Bash
mvn clean validate
```

Now that we have updated our dependencies it is time to adapt the actual java code.

[DONE]
[ACCORDION-END]


[ACCORDION-BEGIN [Step 3: ](Migrate simple OData request)]

With the new version of the Cloud SDK it's usage became a lot simpler and more streamlined. This means that our application will become simpler and have less boilerplate code -- we will mostly delete stuff in the following steps. But first we'll have to add a piece.

Start by heading to the `CreateAddressCommand`. It is responsible for executing an OData request that creates a `BusinessPartnerAddress`. Your IDE should inform you about various missing classes. Don't worry about it for now and add the following field to the class:

```Java
private final ErpHttpDestination destination = ErpHttpDestinationUtils.getErpHttpDestination("ERP_SYSTEM");
```

This retrieves the destination by the name `ERP_SYSTEM`. Previously, the destination name was passed in an `ErpConfigContext`, which used a default name if none was provided. With 3.0 `ErpConfigContext` was removed in favor of passing destinations directly.

*Note:* This tutorial assumes your IDE assists with adding the necessary import statements for you. If you have to add them manually, consider the file at the end of the next step that also lists the necessary imports.

With our destination in place we can now modify our very first OData call for 3.0. Dive into the `run()` method of `CreateAddressCommand` and find the statement that creates a new address. Currently, it looks like this:

```Java
final BusinessPartnerAddress addressCreated = service.createBusinessPartnerAddress(address).execute();
```

The statement uses the provided `service` to create a new address. Now change the statement slightly to match the following:

```Java
final BusinessPartnerAddress addressCreated = service.createBusinessPartnerAddress(address)
                    .executeRequest(destination);
```

As you can see, not much has changed. Only `executeRequest()` now takes a destination as input. This may seem uncomfortable at first glance, since our code just got a little more complex. But in fact, the previous execute assumed a default `ErpConfigContext` with a default destination under the name `ErpQueryEndpoint` set in place. With version 3.0 both got removed to increase transparency and offer a more powerful way to specify the system a request shall run against. That is why now the destination has to be passed explicitly with a request.

This wraps up the first step in migration which is all you need if you want to adapt simple requests. However, the code we just changed resides inside a class that made the execution resilient by extending `ErpCommand`. You may have already noticed that `ErpCommand` is no longer found. In fact it was removed in version 3.0 and got replaced by what is called a `ResilienceConfiguration` which we will integrate in the next step.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 4: ](Migrate resilient OData request)]

If you look closely at the `CreateAddressCommand`s constructor you will find the following statement buried in there:

```Java
withExecutionTimeoutInMilliseconds(10000)
```

It sets a timeout for our request of 10 seconds and is one part in an attempt to make the request resilient against failures. With 3.0 this is no longer done by extending `ErpCommand` or `CachingErpCommand` but by wrapping a request with a `ResilienceConfiguration`.

Consider the following code:

```Java
private final TimeLimiterConfiguration timeLimit = TimeLimiterConfiguration.of()
            .timeoutDuration(Duration.ofSeconds(10));

private final ResilienceConfiguration resilienceConfiguration =
            ResilienceConfiguration.of(AddressServlet.class)
            .timeLimiterConfiguration(timeLimit);
```

It sets up a `ResilienceConfiguration` which holds all the parameters and configuration necessary to run resilient requests. One of them is the timeout which is defined in a `TimeLimiterConfiguration` which then is added to the `ResilienceConfiguration`.

Here we again set the timeout to 10 seconds. Of course the configuration allows for more parameters to be tweaked. Here we go with the default values (except for the timeout) but feel free to take a look at what is provided over at the full [migration guide](https://blogs.sap.com/2019/08/01/migrate-to-version-3.0.0-of-the-sap-cloud-sdk-for-java/). Last but not least take note of the `of(AddressServlet.class)` part that creates the resilience configuration. The class name is simply used as an identifier here. You may provide your own string identifier if you like.

Such a configuration may be attached to any request that is to be executed in a resilient manner. Not only allows this for a clear assembly of the desired properties, but it also makes the configuration reusable. One configuration may be used by different requests.

Thus, add the code above to the `AddressServlet` just beneath where the service is defined.

Let's see how we now can use the configuration in our requests. Again, let's first understand the code:

```Java
final BusinessPartnerAddress addressCreated = ResilienceDecorator.executeCallable(
                    () -> service.createBusinessPartnerAddress(address).executeRequest(destination),
                    resilienceConfiguration);
```

As you can see, instead of calling the service directly, we now wrap the call by means of the `ResilienceDecorator`. We supply our initial request as callable together with the `resilienceConfiguration` we just created. And that's it. The decorator takes care of handling the request in the resilient way we configured above.

So let's put together the individual pieces and adapt the complete request.

1. Move the `ErpHttpDestination` we introduced in step 2 from `CreateAddressCommand` over to the `AddressServlet`:
    ```Java
    private final ErpHttpDestination destination = ErpHttpDestinationUtils.getErpHttpDestination("ERP_SYSTEM");
    ```
2. In `AddressServlet` navigate to the `CreateAddressCommand(service, address).executeRequest()` call inside `doPost()` and replace it to now use the resilience configuration:
    ```Java
    final BusinessPartnerAddress addressCreated = ResilienceDecorator.executeCallable(
                    () -> service.createBusinessPartnerAddress(address).executeRequest(destination),
                    resilienceConfiguration);
    ```
3. Last but not least remove the `CreateAddressCommand` class altogether.

That's right, we don't need the `CreateAddressCommand` class anymore. Instead of always inheriting `ErpCommand` in a dedicated class for a command we can now just build and execute our request in a single line of code. The `ResilienceDecorator` takes care of the heavy lifting for us.

With that we successfully migrated our resilient request to to version 3.0. You can now apply and test what you just learned by updating the `UpdateAddressCommand` and `DeleteAddressCommand` in the same manner. In case of the latter, build the `addressToDelete` required by `deleteBusinessPartnerAdrdress` directly in the servlet. If you'd like to jump directly to the next section either copy the relevant parts from below or come back later to adapt the commands.

At the end of this step, the `AddressServlet` should look similar to this (code shortened for better visibility of what changed):

```Java
package com.sap.cloud.s4hana.examples.addressmgr;

import com.google.common.base.Charsets;
import com.google.common.base.Strings;
import com.google.common.io.CharStreams;
import com.google.gson.Gson;
import com.google.gson.JsonParseException;
import org.slf4j.Logger;

import javax.servlet.ServletException;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.InputStreamReader;
import java.time.Duration;

import com.sap.cloud.s4hana.examples.addressmgr.util.HttpServlet;
import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration.TimeLimiterConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceDecorator;

import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestination;
import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestinationUtils;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartnerAddress;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.BusinessPartnerService;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.DefaultBusinessPartnerService;

@WebServlet("/api/addresses")
public class AddressServlet extends HttpServlet {
   private static final long serialVersionUID = 1L;
   private static final Logger logger = CloudLoggerFactory.getLogger(AddressServlet.class);

   private final BusinessPartnerService service = new DefaultBusinessPartnerService();
   private final ErpHttpDestination destination = ErpHttpDestinationUtils
           .getErpHttpDestination("ERP_SYSTEM");

   private final TimeLimiterConfiguration timeLimit = TimeLimiterConfiguration.of()
           .timeoutDuration(Duration.ofSeconds(10));

   private final ResilienceConfiguration resilienceConfiguration =
           ResilienceConfiguration.of(AddressServlet.class)
            .timeLimiterConfiguration(timeLimit);

   @Override
   protected void doPost(final HttpServletRequest request, final HttpServletResponse response)
           throws ServletException, IOException {
       // ...
       logger.info("Received post request to create address {}", address);
       try {
         final BusinessPartnerAddress addressCreated = ResilienceDecorator.executeCallable(
                         () -> service.createBusinessPartnerAddress(address).executeRequest(destination),
                         resilienceConfiguration);
           response.setStatus(HttpServletResponse.SC_CREATED);
           response.setContentType("application/json");
           response.getWriter().write(new Gson().toJson(addressCreated));
       } catch (Exception e) {
           // ...
       }
   }

   // ...

   @Override
   protected void doPatch(final HttpServletRequest request, final HttpServletResponse response)
           throws ServletException, IOException {
       // ...
       final BusinessPartnerAddress addressToUpdate = createAddressToUpdate(businessPartnerId, addressId, addressFromBody);

       logger.info("Received patch request to update address {}", addressToUpdate);
       try {
         ResilienceDecorator.executeCallable(
                         () -> service.updateBusinessPartnerAddress(addressToUpdate).executeRequest(destination),
                        resilienceConfiguration);

           response.setStatus(HttpServletResponse.SC_NO_CONTENT);
       } catch (Exception e) {
           // ...
       }
   }

   // ...

   @Override
   protected void doDelete(final HttpServletRequest request, final HttpServletResponse response)
           throws ServletException, IOException {
       // ...
       logger.info("Received delete request to delete address {},{}", businessPartnerId, addressId);
       final BusinessPartnerAddress addressToDelete = BusinessPartnerAddress.builder()
               .businessPartner(businessPartnerId)
               .addressID(addressId)
               .build();
       try {
         ResilienceDecorator.executeCallable(
                         () -> service.deleteBusinessPartnerAddress(addressToDelete).executeRequest(destination),
                         resilienceConfiguration);

           response.setStatus(HttpServletResponse.SC_NO_CONTENT);
       } catch (Exception e) {
           // ...
       }
   }
}
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 5: ](Migrate cached OData request)]

Now we'll move away from the `AddressServlet` and tend to our business partner commands. For the purposes of this tutorial we'll keep the classes instead of removing them. When to have a dedicated class for a command or group of commands now is up to you. For more complex requests it might be convenient to have a dedicated class to keep the code clean and organized.

Tend to the `GetSingleBusinessPartnerByIdCommand` and compare it to the following updated version:

```Java
package com.sap.cloud.s4hana.examples.addressmgr.commands;

import org.slf4j.Logger;

import java.time.Duration;

import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration.TimeLimiterConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration.CacheConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceDecorator;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;

import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestination;
import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestinationUtils;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartnerAddress;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.BusinessPartnerService;

public class GetSingleBusinessPartnerByIdCommand {
    private static final Logger logger = CloudLoggerFactory.getLogger(GetAllBusinessPartnersCommand.class);

    private final ErpHttpDestination destination;
    private final BusinessPartnerService service;
    private final ResilienceConfiguration.TimeLimiterConfiguration timeLimit;
    private final ResilienceConfiguration.CacheConfiguration cacheConfiguration;
    private final ResilienceConfiguration resilienceConfiguration;

    private final String partnerId;


    public GetSingleBusinessPartnerByIdCommand(final BusinessPartnerService service, final String partnerId) {
        this.service = service;
        this.partnerId = partnerId;

        destination = ErpHttpDestinationUtils.getErpHttpDestination("ERP_SYSTEM");
        timeLimit = ResilienceConfiguration.TimeLimiterConfiguration.of()
                .timeoutDuration(Duration.ofSeconds(10));
        cacheConfiguration = ResilienceConfiguration.CacheConfiguration
                .of(Duration.ofMinutes(5))
                .withParameters(partnerId);
        resilienceConfiguration = ResilienceConfiguration.of(GetSingleBusinessPartnerByIdCommand.class)
                .timeLimiterConfiguration(timeLimit)
                .cacheConfiguration(cacheConfiguration);
    }

    public BusinessPartner execute() {
        return ResilienceDecorator.executeCallable(
                this::run,
                resilienceConfiguration,
                e -> BusinessPartner.builder().businessPartner(partnerId).build()
        );
    }

    private BusinessPartner run() throws ODataException {
        return service
                .getBusinessPartnerByKey(partnerId)
                .select(BusinessPartner.BUSINESS_PARTNER,
                        BusinessPartner.LAST_NAME,
                        BusinessPartner.FIRST_NAME,
                        BusinessPartner.IS_MALE,
                        BusinessPartner.IS_FEMALE,
                        BusinessPartner.CREATION_DATE,
                        BusinessPartner.TO_BUSINESS_PARTNER_ADDRESS.select(
                                BusinessPartnerAddress.BUSINESS_PARTNER,
                                BusinessPartnerAddress.ADDRESS_ID,
                                BusinessPartnerAddress.COUNTRY,
                                BusinessPartnerAddress.POSTAL_CODE,
                                BusinessPartnerAddress.CITY_NAME,
                                BusinessPartnerAddress.STREET_NAME,
                                BusinessPartnerAddress.HOUSE_NUMBER))
                .executeRequest(destination);
    }
}
```

Let's go through the changes step by step:

- The command no longer extends `CachingErpCommand` and everything associated with it (constructor, overriding methods, properties) got removed.
- The destination and resilience configuration we just used in the `AddressServlet` were also added here (with a different identifier).
- The resilience configuration was expanded with a `CacheConfiguration`.
- The resilient execution was integrated into the execute method.
- The `ResilienceDecorator.executeCallable` now is provided an additional parameter in the form of a lambda function.

Let's take a closer look at what is new. Starting with the last point: the `ResilienceDecorator` allows us to specify a fallback behavior to act out in case of a failure. Previously this was achieved by overriding the `getFallback()` method of `ErpCommand`. Now the fallback functionality is added fairly easily by providing a fallback function to the `ResilienceDecorator`. Here we just provide a lambda that constructs a new business partner based on the id we passed:

```Java
return ResilienceDecorator.executeCallable(
        this::run,
        resilienceConfiguration,
        e -> BusinessPartner.builder().businessPartner(partnerId).build()
);
```

The bigger change here is the implementation of our cache, which is now provided by the aforementioned `CacheConfiguration`:

```Java
cacheConfiguration = ResilienceConfiguration.CacheConfiguration
        .of(Duration.ofMinutes(5))
        .withParameters(partnerId);
```

Setting up the cache consists of two steps:

1. Setting the duration after which the cache expires
2. Declaring the parameters associated with cached data

For this command we need to not only store the result but also the parameter it was retrieved with, specifically the `partnerId`. The SDK handles this for us since we specify the parameter when building the cache via `.withParameters(partnerId)`. Also we choose 5 minutes as timeout just like before.

As you may have already noticed, the cache configuration is then build into our resilience configuration and therefore automatically applied by the `ResilienceDecorator`. This allows for convenient execution of cached requests and does not rely on manually setting up the cache. Be cautious though when re-using the resilience configuration for other commands. Now that we introduced a cache and bound it to the configuration it would be shared by multiple requests.

Now that we went through the adaptations copy over the new command.

Also go ahead and apply the same approach to the `GetAllBusinessPartnersCommand`. The main difference here will be that the caching does not need to keep track of any parameters. You will find a full updated version of the class in the [Appendix](#Appendix).

One more thing on caching: With 3.0 the underlying framework was changed. Now the SDK relies on `JCache` [JSR 107](https://github.com/jsr107/jsr107spec). It defines a caching API but is not an implementation in itself. The SDK also does not export an implementation for it but instead gives the user the freedom to use any implementation they like. But this also means that we have to provide an implementation for our application. For this tutorial we will use [`Caffine`](https://github.com/ben-manes/caffeine). The required dependency can be quickly added to the application `pom.xml`:

```XML
<dependency>
    <groupId>com.github.ben-manes.caffeine</groupId>
    <artifactId>jcache</artifactId>
    <version>2.7.0</version>
</dependency>
```

This wraps up the steps necessary to migrate the application itself. Go ahead and run `mvn clean compile` to check that everything assembles smoothly (assuming you changed both commands).

Now that the main code is changed let's go ahead to adapting our tests.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 6: ](Adapt unit tests)]

First we are going to make changes to the unit tests. Therefore, head towards the `GetAllBusinessPartnersCommandTest` under `unit-tests/` and find the `before()` method. Here you will find stuff that is set up before each test is run. Take note of the invalidation of caches that is performed. Due to the change of the underlying caching framework we'll need to adapt this statement. Replace it with the following code:

```Java
Caching.getCachingProvider().getCacheManager().destroyCache(GetAllBusinessPartnersCommand.class.getName());
```

It does the exact same thing as before: Getting the specific cache of our command and invalidating it. Instead of constructing a new command the cache is now identified by a string identifier. Here, we used the class name.

Next move on the `mockService()` block. Here we need to account for the changed execute method that now expects a destination. To account for the change modify the code to allow for any destination in the request:

```Java
return when(service
                .getAllBusinessPartner()
                .select(any(BusinessPartnerSelectable.class))
                .filter(BusinessPartner.BUSINESS_PARTNER_CATEGORY.eq("1"))
                .orderBy(BusinessPartner.LAST_NAME, Order.ASC)
                .executeRequest(any()));
```

The `any()` expression inside `execute` allows any destination to be passed but you could also pass a specific destination here to test the correct destination is passed in your code.

And that's all that is to it. Go ahead and run the test to see that it is working as expected by issuing:

```Bash
mvn clean test -pl unit-tests -am
```

Then proceed with tackling the integration tests.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 7: ](Adapt integration tests)]

We'll now proceed with changing the integration tests and go trough the necessary changes one by one.

1. Head to the `Testutil` class and find the `createDeployment` method. Replace it's content with the following code:

    ```Java
    return ShrinkWrap
            .create(WebArchive.class)
            .addClasses(classesUnderTest)
            .addClasses(RequestThreadContextListener.class)
            .addAsManifestResource("arquillian.xml")
            .addAsWebInfResource(new ByteArrayAsset("<beans/>".getBytes()), ArchivePaths.create("beans.xml"));
    ```

      This replaces the different context listeners with the `RequestThreadContextListener` that is used instead as of 3.0. To read more about this change consult the more extensive [migration guide](https://blogs.sap.com/2019/08/01/migrate-to-version-3.0.0-of-the-sap-cloud-sdk-for-java/).

2. Dive into the actual `AddressServletTest` and replace the `getAddress(..)` method as follows:

    ```Java
    private BusinessPartnerAddress getAddress(final String bupaId, final String addressId) {
        final BusinessPartnerService service = new DefaultBusinessPartnerService();
        final ErpHttpDestination destination = ErpHttpDestinationUtils.getErpHttpDestination("ERP_SYSTEM");
        return ResilienceDecorator.executeCallable(
                () -> service.getBusinessPartnerAddressByKey(bupaId,addressId).executeRequest(destination),
                ResilienceConfiguration.of(AddressServletTest.class),
                e ->  null
        );
    }
    ```

    Previously it used a `GetAddressCommand` to retrieve addresses directly from the test S/4 system to compare them to the expected outcome of tests. The new code simply migrates this command in the same way we migrated the other commands in step 4. Therefore the now obsolete `GetAddressCommand` can be removed.

3. Last but not least find the `mockErpDestination()` in the `before()` block of `AddressServletTest`. In 3.0 it now requires two arguments:

    - A destination name
    - An alias

    The name is necessary since the default name `ErpQueryEndpoint` got removed. Same goes for the alias that is used to pull destinations from the `systems.yaml` file.

    Go ahead and fill in the destination name (in this example "`ERP_SYSTEM`") and the alias defined in your `systems.yaml`. Also apply the exact same change to `BusinessPartnerServletTest`.

With that the integration tests are now set up an ready to run with version 3.0. Assure that everything is running smoothly by issuing:

```Bash
mvn clean test-compile -pl integration-tests -am
```

>In order for the integration tests to run an instance of a S/4HANA system populated with some test data is required. Specifically an existing business partner is needed. Find any business partner in your S/4 system and use their id under `BusinessPartnerServletTest.BUPA_ID` for the tests. Then run the tests with `mvn clean test`.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 8: ](Conclusion)]

And with that the entire application is successfully migrated to a new version of the Cloud SDK. Version 3.0 offers a more streamlined and powerful API. Migration requires some tweaking but most changes necessary are rather minor.

Find out more about what's new and how to migrate your application by taking a look into the [release notes](https://help.sap.com/doc/6c02295dfa8f47cf9c08a19f2e172901/1.0/en-US/index.html#version-3.0.0-naming) and the [migration guide](https://blogs.sap.com/2019/08/01/migrate-to-version-3.0.0-of-the-sap-cloud-sdk-for-java/). If you like you can now deploy your newly migrated application locally or to Cloud Foundry. If you previously used the `ALLOW_MOCKED_AUTH_HEADER` variable for testing you may remove that, it was removed with 3.0. More information can again be found in the migration guide. You may also get the fully migrated source code of the sample application used here by checking out the [`sdk-v3-migration` branch of the repository](https://github.com/SAP/cloud-s4-sdk-book/tree/sdk-v3-migration).

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Test yourself)]

[VALIDATE_1]

[ACCORDION-END]

[ACCORDION-BEGIN [Appendix: ](Appendix)]

`GetAllBusinessPartnersCommand`:

```Java
package com.sap.cloud.s4hana.examples.addressmgr.commands;

import org.slf4j.Logger;

import java.time.Duration;
import java.util.Collections;
import java.util.List;

import com.sap.cloud.sdk.cloudplatform.logging.CloudLoggerFactory;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration.CacheConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceConfiguration.TimeLimiterConfiguration;
import com.sap.cloud.sdk.cloudplatform.resilience.ResilienceDecorator;
import com.sap.cloud.sdk.datamodel.odata.helper.Order;
import com.sap.cloud.sdk.datamodel.odata.client.exception.ODataException;

import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestination;
import com.sap.cloud.sdk.s4hana.connectivity.ErpHttpDestinationUtils;
import com.sap.cloud.sdk.s4hana.datamodel.odata.namespaces.businesspartner.BusinessPartner;
import com.sap.cloud.sdk.s4hana.datamodel.odata.services.BusinessPartnerService;

public class GetAllBusinessPartnersCommand {
    private static final Logger logger = CloudLoggerFactory.getLogger(GetAllBusinessPartnersCommand.class);

    private static final String CATEGORY_PERSON = "1";
    private final ErpHttpDestination destination = ErpHttpDestinationUtils
            .getErpHttpDestination("ERP_SYSTEM");

    private final BusinessPartnerService service;
    private final TimeLimiterConfiguration timeLimit;
    private final CacheConfiguration cacheConfiguration;
    private final ResilienceConfiguration resilienceConfiguration;


    public GetAllBusinessPartnersCommand(final BusinessPartnerService service) {
        this.service = service;
        timeLimit = TimeLimiterConfiguration.of()
                .timeoutDuration(Duration.ofSeconds(10));
        cacheConfiguration = CacheConfiguration
                .of(Duration.ofMinutes(5)).withoutParameters();
        resilienceConfiguration = ResilienceConfiguration.of(GetAllBusinessPartnersCommand.class)
                .timeLimiterConfiguration(timeLimit)
                .cacheConfiguration(cacheConfiguration);
    }

    public List<BusinessPartner> execute() {
        return ResilienceDecorator.executeCallable(
                this::run,
                resilienceConfiguration,
                e -> Collections.emptyList()
        );
    }

    private List<BusinessPartner> run() throws ODataException {
        return service
                .getAllBusinessPartner()
                .select(BusinessPartner.BUSINESS_PARTNER,
                        BusinessPartner.LAST_NAME,
                        BusinessPartner.FIRST_NAME)
                .filter(BusinessPartner.BUSINESS_PARTNER_CATEGORY.eq(CATEGORY_PERSON))
                .orderBy(BusinessPartner.LAST_NAME, Order.ASC)
                .executeRequest(destination);
    }
}
```

[DONE]
[ACCORDION-END]


---
