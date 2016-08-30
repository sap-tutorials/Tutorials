---

title: Continuous Integration (CI) Best Practices with SAP: CI/CD Practices
description: Part 2.1: The Practices and Principles of CI and CD.
tags: [  tutorial>intermediate, tutorial:type/project ]

---

## Prerequisites

  - **Proficiency:** Intermediate

## Next Steps
 
  - [Pipeline Suggestions](http://go.sap.com/developer/tutorials/ci-best-practices-pipelines.html)
  - [Back to the Navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)

---

The terms Continuous Integration (CI) and Continuous Delivery (CD) are both widely used in software engineering: CI as the adoption of agile principles; CD as a combination of agile methodology techniques and a high-quality delivery process. Whereas CI addresses building a software product from the contributions of single members of a development team in a controlled though lean way, the goal of CD is validation of every change, preferably in an automated way, so that it is potentially shippable.

What "shippable" means depends on the context. It could mean, for example, moving a web application feature into production mode or making software available from a download site. Both CI and CI serve one-person-shops as well as multiple teams working on one product. They can be adjusted to individual requirements and constraints. They are also flexible in that you can decide "how agile" you want the delivery process to be, which means you need not implement  practices that do not suit the environment. At the same time, you can expand the process as needed.

The next section outlines best practices for both CI and CD.


### Continuous Integration, Delivery and Deployment

The practices of Continuous Integration are well-known. There are many sources for developers or build operators to get advice including the following:

> Wikipedia: https://en.wikipedia.org/wiki/Continuous_integration   
> Martin Fowler: http://www.martinfowler.com/articles/continuousIntegration.html

Continuous Integration (CI) focuses on the controlled integration of any change that is created in a collaborative software project into a common main line.

The CI practices are:

  - Keep everything under version control
  - Automate the build
  - Run unit test in the build
  - Commit early and often
  - Build each change
  - Fix build errors immediately
  - Keep the build fast
  - Test in a clone of the production environment
  - Make it easy to get the latest build results
  - Ensure that the build process is transparent to everyone
  - Automate the deployment

Continuous Delivery (CD) adds the following aspect to the Continuous Integration practices:
Any change passing the tests is immediately ready to be deployed to production, both from a technical and from a quality standpoint.  This means that the most current version of the product is successfully built, tested, and provided in a shippable format. With a press of a button at any time, based on a release decision by the development team or delivery manager, it can be shipped to customers or deployed to production. You can find additional information about CD from the following sources:

> Wikipedia: https://en.wikipedia.org/wiki/Continuous_delivery   
> Martin Fowler: http://martinfowler.com/bliki/ContinuousDelivery.html

By contrast, Continuous *Deployment*, which is not discussed in this document, means that each change is automatically built, tested, and deployed to production without manual interaction.


### Keep Everything Under Version Control

Anyone who has participated in collaborative development, or even worked as a single developer, has been confronted with the necessity of developing and fixing bugs in parallel and is familiar with the difficulties and challenges of doing so. There are two basic principles you can implement to avoid this problem:

  - Define a single source of truth.
  - Place everything under version control.

A source code management system provides the single source of truth for the delivery process. It predicts the outcome of all the results in the CI process including your final product. Everything must be reproducible out of it, and ultimately, it defines the contents of your product, including its quality. Any other system (such as an artifact repository, which is discussed later) is of secondary importance. For example, if you have doubts about the validity of build or test results, always rely on the source code and rebuild the artifacts, and run the tests again from the source code.

The first and most important duty of the build operator is maintaining the health of the source code management system and treating it as the leading system in the landscape. However, simply using a version control system is not enough. The manner in which it is used is also important.

#### The Main Line

The contributions of any developer must converge into a common result, and therefore the definition of a common main line or main integration branch is crucial. Any changes that are not integrated into the unique main line do not become part of the final product.

There are many models for setting this up. In the simplest case, one main line can be sufficient. When a developer starts implementing a feature, he obtains a copy of the current snapshot ("pull") of the main line to a local disk. The developer begins coding once the local version on the PC is up-to-date. When feature coding is finished, the change is integrated ("pushed") into the main line.

In general, the main line has evolved in the meantime, the developer must rebase his change. This means, he must fetch the current version from the main line and merge it into the local changes. As a result, the local change is a successor of the main line's current version. All versioning tools come with features that support this kind of merging in one or another way. The technical details are not of interest here and especially, the high-level term "rebase" used here should not be confused with a Git feature having the same name.

After rebasing, the developer can push the change safely into the current main line, without the risk of introducing any regressions into the current main line. The change then becomes a part of the most current snapshot and can be pulled by other developers.

![Working with the Main Line](principles-1.png)

#### Feature Branches

Imagine the following more complex scenario:

  - A lot of developers, for example, more than 10, are working on the project. 
  - Not all changes are meant to be integrated at once into the main line. It is a common practice of especially big organizations that larger features and prototyping are isolated from the main line first and integrated into it at a later point in time.

In these cases, so-called feature branches, which are created for a given amount of time as children of the main line, are the common solution.

![Working with Feature Branches](principles-2.png)

Developers who work on a dedicated feature are not working directly with the main line, but with a feature branch. Though the mechanism is the same, the developers of the feature are using the feature branch as their integration branch. It becomes interesting when the team decides that a feature is ready to be integrated into the main line. A responsible person must rebase all the changes in the feature branch to the current version of the main line. Similar to the single developer scenario, this means that the current version of the main line is merged into the feature branch to make the current version of the feature branch a successor of the current version of the main line. This can cause a high merging effort. Therefore, you should rebase changes as frequently as possible.

The principle of the fetch - develop - rebase (merge) - push cycle is the same for any level of the branch hierarchy of the development project. Even sub-feature branches that are essentially children of the feature branches can be handled in this way. 

#### Key Learning

- Define your main line.
- Place everything under version control.

### Automate the Build

The stories of developers who complain about broken builds and comments that "it works on my machine" are legendary. The solution is an automated, standardized build procedure with an automatically running "central build". The build result is the only reference to decide whether the project is broken or clean from a build perspective.

For the sake of being reproducible for all team members, the build description must contain, and declare all the dependencies that are needed to execute the build, including the environment in which the build runs. Ideally, calling the build tool can be implemented as a bootstrapping mechanism that guarantees the proper, standardized installation of the build tools and checks their up-to-date state.  In this case, the developer doesn't need to worry about the installation at all, and the risk of build problems caused by improper installations on the developer’s PC is minimized.

A build scheduler such as [Jenkins](https://jenkins.io/) is useful when operating a central build infrastructure:

  - It offers a convenient UI for end users to trigger builds, monitor them, and so on.
  - It provides interfaces that integrate the builds with other components of the CI process.

#### Key Learning

Set up a central, reproducible build.


### Run Tests in the Build

Builds are not restricted to validating the syntactical correctness of the code. There are several kinds of tests:

  - Component (unit) tests
  - Static code checks
  - Scenario tests

The differences between these types of tests are as follows:

  - They operate directly on build results (unit tests) or sources (static code checks), or,
  - they require the newly built application to be installed and started (scenario tests).

In any case, you should ensure that appropriate tests are automatically run during the build, enabling developers to avoid regressions and failures in an early state. Always include automatic unit tests, and also invest in scenario tests and scripts which automatically install, start, and test your application.

Unit test frameworks are available for many source code technologies, most prominently, [JUnit](http://junit.org/) for Java. There are many tools that let you run tests that automatically execute software interactions, and verify the results. There are frameworks, such as [Selenium](http://www.seleniumhq.org/), which can test graphical user interfaces and browser behavior.

A good build process is not restricted to the pure role of syntactical validation of the source code and production of the build artifacts (which could ultimately turn out to be unusable from a functional point of view). The build also acts as a hurdle to ensure the functional correctness of the product (see also "Build Each Change").

Implementing these tests early as part of the build that can be executed on the developer’s workstation saves time that might otherwise be required to address bugs discovered late in the development process.

#### Key Learning

Automate as many tests as possible in the build.


### Commit Early and Often

As stated earlier, in a distributed development project with multiple developers, every developer aims to integrate his or her changes with the main line (the same applies for feature branches and their integration into the main line). We recommend to integrate as often as possible for two reasons:

  - Merging local changes with the main line becomes more and more complicated and time-consuming as the main line evolves.
  - From product quality standpoint, the risk of instability introduced by a new change increases when the distance between the original base of the change and the current main line version has increased.

Every developer, whether working with the main line or with a feature branch, should push at least once per day. This principle seems easier to accomplish with feature branches, but in very active projects you have to balance that with the potentially high effort of merging or constantly rebasing. There is no "golden rule" or “one size fits all” solution.

#### Key Learning

Integrate in small steps. Do not accumulate changes and attempt bulk merges into the main line. 


### Build Each Change

Each change must be integrated into the main line at some point. However, each single change bears the risk of destabilizing the main line, and affecting other developers who rely on its integrity. To balance the conflicting interests, a continuous build mechanism  builds each change to be integrated into the main line. This may be the principle that represents best the basic idea of a CI process.

The typical CI build stores any commit in the main line as soon as it integrated there, and tests the build result automatically. This enables the developer or team to identify whether the change does any harm. To maintain constant monitoring of the quality of the main line, a CI build is mandatory. 

A broken CI build requires immediate action as it indicates a severe problem with the quality of the main line. Developers must fix this as soon as possible, so it can be built again and development can continue. This is potentially rather expensive since, depending on the frequency of commits being entered into the main line, a broken build blocks the entire evolution of the main line. It is much cheaper to avoid this situation in the first place.

It is impossible to avoid broken main lines completely; however, you can avoid most such problems by using voter builds, also called "pending head".

> Martin Fowler: http://martinfowler.com/bliki/PendingHead.html

The developer first proposes a change that is not yet integrated into the main line, using for example a "push for review" in Gerrit or the "pull request" mechanism in GitHub. Builds can be scheduled automatically on the proposed change before it is integrated into the main line. The voter build is measuring the quality of a change, identifying problems before they pollute the main line and require re-work.

Voter builds alone are not sufficient to prevent quality issues in the main line, as source code versions on which voter builds run differ, in general, from the versions on the main line. 

Assume a simple scenario, where changes run into the main line sequentially without overlapping. The figure below shows that one developer creates a change based on version 2 and proposes it. After a successful build, it is merged into the main line as version 3. Another developer may take this new version as a basis for his change, which is finally integrated as version 4 into the main line.

![Change Proposal](principles-3.png)

In this example, the results of voter and CI builds are identical since the merge is trivial. Content-wise, it results in the same version.

But when there are more frequent changes, the chance of overlapping changes increases. A situation as shown in the next figure is rather common, where two changes, both based on version 2, are created in parallel proposals. The "Creation as proposal" on the right side of the figure "wins" in the sense that it is built and merged into the main line as version 3 before the change depicted on the left has the chance to.

In most cases, the left change can be merged into the main line as version 4, applying an automatic optimistic merge strategy, which  applies if the two changes affect completely different code sections. Most source code repositories offer optimistic merge strategies that in the generic case work very well without requiring manual interaction. But it is clear in this case that the voter build and the CI build are operating on different versions.

![Parallel Changes](principles-4.png)

In the end, it is only the CI build that determines the quality of the main line.

An optimistic merge does not always work, namely when the two changes overlap. Such a case, the developer who authors the change shown on the left must first rebase his or her change to version 3. Doing this ensures that the change integrates into the main line as version 4.

![Parallel Changes with merge](principles-6.png)

What are the technical requirements that ensure that the voter and CI builds are reliable? These builds must run on a separate build machine which is set up in a reproducible way and managed by a build operator. By contrast, builds on a developer's workstation will not be reproducible, as they are unlikely to use standard and consistent setups.

In a more complex development landscape that uses feature branches in addition to the main line, we recommend that you set up central CI and voter builds in the same way as for the main line. Not implementing automatic builds and tests on the feature branches leads to a much higher integration effort to push from the feature branch into the main line. Apply the principle "fail early and often".

#### Key Learning

  - Keep the main line clean.
  - Build and automatically test each change that flows into it.
  - Implement CI and voter builds.


### Fix Build Errors Immediately

The "build each change" principle obliges developers to immediately react on build errors. This is quite obvious in a monitor build - which is not recommended - because a change affects the entire team. In the worst case, developers cannot continue working. If voter builds are applied, the risk of spoiling the main line is minimized, so the build errors by one developer do not affect other developers. Nevertheless, developers who break their own builds are urged to fix them immediately. Otherwise, any further build by the developer fails and problems accumulate from change to change.

Combining these two principles guides the developer's workflow toward "change - commit - build - fix".

#### Key Learning

Fix build errors as soon as they are identified; do not accumulate them.


### Keep the Build Fast

The build and test steps that are executed automatically after proposing a change make up a central part of the developer’s workflow.
He or she should not have to wait too long for CI build and test results; therefore, good performance of CI builds, including tests, is of vital importance.

There is always a conflict of interest: You want to integrate new changes into the main line as securely as possible. At the same time, building, validating, and especially scenario testing can be quite time consuming, thus conflicting with the desire for a fast build.

For example, a 15 minute wait time might be acceptable for small projects. As the project grows larger, and the amount of time that build and tests take increases, acceptance decreases.

Therefore, you must find a compromise between different levels of acceptance criteria:

  - Those that are considered as crucial for the quality of the main line. Each change merged must fulfill them.
    This includes of course the syntactical correctness, but also unit tests and fast-running scenario tests. They have to run in the CI builds and maybe even in the voter builds.
  - More exhaustive scenario tests that take longer than what would be acceptable for a CI build.
    For running these tests, additional scheduled builds could run independently from the CI builds. They do not block the developer, since their processing time does not play such an important role.
    The frequency of the scheduled builds depends on how often the current snapshot of the main line should be intensively tested. Even a test of every single change that is integrated into the main line would be possible. Scheduled builds for subsequent changes may eventually overlap in time, hence they require enough machine capacity.

![Testing](principles-5.png)

For scheduled builds, we recommend that you reuse the build results from the most recent, successful CI build instead of doing the same build again.

#### Key Learning

Balance between build performance and the number of tests that are required to be run inside the CI build. 

### Test in a Clone of the Production Environment

Different setups and different environments at different stages of a production pipeline are often cause of errors. What is obviously true with respect to the relationship between local builds on the developer’s PC on the one hand, and central builds on the other hand, cannot be omitted with respect to the relationship between test machines and productive landscape. Make sure that the machines on which scenario tests are executed are comparable to the final production landscape.

The challenge lies in achieving this. When using traditional physical hardware with manually installed and configured software, it is almost impossible to avoid divergence: across time, production and test machines, or any two different environments, diverge.

The solution to this problem is to treat the setup and configuration in the same way as writing code (this principle is sometimes referred to as DevOps). Any software and configuration that is installed on a machine is described in source files, which enables you to re-create the machine set up at any time. Use automation tools, for example, Chef, Puppet, Salt, Ansible, and so on. Configuration changes are made by applying a change to those scripts. This enables you to roll out changes in a controlled and reproducible way. This approach is especially well suited when working with virtual hardware. Setting up new virtual machines, providing them with the software and putting them into production can be completely automated by pressing a single button.

Rather than install software directly on the hardware (be it physical or virtual), we recommend that you run the applications inside containers (like Docker) that are instantiated from predefined images.

The key principle is that the setup of the machine is always reproducible. Make sure you can set up a dedicated machine again in exactly the same way by simply pressing a button.

#### Key Learning

Use the infrastructure-as-code approach to set up your landscape in a reproducible way.


### Make it Easy to Get the Latest Build Results

In addition to automatic build and testing, the CI infrastructure needs to support the need to do some manual test or experiments with the newly built application. The build infrastructure itself, that is, the build servers, should, for security reasons and to minimize the risk of spoiling build results, run without being disturbed by any unauthorized access. However, this also means that developers who have a valid interest in getting the latest build result cannot  be allowed to download it directly from the build server.

Therefore, you must also maintain a central location, from which developers can get the newest versions (and also older versions) of the artifacts. This location should fulfill the following criteria:

  - Developers can easily find what they are looking for.
  - Only official build results are kept there. Unauthorized changes on the artifacts (for example, manual replacement of build results) are not allowed, under any circumstance.

The approach "just store the artifacts to a central share" can cause problems when:

  - More than one application has to be stored.
  - More than one version has to be kept.
  - The application has to be built for more than one platform.
  - More than one build variant is needed, and so on.  
  
A central repository for artifacts is the appropriate solution for storing and providing build results for developers as well as for other processes that may rely on artifacts. There are some products offered on the market. In the Java world, Nexus is one of the most common ones.

#### Key Learning

Define a unique and well-organized place for storing build results. Use an artifact repository manager.


### Ensure that the Build Process is Transparent to Everyone

The status of the build process, and therefore the quality of the product, must be visible for everybody. For a developer, the following questions are of interest:

  - Was my last (or any) change built?
  - Was it tested?
  - What were the build and test results and was my change merged into the main line?
  
The same questions apply to a change of another developer that I am relying on.

After a change was committed by a developer, it is processed in the CI process chain. Different tools are involved: the source code management tool, the build scheduler, the test landscape, the artifact repository and maybe some others. At any point in time, a change is in a dedicated state (for example, pushed, waiting for build, built with success/error, being tested, and so on) and might be processed by one of the listed components. Everybody must be able to easily get the status information on every single change.

A central entry point to access the information is key. The provisioning could be done by some kind of orchestration tool that could cover both: the control of the complete process pipeline and the transparency of the process state to the developers. In the sample cases described in the later sections, we will see that a change review tool (for example Gerrit) will play this role in tight collaboration with a build scheduler (for example Jenkins). Gerrit acts as entry point for the changes, it can be accessed by anybody and any tool playing a role is able to write back a notice such that the overall status is always reproducible there. Another approach is to use Jenkins with the Flow plugin which offers to developers and build engineers an end-to-end overview of the CI pipeline.

#### Key Learning

Implement a single entry point to retrieve status information.


### Automate the Deployment

Deployment, that means the installation of the application to a runtime system, should be done in an automated way. This can be achieved in several ways and for different purposes:

  - Automated scenario tests that already run in the build, or as a separate process that can be triggered by or after the build (which does not make any difference from a semantic point of view). The intention is that the scenario tests that must connect to a running application are part of the acceptance process for a change proposed or committed by the developer. Different implementations are possible.
  - A single runtime system (like a JEE server) that is waiting for deployments can be sufficient in the beginning, but this approach has some drawbacks. Since it is a single instance, the test requests out of the CI process would have to be serialized to avoid clashes from two parallel test requests. A high amount of changes will make this instance a bottleneck.
    Additionally, reusing the same instance again and again bears the risk of degeneration of the system with time. A better approach is to install and start the runtime system on demand during the build. This could be done on the temporary file system that is dedicated to the single test execution instance, or using container technology (like Docker) to bring up the runtime system on demand in a well-defined state and to remove the container after use.
  - Provisioning of a test system for manual tests: In a CI build scenario, this might not be appropriate since the frequency and time of the validation of new commits is not really predictable. It makes much more sense to provision manual test systems from a scheduled build that, for example, runs daily at a given time, deploys to the test system, and notifies the tester after successful deployment. What concerns the implementation of runtime system, all what was said before applies here as well though the risk of bottlenecks and collision of different tests is not given here. So one single runtime instance dedicated for this purpose could do the job in a satisfying way. This approach is also capable of supporting a staged environment (several test instances of different stages, that means, quality levels), as desired.
  - The ultimate level of automation is the automated deployment into the productive environment. As a post-step of the CI build, you should only do this if the risk of breaking something is minimized. This approach applies only when manual tests are not needed any more and every test is already done automatically.

Therefore, it is more common to trigger the deployment to production manually after a conscious decision to release. Nevertheless, an automated framework should support this manual step as much as possible. For example by just pressing a "release" button, the process framework should automatically start the deployment job to the productive system.

Keep in mind, Continuous Delivery doesn't require every change to be deployed to production automatically. Continuous Delivery means that any change integrated is validated such that it is ready to be deployed to production.

#### Key Learning

Do deployment as automatic post-build step. Also deploy automatically to production systems, but manually triggered.


## Next Steps
 
  - [Pipeline Suggestions](http://go.sap.com/developer/tutorials/ci-best-practices-pipelines.html)
  - [Back to the Navigator](http://go.sap.com/developer/tutorials/ci-best-practices-intro.html)
