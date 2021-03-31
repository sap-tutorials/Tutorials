---
title: Get an Understanding of CoreML
description: Understand what CoreML is and how it works.
auto_validation: true
author_name: Kevin Muessig
author_profile: https://github.com/KevinMuessig
primary_tag: products>ios-sdk-for-sap-btp
tags: [  tutorial>intermediate, operating-system>ios, topic>mobile, topic>odata, products>sap-business-technology-platform, products>sap-mobile-services ]
time: 5
---

## Prerequisites  

- **Development environment:** Apple Mac running macOS Catalina or higher with Xcode 11 or higher
- **SAP BTP SDK for iOS:** Version 5.0 or newer

## Details

### You will learn  

- What Core ML is
- Why it is great for mobile apps

---

[ACCORDION-BEGIN [Step 1: ](Introduction to machine learning)]

Machine Learning got more and more popularity over the last view years. With help of Machine Learning, Software Developers can achieve great things for all sorts of use cases. You can use it for predictions, combined with Internet of Things you can use it in Manufacturing and other industrial areas, research groups use it in the medical field or it's used by the Game Industry. Whatever the use case might be, the question you might have is "How can I use it in a mobile context?". This tutorial will go a little bit into the details of how you can use Machine Learning on an iOS device.

So why is Machine Learning on Mobile so great?

Using Machine Learning inside your mobile iOS app can bring your user experience to the next level. It will allow you to recognize gestures, speech, text and images. This will make your app smarter and allows the user to have a totally different way to interact with other people or objects in their daily private or work life. You can combine it with other technologies like Augmented Reality to build amazing experiences.

Machine Learning is simple the use of data to give answers to questions you have: Is that a human or not? ; If my industrial machine is running under certain environmental circumstances, what is the possibility for it to break? ; Is that noise to loud and dangerous for a human ear?

Those are just examples and the possibilities are endless. In the next step, you will get an introduction to Core ML, Apple's own Machine Learning framework.

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](What is Core ML)]

When it comes to Machine Learning, Apple established a Framework called Core ML back in 2017 for performing predictions and inference using pre-trained machine learning models. The beauty about Core ML is, that it is on-device which means even if your device is offline, your Machine Learning model will still be able to fulfill it's job.

Before you dive more into the capabilities of Core ML let's see how you as an iOS developer can access Machine Learning models. In general there are three different ways for your app to get an answer from a Machine Learning model:

1. Host your own Machine Learning model on the cloud. Send data from your app to an endpoint and retrieve the answer in a HTTP response.
2. Use available APIs hosted for example by Cloud Companies like SAP. With SAP you could use API Hub or the Machine Learning capabilities provided by SAP BTP to access all sorts of different Machine Learning services.
3. Use an offline Machine Learning model on device with help of Core ML. This will allow you to get predictions and inference right on device without having to have a roundtrip to the cloud and back.

Core ML is not only available for iOS but also for all the other great operating systems (macOS, iOS, watchOS, tvOS). You can use it to make life predictions on photos and video and soon with Core ML 3 even Sound.

What benefits do you get by using Machine Learning on device? - If you're using Core ML and import pre-trained Machine Learning models into your app you can spare the Request-Response Roundtrip to the Cloud, which gives you low-latency and Real-Time predictions. It also gives you a great deal of user privacy because the user's data will never leave the device at any time. Your on-device model will also never stop learning and gets smarter and smarter the more the user uses it. It will also learn only on the specific users data which allows it to be suited for each and every user independently.

You can use different tools to train your model, there is a whole market out there for you to use. As an SAP developer using SAP owned machine learning platform would be the recommended way to train your model, it gives you a whole lot of pre-trained models already and also makes it easy for you to train it directly in the cloud and export it to the Core ML format to use it on your Apple device.

Using Apple's Vision framework and NLP (Natural Language Processing) API together with Core ML is a way to go for you as an iOS developer. The Vision framework allows you to perform face, landmark, text, barcode recognition and detection as also image registration, and feature tracking. Use the Vision framework to pipeline those information from your mobile device to your Core ML model to solve your computer vision problems.

Using NLP allows your app to understand text written and printed using features like detecting names and language identification. If you have used the Concur app on your mobile device and scanned a receipt using the OCR feature you get a great example on how you can utilize this great technology.

[VALIDATE_2]
[ACCORDION-END]
