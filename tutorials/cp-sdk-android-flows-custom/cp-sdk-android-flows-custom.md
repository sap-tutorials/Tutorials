---
author_name: Haoyue Feng
author_profile: https://github.com/Fenghaoyue
title: Create Your Own Flow
description: Learn how to create your own flow with the framework provided by the Flows component of SAP BTP SDK for Android.
auto_validation: true
time: 60
tags: [ tutorial>intermediate, operating-system>android, topic>mobile, products>sap-business-technology-platform]
primary_tag: products>sap-btp-sdk-for-android
---

## Prerequisites
- You completed [Get Familiar with the Flows Component by a Wizard Generated Application](cp-sdk-android-flows-wizard).
- You completed [Customize the Onboarding Flow](cp-sdk-android-flows-onboarding).
- You completed [Handle Passcode with the Flows Component](cp-sdk-android-flows-passcode).
- You completed [Restore and Reset Applications Using the Flows Component](cp-sdk-android-flows-restore).

## Details
### You will learn
  - How to write a flow step
  - How to write your own flow

Besides the pre-defined flows described in the previous tutorials, the Flows Component also provides a framework for client code to create its own flow. Typically a flow consists of several flow steps and also may include other flows as sub-flows. The Flows framework provides flexible ways for client code to create a customized flow:

- The client code can create its own flow steps and then create a flow with the combination of the steps.
- The client code can create a flow which only includes the pre-defined flow steps or pre-defined flows as sub-flows without defining its own flow steps.
- The client code can combine its own flow steps, pre-defined flow steps, its own flows and the pre-defined flows to create a customized flow.

---

[ACCORDION-BEGIN [Step 1: ](Write your own flow step)]

[OPTION BEGIN [Java]]

1.  A flow step must extend from the parent class **`com.sap.cloud.mobile.flowv2.core.FlowStepFragment`**.

2.  A flow step can override the **`onCreateView`** function to create UI for the step.

3.  The parent class provides one important helper function for the flow step to notify the framework that it's done, so the flow framework can move to next step or finish the flow.

    **`protected fun stepDone(currentStepId: Int, popCurrent: Boolean = false, vararg args: Pair<String, String>)`**

4.  The following code snippet creates a simple flow step. First we create a layout XML file named **`fragment_step1`** for the UI part which includes a text view and a button.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <androidx.constraintlayout.widget.ConstraintLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:id="@+id/fragmentStepOne"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:context=".steps.CustomStepOne">

        <TextView
            android:id="@+id/txtStepOne"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            app:layout_constraintBottom_toBottomOf="parent"
            app:layout_constraintEnd_toEndOf="parent"
            app:layout_constraintStart_toStartOf="parent"
            app:layout_constraintTop_toTopOf="parent" />

        <Button
            android:id="@+id/btnOKStepOne"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_marginTop="24dp"
            android:text="@android:string/ok"
            app:layout_constraintEnd_toEndOf="parent"
            app:layout_constraintStart_toStartOf="parent"
            app:layout_constraintTop_toBottomOf="@+id/txtStepOne" />
    </androidx.constraintlayout.widget.ConstraintLayout>
    ```

    Then we can create a flow step with the layout created above. In this flow step, we set a step name, create view with the layout file **`fragment_step1`** and add button click listener to notify step done.

    ```Java
    public class CustomStepOne extends FlowStepFragment {
        @NotNull
        @Override
        public String getStepName() {
            return "Step 1";
        }

        @Nullable
        @Override
        public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container,
                @Nullable Bundle savedInstanceState) {
            return inflater.inflate(R.layout.fragment_step1, container, false);
        }

        @Override
        public void onActivityCreated(@org.jetbrains.annotations.Nullable Bundle savedInstanceState) {
            super.onActivityCreated(savedInstanceState);
            TextView txtStepOne = this.getView().findViewById(R.id.txtStepOne);
            txtStepOne.setText(getStepName());
            Button btnStepOne = this.getView().findViewById(R.id.btnOKStepOne);
            btnStepOne.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    stepDone(R.id.fragmentStepOne);
                }
            });
        }
    }
    ```

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  A flow step must extend from the parent class **`com.sap.cloud.mobile.flowv2.core.FlowStepFragment`**.

2.  A flow step can override the **`onCreateView`** function to create UI for the step.

3.  The parent class provides one important helper function for the flow step to notify the framework that it's done, so the flow framework can move to next step or finish the flow.

    **`protected fun stepDone(currentStepId: Int, popCurrent: Boolean = false, vararg args: Pair<String, String>)`**

4.  The following code snippet creates a simple flow step. First we create a layout XML file named **`fragment_step1`** for the UI part which includes a text view and a button.

    ```XML
    <?xml version="1.0" encoding="utf-8"?>
    <androidx.constraintlayout.widget.ConstraintLayout xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:id="@+id/fragmentStepOne"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        tools:context=".steps.CustomStepOne">

        <TextView
            android:id="@+id/txtStepOne"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            app:layout_constraintBottom_toBottomOf="parent"
            app:layout_constraintEnd_toEndOf="parent"
            app:layout_constraintStart_toStartOf="parent"
            app:layout_constraintTop_toTopOf="parent" />

        <Button
            android:id="@+id/btnOKStepOne"
            android:layout_width="wrap_content"
            android:layout_height="wrap_content"
            android:layout_marginTop="24dp"
            android:text="@android:string/ok"
            app:layout_constraintEnd_toEndOf="parent"
            app:layout_constraintStart_toStartOf="parent"
            app:layout_constraintTop_toBottomOf="@+id/txtStepOne" />
    </androidx.constraintlayout.widget.ConstraintLayout>
    ```

    Then we can create a flow step with the layout created above. In this flow step, we set a step name, create view with the layout file **`fragment_step1`** and add button click listener to notify step done.

    ```Kotlin
    class CustomStepOne : FlowStepFragment() {

        override fun getStepName(): String {
            return "Step 1"
        }

        override fun onCreateView(
            inflater: LayoutInflater, container: ViewGroup?,
            savedInstanceState: Bundle?
        ): View? {
            return inflater.inflate(R.layout.fragment_step1, container, false)
        }

        override fun onActivityCreated(savedInstanceState: Bundle?) {
            super.onActivityCreated(savedInstanceState)
            txtStepOne.text = getStepName()
            btnOKStepOne.setOnClickListener { _ ->
                stepDone(R.id.fragmentStepOne)
            }
        }
    }
    ```

[OPTION END]

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 2: ](Write your own flow)]

Your own flow class must extend the **`com.sap.cloud.mobile.flowv2.core.Flow`** class, and override some of the methods defined in it.

[OPTION BEGIN [Java]]

1.  Following method allows client code to add a flow step:

    **`protected fun <T : FlowStepFragment> addSingleStep(stepId: Int, fragmentClass: KClass<T>)`**

    You can add both pre-defined flow step and customized flow step to your flow with this method. Following is the sample code for a simple flow with a pre-defined EULA step and a customized step **`CustomStepOne`** we created in the first section.

    ```Java
    public class TestFlow extends Flow {
        public TestFlow(@NotNull Application application) {
            super(application);
            addSingleStep(R.id.stepEula, JvmClassMappingKt.getKotlinClass(EulaFragment.class));
            addSingleStep(R.id.fragmentStepOne, JvmClassMappingKt.getKotlinClass(CustomStepOne.class));
        }
    }
    ```

    Now you have created your first flow. Start the flow with the code below, and you can see the EULA screen and then the screen for "Step 1".

    ```Java
       Flow testFlow = new TestFlow(application);
       FlowContext flowContext = new FlowContextBuilder()
               .setFlow(testFlow)
               .build();
       Flow.start(activity, flowContext);
    ```

2.  A flow which only contains sequential steps cannot always meet your requirements. There may be some branching scenarios to navigate to different steps based on a certain condition. Following method controls the step navigation in a flow and client code can override this method to add its own logic:

    **`open suspend fun getNextStep(currentStepId: Int, businessData: BusinessDataMap): Pair<Int, KClass<*>?>?`**

    Now copy the layout XML file and the flow step class in the first section to create two more steps named **`CustomStepTwo`** and **`CustomStepThree`**. Just rename the id in the files to avoid duplication error and set the step name to "Step 2" and "Step 3".

    Then create a **`ConditionFlow`** class to add all the three customized flow steps and override the **`getNextStep`** method.

    ```Java
    public class ConditionFlow extends Flow {
        public static final String WHICH_STEP_KEY = "which.step";

        public ConditionFlow(@NotNull Application application) {
            super(application);
            addSingleStep(R.id.fragmentStepOne, JvmClassMappingKt.getKotlinClass(CustomStepOne.class));
            addSingleStep(R.id.fragmentStepTwo, JvmClassMappingKt.getKotlinClass(CustomStepTwo.class));
            addSingleStep(R.id.fragmentStepThree, JvmClassMappingKt.getKotlinClass(CustomStepThree.class));
        }

        @Nullable
        @Override
        public Object getNextStep(int currentStepId, @NotNull BusinessDataMap businessDataMap,
                @NotNull Continuation<? super Pair<Integer, ? extends KClass<?>>> $completion) {
            int whichStep = this.getStepNum(businessDataMap);
            if (currentStepId == R.id.fragmentStepOne) {
                if (whichStep == 1) {
                    return new Pair(R.id.fragmentStepThree,
                            JvmClassMappingKt.getKotlinClass(CustomStepThree.class));
                } else {
                   return new Pair(R.id.fragmentStepTwo,
                            JvmClassMappingKt.getKotlinClass(CustomStepTwo.class));
                }
            } else if (currentStepId == R.id.fragmentStepTwo || currentStepId == R.id.fragmentStepThree) {
                return new Pair(FlowConstants.FLOW_STATUS_END, null);
            }
            return super.getNextStep(currentStepId, businessDataMap, $completion);
        }

        private int getStepNum(BusinessDataMap businessDataMap) {
            Object whichStep = businessDataMap.getBusinessData(WHICH_STEP_KEY);
            if(whichStep == null) {
                return 0;
            } else {
                return  ((Integer) whichStep).intValue();
            }
        }
    }
    ```

    The navigation for this flow is when current step is "Step 1", the client code will check the data saved in the **`BusinessDataMap`** instance and navigate to "Step 3" if the value for the key **`WHICH_STEP_KEY`** is explicitly set to 1, otherwise navigate to "Step 2". And when current step is "Step 2" or "Step 3", current step is the last step of the flow. For other cases, the navigation logic is from the parent class.

    Now make some modification to "Step 1" to make the **`ConditionFlow`** work. First add one more button "Step 3" to the layout XML file.

    ```XML
    <Button
        android:id="@+id/btnConditionStepTest"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_marginTop="16dp"
        android:text="Step 3"
        app:layout_constraintEnd_toEndOf="parent"
        app:layout_constraintStart_toStartOf="parent"
        app:layout_constraintTop_toBottomOf="@+id/btnOKStepOne" />
    ```

    Then add button click logic to save value 1 for the key **`WHICH_STEP_KEY`** to the **`BusinessDataMap`** instance, so when this new button is clicked, the flow will navigate to "Step 3", otherwise navigate to "Step 2".

    ```Java
    Button btnStepTest = this.getView().findViewById(R.id.btnConditionStepTest);
    btnStepTest.setOnClickListener(new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            getFlowViewModel().getBusinessData().saveBusinessData(ConditionFlow.WHICH_STEP_KEY, 1, true);
            stepDone(R.id.fragmentStepOne);
        }
    });
    ```

    Now start the flow with following code. The first screen of the flow is "Step 1" which contains two buttons. If the OK button is clicked, the screen "Step 2" is displayed and if the "Step 3" button is clicked, the screen "Step 3" is displayed. Then click the button on "Step 2" or "Step 3", the flow is finished.

    ```Java
    Flow testFlow = new ConditionFlow(application);
    FlowContext flowContext = new FlowContextBuilder()
             .setFlow(testFlow)
             .build();
    Flow.start(activity, flowContext);
    ```

3.  As mentioned in the beginning, besides the single flow step, a flow can also include sub-flows. Following method allows client code to add a sub-flow.

    **`protected fun addNestedFlow(nestedFlow: Flow)`**

    The following sample code creates a flow with a pre-defined flow step and a sub-flow.

    ```Java
    public class NestedFlow extends Flow {
        public NestedFlow(@NotNull Application application) {
            super(application);
            addSingleStep(R.id.stepEula, JvmClassMappingKt.getKotlinClass(EulaFragment.class));
            addNestedFlow(new ConditionFlow(application));
        }
    }
    ```

4.  Client code can insert logic at different stage of the flow by overriding the following methods:

    **`protected open suspend fun onStart(businessData: BusinessDataMap)`**

    **`open suspend fun onFinish(businessData: BusinessDataMap)`**

    **`open suspend fun onTerminate(businessData: BusinessDataMap)`**

    **`open fun getInitialData(): Map<String, Any>?`**

    See [Write Your Own Flow](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/onboarding/android/newflows/WriteYourOwnFlow.html) in the help documentation for detailed explanation of the methods.

[OPTION END]

[OPTION BEGIN [Kotlin]]

1.  Following method allows client code to add a flow step:

    **`protected fun <T : FlowStepFragment> addSingleStep(stepId: Int, fragmentClass: KClass<T>)`**

    You can add both pre-defined flow step and customized flow step to your flow with this method. Following is the sample code for a simple flow with a pre-defined EULA step and a customized step **`CustomStepOne`** we created in the first section.

    ```Kotlin
    open class TestFlow(private val application: Application) : Flow(application) {
        init {
            addSingleStep(R.id.stepEula, EulaFragment::class)
            addSingleStep(R.id.fragmentStepOne, CustomStepOne::class)
        }
    }
    ```

    Now you have created your first flow. Start the flow with the code below, and you can see the EULA screen and then the screen for "Step 1".

    ```Kotlin
    val testFlow = TestFlow(application)
    val flowContext = FlowContext(flow = testFlow)
    Flow.start(activity, flowContext)
    ```

2.  A flow which only contains sequential steps cannot always meet your requirements. There may be some branching scenarios to navigate to different steps based on a certain condition. Following method controls the step navigation in a flow and client code can override this method to add its own logic:

    **`open suspend fun getNextStep(currentStepId: Int, businessData: BusinessDataMap): Pair<Int, KClass<*>?>?`**

    Now copy the layout XML file and the flow step class in the first section to create two more steps named **`CustomStepTwo`** and **`CustomStepThree`**. Just rename the id in the files to avoid duplication error and set the step name to "Step 2" and "Step 3".

    Then create a **`ConditionFlow`** class to add all the three customized flow steps and override the **`getNextStep`** method.

    ```Kotlin
    class ConditionFlow(private val application: Application) : Flow(application) {
        init {
            addSingleStep(R.id.fragmentStepOne, CustomStepOne::class)
            addSingleStep(R.id.fragmentStepTwo, CustomStepTwo::class)
            addSingleStep(R.id.fragmentStepThree, CustomStepThree::class)
        }

        override suspend fun getNextStep(
            currentStepId: Int,
            businessData: BusinessDataMap
        ): Pair<Int, KClass<*>?>? {
            val whichStep = businessData.getBusinessData(WHICH_STEP_KEY) ?: 0
            return when (currentStepId) {
                R.id.fragmentStepOne ->
                    return if (whichStep == 1) Pair(
                        R.id.fragmentStepThree,
                        CustomStepThree::class
                    ) else Pair(R.id.fragmentStepTwo, CustomStepTwo::class)
                in listOf(
                    R.id.fragmentStepTwo,
                    R.id.fragmentStepThree
                ) -> Pair(FlowConstants.FLOW_STATUS_END, null)
                else -> super.getNextStep(currentStepId, businessData)
            }
        }

        companion object {
            const val WHICH_STEP_KEY = "which.step"
        }
    }
    ```

    The navigation for this flow is when current step is "Step 1", the client code will check the data saved in the **`BusinessDataMap`** instance and navigate to "Step 3" if the value for the key **`WHICH_STEP_KEY`** is explicitly set to 1, otherwise navigate to "Step 2". And when current step is "Step 2" or "Step 3", current step is the last step of the flow. For other cases, the navigation logic is from the parent class.

    Now make some modification to "Step 1" to make the **`ConditionFlow`** work. First add one more button "Step 3" to the layout XML file.

    ```XML
    <Button
        android:id="@+id/btnConditionStepTest"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_marginTop="16dp"
        android:text="Step 3"
        app:layout_constraintEnd_toEndOf="parent"
        app:layout_constraintStart_toStartOf="parent"
        app:layout_constraintTop_toBottomOf="@+id/btnOKStepOne" />
    ```

    Then add button click logic to save value 1 for the key **`WHICH_STEP_KEY`** to the **`BusinessDataMap`** instance, so when this new button is clicked, the flow will navigate to "Step 3", otherwise navigate to "Step 2".

    ```Kotlin
    btnConditionStepTest.setOnClickListener { _ ->
        flowViewModel.businessData.saveBusinessData(ConditionFlow.WHICH_STEP_KEY, 1)
        stepDone(R.id.fragmentStepOne)
    }
    ```

    Now start the flow with following code. The first screen of the flow is "Step 1" which contains two buttons. If the OK button is clicked, the screen "Step 2" is displayed and if the "Step 3" button is clicked, the screen "Step 3" is displayed. Then click the button on "Step 2" or "Step 3", the flow is finished.

    ```Kotlin
    val testFlow = ConditionFlow(application)
    val flowContext = FlowContext(flow = testFlow)
    Flow.start(activity, flowContext)
    ```

3.  As mentioned in the beginning, besides the single flow step, a flow can also include sub-flows. Following method allows client code to add a sub-flow.

    **`protected fun addNestedFlow(nestedFlow: Flow)`**

    The following sample code creates a flow with a pre-defined flow step and a sub-flow.

    ```Kotlin
    class NestedFlow(private val application: Application) : Flow(application) {
        init {
            addSingleStep(R.id.stepEula, EulaFragment::class)
            addNestedFlow(ConditionFlow(application))
        }
    }
    ```

4.  Client code can insert logic at different stage of the flow by overriding the following methods:

    **`protected open suspend fun onStart(businessData: BusinessDataMap)`**

    **`open suspend fun onFinish(businessData: BusinessDataMap)`**

    **`open suspend fun onTerminate(businessData: BusinessDataMap)`**

    **`open fun getInitialData(): Map<String, Any>?`**

    See [Write Your Own Flow](https://help.sap.com/doc/f53c64b93e5140918d676b927a3cd65b/Cloud/en-US/docs-en/guides/features/onboarding/android/newflows/WriteYourOwnFlow.html) in the help documentation for detailed explanation of the methods.

[OPTION END]

Congratulations! You now have learned how to create your own flow using the Flows component!

[VALIDATE_1]
[ACCORDION-END]

---
