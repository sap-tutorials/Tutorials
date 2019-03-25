---
title: Create additional database artifacts (Forecast App)
description: Create a series of SQL Views that will be used in a SAPUI5 application
auto_validation: true
primary_tag: topic>machine-learning
tags: [ tutorial>beginner, products>sap-hana\, express-edition, topic>machine-learning ]
time: 20
---

## Prerequisites
 - [Use Machine Learning to Build a Forecasting application using the XS advanced development model](https://developers.sap.com/group.hxe-aa-forecast.html)

## Details
### You will learn
- Create a HDB SQL View Artifact
- Create a HDB Procedure Artifact


[ACCORDION-BEGIN [Step 1: ](Open the Web IDE)]

Open the Web IDE, and login using the **`XSA_DEV`** credentials.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

![Web IDE](01-01.png)

As a reminder the default URL for the Web IDE is:

 - `https://hxehost:53075`

A link to the Web IDE can also be found on the ***XSA Controller page*** at:

- `https://hxehost:39030`

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the APL procedures)]

In order to expose in your application the ability to execute the APL Forecast algorithm and get the results, you will need create a series of stored procedure, that you will later expose as XSJS services.

In this procedure, you will expose a series of parameters from the APL function:

- Horizon
- Last Training Time Point
- Cutting Strategy
- Forecast Method
- Forecast Fallback Method
- Smoothing Cycle Length
- Force Positive Forecast
- Forecast Max Cycles
- Forecast Max Lags

The complete description of these parameters is available in the [`FORECAST` function](https://help.sap.com/viewer/4055990955524bb2bc61ee75de3b08ff/latest/en-US/34ec8a14b4a442a29351bb26bb082e6e.html) documentation.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

Right click on the **`apl`** folder (from **`forecast/db/src/hdb`** ) and select **New > Folder**.

Enter **`procedures`** as the folder name, then click on **OK**.

#### The execution procedure

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`forecast.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/db/src/hdb/apl/procedures/forecast.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.forecast.db.hdb.apl.procedures::forecast" (
    in DatasetName            varchar(100),
    in Horizon                integer default 1,
    in LastTrainingTimePoint  date,
    in CuttingStrategy        varchar(100) default 'sequential',
    in ForecastMethod         varchar(100) default 'Default',
    in ForecastFallbackMethod varchar(100) default 'ExponentialSmoothing',
    in SmoothingCycleLength   integer      default NULL,
    in ForcePositiveForecast  varchar(100) default 'false',
    in ForecastMaxCyclics     integer      default 450,
    in ForecastMaxLags        integer      default NULL,
    out operation_config     "aa.forecast.db.hdb.apl::forecast.tt_operation_config",
    out results              "aa.forecast.db.hdb.apl::forecast.tt_results",
    out operation_log        "aa.forecast.db.hdb.apl::forecast.tt_operation_log",
    out summary              "aa.forecast.db.hdb.apl::forecast.tt_summary",
    out indicators           "aa.forecast.db.hdb.apl::forecast.tt_indicators"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    declare FunctionName varchar(255) := 'forecast';
    --declare cursor coldesc for select position, column_name from sys.table_columns where table_name = 'CashFlows_extrapredictors';

    create local temporary table #forecast_function_header  like "aa.forecast.db.hdb.apl::forecast.tt_function_header";
    create local temporary table #forecast_operation_config like "aa.forecast.db.hdb.apl::forecast.tt_operation_config";
    create local temporary table #forecast_variable_descs   like "aa.forecast.db.hdb.apl::forecast.tt_variable_descs";
    create local temporary table #forecast_variable_roles   like "aa.forecast.db.hdb.apl::forecast.tt_variable_roles";

    create local temporary table #forecast_operation_log    like "aa.forecast.db.hdb.apl::forecast.tt_operation_log";
    create local temporary table #forecast_summary          like "aa.forecast.db.hdb.apl::forecast.tt_summary";
    create local temporary table #forecast_indicators       like "aa.forecast.db.hdb.apl::forecast.tt_indicators";
    create local temporary table #forecast_results          like "aa.forecast.db.hdb.apl::forecast.tt_results";

    -- Insert operation parameters
    insert into #forecast_function_header values ('Oid', '#42');
    insert into #forecast_function_header values ('LogLevel', '8');

    if :Horizon                 is not null then insert into #forecast_operation_config values ('APL/Horizon'                 , cast(:Horizon                 as varchar), NULL); end if;
    if :LastTrainingTimePoint   is not null then insert into #forecast_operation_config values ('APL/LastTrainingTimePoint'   , cast(:LastTrainingTimePoint   as varchar), NULL); end if;
    if :ForecastMethod          is not null then insert into #forecast_operation_config values ('APL/ForecastMethod'          , cast(:ForecastMethod          as varchar), NULL); end if;
    if :ForecastFallbackMethod  is not null then insert into #forecast_operation_config values ('APL/ForecastFallbackMethod'  , cast(:ForecastFallbackMethod  as varchar), NULL); end if;
    if :SmoothingCycleLength    is not null then insert into #forecast_operation_config values ('APL/SmoothingCycleLength'    , cast(:SmoothingCycleLength    as varchar), NULL); end if;
    if :ForcePositiveForecast   is not null then insert into #forecast_operation_config values ('APL/ForcePositiveForecast'   , cast(:ForcePositiveForecast   as varchar), NULL); end if;
    if :ForecastMaxCyclics      is not null then insert into #forecast_operation_config values ('APL/ForecastMaxCyclics'      , cast(:ForecastMaxCyclics      as varchar), NULL); end if;
    if :ForecastMaxLags         is not null then insert into #forecast_operation_config values ('APL/ForecastMaxLags'         , cast(:ForecastMaxLags         as varchar), NULL); end if;

    insert into #forecast_operation_config values ('APL/TimePointColumnName'   , 'signal_time'              , null);
    insert into #forecast_operation_config values ('APL/ApplyExtraMode'        , 'Forecasts and Error Bars' , null);

    if :DatasetName like '%_extrapredictors'
    then
        FunctionName := FunctionName || '_' || DatasetName;
        exec 'insert into #forecast_variable_descs select * from "aa.forecast.db.data::KxDesc_' || :DatasetName || '";';
    end if;

    insert into #forecast_variable_descs values (0, 'signal_time'  , 'date'     , 'continuous', 1, 1, null, null, null, null);
    insert into #forecast_variable_descs values (1, 'signal_value' , 'number'   , 'continuous', 0, 0, null, null, null, null);

    insert into  #forecast_variable_roles values ('signal_time'  , 'input' , NULL, NULL, '#1');
    insert into  #forecast_variable_roles values ('signal_value' , 'target', NULL, NULL, '#1');

    exec 'call "aa.forecast.db.hdb.apl.afllang::' || FunctionName || '" '
        || '('
        || '    #forecast_function_header,'
        || '    #forecast_operation_config,'
        || '    #forecast_variable_descs,'
        || '    #forecast_variable_roles,'
        || '    "aa.forecast.db.data::' || DatasetName || '",'
        || '    #forecast_results,'
        || '    #forecast_operation_log,'
        || '    #forecast_summary,'
        || '    #forecast_indicators'
        || ') with overview';

    operation_config  = select * from #forecast_operation_config;
    results           = select * from #forecast_results;
    operation_log     = select * from #forecast_operation_log;
    summary           = select * from #forecast_summary;
    indicators        = select * from #forecast_indicators;

    drop table #forecast_function_header;
    drop table #forecast_operation_config;
    drop table #forecast_variable_descs;
    drop table #forecast_variable_roles;

    drop table #forecast_operation_log;
    drop table #forecast_summary;
    drop table #forecast_indicators;
    drop table #forecast_results;
END;

```

Save the file using the ![save](00-save.png) icon from the menu.

Right click on **`forecast.hdbprocedure`**  and select **Build Selected Files**.

The console should display at the end the following message:

```
(Builder) Build of /forecast/db completed successfully.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the APL procedure)]

You can now test the procedure.

Switch to the ***Database Explorer*** perspective using the ![explorer](00-dbexplorer-icon.png) icon.

Select your **HDI Container** connection, and open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
DO BEGIN
    CALL "aa.forecast.db.hdb.apl.procedures::forecast"(
        DATASETNAME             => 'CashFlows',
        HORIZON                 => 21,
        LASTTRAININGTIMEPOINT   => '2001-12-28',
        OPERATION_CONFIG        => :OPERATION_CONFIG,
        RESULTS                 => :RESULTS,
        OPERATION_LOG           => :OPERATION_LOG,
        SUMMARY                 => :SUMMARY,
        INDICATORS              => :INDICATORS
    );

    SELECT
          c."signal_time"
        , c."signal_value"
        , "kts_1"                as forecast
        , "kts_1_lowerlimit_95%" as lower_limit
        , "kts_1_upperlimit_95%" as upper_limit
    from
         "aa.forecast.db.data::CashFlows" c
    join :RESULTS f
    on   c."signal_time" = f."signal_time"
    order by c."signal_time" asc;
END;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_1]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Create the PAL procedures)]

In order to expose in your application the ability to execute the PAL algorithms, you will need create a stored procedure, that you will later expose in a XSJS service.

Switch to the ***Development*** perspective using the ![Web IDE Development](00-development.png) icon.

Right click on the **`pal`** folder (from **`forecast/db/src/hdb`** ) and select **New > Folder**.

Enter **`procedures`** as the folder name, then click on **OK**.

#### PAL Seasonality Test

In this procedure, you will expose a series of parameters from the PAL Seasonality Test function:

  - Alpha

The complete description of these parameters is available in the [Seasonality Test function](https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/latest/en-US/d990dc754e714d95a30721aa1dc3f2f0.html) documentation.

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`seasonality_test.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/db/src/hdb/pal/procedures/seasonality_test.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.forecast.db.hdb.pal.procedures::seasonality_test" (
    in DatasetName           varchar(100),
    in Alpha                 double default 0.2,
    out operation_config     "aa.forecast.db.hdb.pal::common.tt_parameter",
    out statistic            "aa.forecast.db.hdb.pal::common.tt_statistics",
    out output               "aa.forecast.db.hdb.pal::seasonality_test.tt_output"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    create local temporary table #seasonality_test_operation_config like "aa.forecast.db.hdb.pal::common.tt_parameter";
    create local temporary table #seasonality_test_statistic        like "aa.forecast.db.hdb.pal::common.tt_statistics";
    create local temporary table #seasonality_test_output           like "aa.forecast.db.hdb.pal::seasonality_test.tt_output";

    if :Alpha is not null then insert into #seasonality_test_operation_config values ('ALPHA', null, :Alpha, null); end if;

    exec 'call "aa.forecast.db.hdb.pal.afllang::seasonality_test" '
        || '('
        || '    "aa.forecast.db.hdb.pal.views::' || DatasetName || '",'
        || '    #seasonality_test_operation_config,'
        || '    #seasonality_test_statistic,'
        || '    #seasonality_test_output'
        || ') with overview';

    operation_config  = select * from #seasonality_test_operation_config;
    output            = select * from #seasonality_test_output;
    statistic         = select * from #seasonality_test_statistic;

    drop table #seasonality_test_operation_config;
    drop table #seasonality_test_output;
    drop table #seasonality_test_statistic;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

#### PAL Auto ARIMA

In this procedure, you will expose a series of parameters from the PAL Auto ARIMA function:

 - Search Strategy
 - Seasonal Period
 - Forecast Length

The complete description of these parameters is available in the [Auto ARIMA function](https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/latest/en-US/9f2574e9ad4b4536aa4ddcc8d0d35a78.html) documentation.

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`auto_arima.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/db/src/hdb/pal/procedures/auto_arima.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.forecast.db.hdb.pal.procedures::auto_arima" (
    in DatasetName      varchar(100),
    in SearchStrategy   integer default 1,
    in SeasonalPeriod   integer default -1,
    in ForecastLength   integer default 1,
    out operation_config          "aa.forecast.db.hdb.pal::common.tt_parameter",
    out operation_config_forecast "aa.forecast.db.hdb.pal::common.tt_parameter",
    out fit                       "aa.forecast.db.hdb.pal::arima.tt_fit",
    out model                     "aa.forecast.db.hdb.pal::arima.tt_model",
    out output                    "aa.forecast.db.hdb.pal::arima.tt_output"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    create local temporary table #auto_arima_dataset_empty             like "aa.forecast.db.hdb.pal::common.tt_dataset";
    create local temporary table #auto_arima_operation_config          like "aa.forecast.db.hdb.pal::common.tt_parameter";
    create local temporary table #auto_arima_operation_config_forecast like "aa.forecast.db.hdb.pal::common.tt_parameter";
    create local temporary table #auto_arima_fit                       like "aa.forecast.db.hdb.pal::arima.tt_fit";
    create local temporary table #auto_arima_model                     like "aa.forecast.db.hdb.pal::arima.tt_model";
    create local temporary table #auto_arima_output_raw                like "aa.forecast.db.hdb.pal::arima.tt_output_raw";
    create local temporary table #auto_arima_output                    like "aa.forecast.db.hdb.pal::arima.tt_output";

    if :SearchStrategy    is not null then insert into #auto_arima_operation_config values ('SEARCH_STRATEGY'    , :SearchStrategy    , null    , null); end if;
    if :SeasonalPeriod    is not null then insert into #auto_arima_operation_config values ('SEASONAL_PERIOD'    , :SeasonalPeriod    , null    , null); end if;

    exec 'call "aa.forecast.db.hdb.pal.afllang::auto_arima" '
        || '('
        || '    "aa.forecast.db.hdb.pal.views::' || DatasetName || '",'
        || '    #auto_arima_operation_config,'
        || '    #auto_arima_model,'
        || '    #auto_arima_fit'
        || ') with overview';

    if :ForecastLength    is not null then insert into #auto_arima_operation_config_forecast values ('FORECAST_LENGTH' , :ForecastLength    , null    , null); end if;

    exec 'call "aa.forecast.db.hdb.pal.afllang::auto_arima_forecast" '
        || '('
        || '    #auto_arima_dataset_empty,'
        || '    #auto_arima_model,'
        || '    #auto_arima_operation_config_forecast,'
        || '    #auto_arima_output_raw'
        || ') with overview';

    exec 'insert into #auto_arima_output ('
    || '    select '
    || '        to_int(rank() over (order by "idx" asc, "signal_time" asc)) as "signal_time", "signal_value", "forecast", "standard_error", "lowerlimit_80", "upperlimit_80",  "lowerlimit_95",  "upperlimit_95"'
    || '    from ('
    || '        select 1 as "idx", "signal_time", "signal_value" , null as "forecast", null as "standard_error", null as "lowerlimit_80", null as "upperlimit_80", null as "lowerlimit_95", null as "upperlimit_95" from "aa.forecast.db.hdb.pal.views::' || DatasetName || '"'
    || '        union all'
    || '        select 2 as "idx", "signal_time", null, "forecast", "standard_error", "lowerlimit_80", "upperlimit_80",  "lowerlimit_95",  "upperlimit_95" from #auto_arima_output_raw'
    || '    )'
    || ');';

    operation_config          = select * from #auto_arima_operation_config;
    operation_config_forecast = select * from #auto_arima_operation_config_forecast;
    fit                       = select * from #auto_arima_fit;
    model                     = select * from #auto_arima_model;
    output                    = select * from #auto_arima_output order by 1;

    drop table #auto_arima_dataset_empty;
    drop table #auto_arima_operation_config;
    drop table #auto_arima_operation_config_forecast;
    drop table #auto_arima_model;
    drop table #auto_arima_output_raw;
    drop table #auto_arima_output;
    drop table #auto_arima_fit;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.

#### PAL Auto Smoothing

In this procedure, you will expose a series of parameters from the PAL Auto ARIMA function:

- Forecast Number
- Accuracy Measure
- Training Ratio
- Seasonality
- Seasonal

The complete description of these parameters is available in the [Auto Smoothing function](https://help.sap.com/viewer/2cfbc5cf2bc14f028cfbe2a2bba60a50/latest/en-US/60471b0920104ca3a112dd54d74774ad.html) documentation.

Right click on the **`procedures`** folder node from the tree, and select **New > File**.

Enter **`auto_smoothing.hdbprocedure`** as the file name, then click on **OK**.

This is the full path of the created file:

```
forecast/db/src/hdb/pal/procedures/auto_smoothing.hdbprocedure
```

Paste the following content:

```SQL
PROCEDURE "aa.forecast.db.hdb.pal.procedures::auto_smoothing" (
    in DatasetName          varchar(100),
    in ForecastNum          integer default 1,
    in AccuracyMeasure      varchar(4) default 'MAPE',
    in TrainingRatio        double default 1.0,
    in SeasonalityCriterion double default 0.0000001,
    out operation_config    "aa.forecast.db.hdb.pal::common.tt_parameter",
    out statistic           "aa.forecast.db.hdb.pal::common.tt_statistics",
    out output              "aa.forecast.db.hdb.pal::smoothing.tt_output"
)
LANGUAGE SQLSCRIPT SQL SECURITY INVOKER AS
BEGIN
    create local temporary table #auto_smoothing_operation_config  like "aa.forecast.db.hdb.pal::common.tt_parameter";
    create local temporary table #auto_smoothing_statistic         like "aa.forecast.db.hdb.pal::common.tt_statistics";
    create local temporary table #auto_smoothing_output_raw        like "aa.forecast.db.hdb.pal::smoothing.tt_output_raw";
    create local temporary table #auto_smoothing_output            like "aa.forecast.db.hdb.pal::smoothing.tt_output";

    insert into #auto_smoothing_operation_config values ('MODELSELECTION', 1 , null, null);
    if :ForecastNum             is not null then insert into #auto_smoothing_operation_config values ('FORECAST_NUM'          , :ForecastNum    , null                  , null); end if;
    if :AccuracyMeasure         is not null then insert into #auto_smoothing_operation_config values ('ACCURACY_MEASURE'      , null            , null                  , :AccuracyMeasure); end if;
    if :TrainingRatio           is not null then insert into #auto_smoothing_operation_config values ('TRAINING_RATIO'        , null            , :TrainingRatio        , null); end if;
    if :SeasonalityCriterion    is not null then insert into #auto_smoothing_operation_config values ('SEASONALITY_CRITERION' , null            , :SeasonalityCriterion , null); end if;

    exec 'call "aa.forecast.db.hdb.pal.afllang::auto_smoothing" '
        || '('
        || '    "aa.forecast.db.hdb.pal.views::' || DatasetName || '",'
        || '    #auto_smoothing_operation_config,'
        || '    #auto_smoothing_output_raw,'
        || '    #auto_smoothing_statistic'
        || ') with overview';

    exec 'insert into #auto_smoothing_output ('
    || '    select '
    || '        ifnull(d."signal_time", f."signal_time") , d."signal_value", "forecast", "lowerlimit_1", "upperlimit_1",  "lowerlimit_2", "upperlimit_2"'
    || '    from "aa.forecast.db.hdb.pal.views::' || DatasetName || '" d'
    || '    full outer join #auto_smoothing_output_raw f on d."signal_time" = f."signal_time"'
    || ');';

    operation_config  = select * from #auto_smoothing_operation_config;
    statistic         = select * from #auto_smoothing_statistic;
    output            = select * from #auto_smoothing_output order by 1;

    drop table #auto_smoothing_operation_config;
    drop table #auto_smoothing_statistic;
    drop table #auto_smoothing_output_raw;
    drop table #auto_smoothing_output;
END;
```

Save the file using the ![save](00-save.png) icon from the menu.


Using the CTRL button on the keyboard, select the following files:

- **`auto_arima.hdbprocedure`**
- **`auto_smoothing.hdbprocedure`**
- **`seasonality_test.hdbprocedure`**

Then, right click the selection, then use the **Build Selected Files** menu.

The console should display at the end the following message:

```
(Builder) Build of /forecast/db completed successfully.
```

[DONE]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the PAL procedure (1/3))]

You can now check how many users will get recommendation for the collaborative filtering approach.

Switch to the ***Database Explorer*** perspective using the ![explorer](00-dbexplorer-icon.png) icon.

Select your **HDI Container** connection, and open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
do begin
    call "aa.forecast.db.hdb.pal.procedures::seasonality_test"(
        datasetname      => 'Ozone',
        operation_config => :operation_config,
        statistic        => :statistic,
        output           => :output
    );
    select * from :output;
end;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_2]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the PAL procedure (2/3))]

You can now check how the collaborative filtering results using the created stored procedure.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
do begin
    call "aa.forecast.db.hdb.pal.procedures::auto_arima"(
        datasetname               => 'Ozone',
        forecastlength            => 60,
        operation_config          => :operation_config,
        operation_config_forecast => :operation_config_forecast,
        fit                       => :fit,
        model                     => :model,
        output                    => :output
    );
    select *    from :output;
end;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_3]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Check the PAL procedure (3/3))]

You can now check how the content based filtering results using the created stored procedure.

Open a new **SQL Console** using the ![sql](00-dbexplorer-sql.png) icon.

Paste the following content in the console, and use the execute icon ![run](00-dbexplorer-run.png) from the menu.

```SQL
do begin
	call "aa.forecast.db.hdb.pal.procedures::auto_smoothing"(
		datasetname      => 'CashFlows',
        forecastnum      => 21,
        accuracymeasure  => 'mape',
        trainingratio    => 0.75,
        operation_config => :operation_config,
        statistic        => :statistic,
        output           => :output
	);
	select *	from :output;
end;
```

Provide an answer to the question below then click on **Validate**.

[VALIDATE_4]
[ACCORDION-END]

[ACCORDION-BEGIN [Step 1: ](Commit your changes)]

On the icon bar located on the right side of the Web IDE, click on the **Git Pane** icon ![Web IDE](00-webide-git.png).

Click on **Stage All**, enter a commit comment, then click on **Commit and Push > origin master**.

[DONE]
[ACCORDION-END]
