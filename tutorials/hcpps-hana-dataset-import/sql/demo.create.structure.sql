CREATE SCHEMA "PSDEMO";

SET SCHEMA "PSDEMO";

DROP TABLE "CashFlow";

CREATE COLUMN TABLE "CashFlow" ("Date" DAYDATE CS_DAYDATE, "WorkingDaysIndices" SMALLINT CS_INT, "ReverseWorkingDaysIndices" SMALLINT CS_INT, "MondayMonthInd" SMALLINT CS_INT, "TuesdayMonthInd" SMALLINT CS_INT, "WednesdayMonthInd" SMALLINT CS_INT, "ThursdayMonthInd" SMALLINT CS_INT, "FridayMonthInd" SMALLINT CS_INT, "BeforeLastMonday" SMALLINT CS_INT, "LastMonday" SMALLINT CS_INT, "BeforeLastTuesday" SMALLINT CS_INT, "LastTuesday" SMALLINT CS_INT, "BeforeLastWednesday" SMALLINT CS_INT, "LastWednesday" SMALLINT CS_INT, "BeforeLastThursday" SMALLINT CS_INT, "LastThursday" SMALLINT CS_INT, "BeforeLastFriday" SMALLINT CS_INT, "LastFriday" SMALLINT CS_INT, "Last5WDaysInd" SMALLINT CS_INT, "Last5WDays" SMALLINT CS_INT, "Last4WDaysInd" SMALLINT CS_INT, "Last4WDays" SMALLINT CS_INT, "LastWMonth" SMALLINT CS_INT, "BeforeLastWMonth" SMALLINT CS_INT, "Cash" DECIMAL(17,6) CS_FIXED, PRIMARY KEY ("Date")) UNLOAD PRIORITY 5  AUTO MERGE ;

DROP TABLE "Census";

CREATE COLUMN TABLE "Census" ("id" BIGINT CS_FIXED NOT NULL , "age" TINYINT CS_INT, "workclass" NVARCHAR(50), "fnlwgt" INTEGER CS_INT, "education" NVARCHAR(50), "education_num" TINYINT CS_INT, "marital_status" NVARCHAR(50), "occupation" NVARCHAR(50), "relationship" NVARCHAR(50), "race" NVARCHAR(50), "sex" NVARCHAR(10), "capital_gain" BIGINT CS_FIXED, "capital_loss" BIGINT CS_FIXED, "hours_per_week" TINYINT CS_INT, "native_country" NVARCHAR(50), "class" SMALLINT CS_INT, PRIMARY KEY ("id"));

DROP TABLE "Transactions";

CREATE COLUMN TABLE "Transactions" ("UserID" SMALLINT CS_INT NOT NULL, "ItemPurchased" NVARCHAR(50) NOT NULL, "Date_PutInCaddy" LONGDATE CS_LONGDATE NOT NULL, "Quantity" TINYINT CS_INT, "TransactionID" SMALLINT CS_INT NOT NULL, PRIMARY KEY ("TransactionID")) UNLOAD PRIORITY 5 AUTO MERGE ;