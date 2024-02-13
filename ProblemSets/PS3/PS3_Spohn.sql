
CREATE TABLE PS3_Spohn(
"policyID" TEXT,
"state" TEXT,
"county" TEXT,
"eq_site_limit" REAL,
"hu_site_limit" REAL,
"fl_site_limit" REAL,
"fr_site_limit" REAL,
"tiv_2011" REAL,
"tive_2012" REAL,
"eq_site_deductible" REAL,
"hu_site_deductible" REAL,
"fl_site_deductible" REAL,
"fr_site_deductible" REAL,
"point_latitude" REAL,
"point_longitude" REAL,
"line" TEXT,
"construction" TEXT,
"point_granularity" INTEGER
);
.shell pwd
.mode csv
.import "FL_insurance_sample.csv" PS3_Spohn
SELECT * FROM PS3_Spohn LIMIT_10;
SELECT DISTINCT "County" FROM PS3_Spohn;
SELECT AVG("tiv_2012" - "tiv_2011") AS avg_appreciation
FROM PS3_Spohn;
SELECT "construction", COUNT(*) AS frequency, 
CAST(COUNT(*) AS REAL) / (SELECT COUNT(*) FROM PS3_Spohn) AS fraction
FROM PS3_Spohn
GROUP BY "construction";
ls
;
.shell ls
.quit
