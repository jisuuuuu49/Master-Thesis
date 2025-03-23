

-- CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.merged_eurostat_01` AS

-- -- Convert filtered_gdp_nuts2 to long format and merge with merged_eurostat
-- WITH gdp_long AS (
--     -- Convert 2014, 2015, 2016 columns to long format
--     SELECT 
--         Country,
--         NUTS1,
--         NUTS2,
--         '2014' AS Year, 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(`2014`, r'[^0-9.]', ''), '') AS FLOAT64) AS GDP
--     FROM `thesis-project-451520.thesis_data.filtered_gdp_nuts2`
    
--     UNION ALL

--     SELECT 
--         Country,
--         NUTS1,
--         NUTS2,
--         '2015' AS Year, 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(`2015`, r'[^0-9.]', ''), '') AS FLOAT64) AS GDP
--     FROM `thesis-project-451520.thesis_data.filtered_gdp_nuts2`
    
--     UNION ALL

--     SELECT 
--         Country,
--         NUTS1,
--         NUTS2,
--         '2016' AS Year, 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(`2016`, r'[^0-9.]', ''), '') AS FLOAT64) AS GDP
--     FROM `thesis-project-451520.thesis_data.filtered_gdp_nuts2`
-- )

-- -- Merge GDP data with merged_eurostat
-- SELECT 
--     eu.*,  -- Keep all existing Eurostat data
--     gdp.GDP AS gdp_nuts2

-- FROM `thesis-project-451520.thesis_data.merged_eurostat` eu
-- LEFT JOIN gdp_long gdp
--     ON eu.Country = gdp.Country
--     AND eu.NUTS1 = gdp.NUTS1
--     AND eu.NUTS2 = gdp.NUTS2
--     AND SAFE_CAST(eu.Year AS STRING) = gdp.Year;










-- WITH gdppc_nuts2 AS (
--     -- GDP per Capita (NUTS2) - Fixing Comma & Missing Value Issue
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2, 
--         string_field_3 AS NUTS2_label, 
--         '2014' AS Year, 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_6, r'[^0-9.-]', ''), '') AS FLOAT64) AS gdppc_nuts2
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2`
--     WHERE LENGTH(string_field_2) = 4 AND string_field_6 NOT IN (':')
    
--     UNION ALL
    
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, 
--         CAST(string_field_2 AS STRING), string_field_3, 
--         '2015', 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_7, r'[^0-9.-]', ''), '') AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2`
--     WHERE LENGTH(string_field_2) = 4 AND string_field_7 NOT IN (':')
    
--     UNION ALL
    
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, 
--         CAST(string_field_2 AS STRING), string_field_3, 
--         '2016', 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_8, r'[^0-9.-]', ''), '') AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2`
--     WHERE LENGTH(string_field_2) = 4 AND string_field_8 NOT IN (':')
-- ),

-- crdrate_net_mig AS (
--     -- Crude Rate of Net Migration (NUTS2)
--     SELECT TRIM(UPPER(string_field_0)) AS Country, string_field_1 AS NUTS1, CAST(string_field_2 AS STRING) AS NUTS2, string_field_3 AS NUTS2_label, 
--            '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_net_migration
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3, 
--            '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3, 
--            '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
-- ),

-- crdrate_pop_chg AS (
--     -- Crude Rate of Population Change (NUTS2)
--     SELECT TRIM(UPPER(string_field_0)) AS Country, string_field_1 AS NUTS1, CAST(string_field_2 AS STRING) AS NUTS2, string_field_3 AS NUTS2_label, 
--            '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_pop_change
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3, 
--            '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3, 
--            '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
-- ),

-- gdp_nuts2 AS (
--     -- GDP per Capita (NUTS2) - Fixing Comma & Missing Value Issue
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2, 
--         string_field_3 AS NUTS2_label, 
--         '2014' AS Year, 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_6, r'[^0-9.-]', ''), '') AS FLOAT64) AS gdp_per_capita
--     FROM `thesis-project-451520.thesis_data.gdp_nuts2`
--     WHERE LENGTH(string_field_2) = 4 AND string_field_6 NOT IN (':')
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, 
--         CAST(string_field_2 AS STRING), string_field_3, 
--         '2015', 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_7, r'[^0-9.-]', ''), '') AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdp_nuts2`
--     WHERE LENGTH(string_field_2) = 4 AND string_field_7 NOT IN (':')
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, 
--         CAST(string_field_2 AS STRING), string_field_3, 
--         '2016', 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_8, r'[^0-9.-]', ''), '') AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdp_nuts2`
--     WHERE LENGTH(string_field_2) = 4 AND string_field_8 NOT IN (':')
-- ),


-- gdppc_c AS (
--     -- GDP per Capita (Country Level)
--     SELECT TRIM(UPPER(string_field_0)) AS Country, '2014' AS Year, SAFE_CAST(int64_field_21 AS FLOAT64) AS gdppc_country
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), '2015', SAFE_CAST(int64_field_22 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), '2016', SAFE_CAST(int64_field_21 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
-- ),
-- pop_den_nuts3 AS (
--     -- Population Density (NUTS3)
--     SELECT TRIM(UPPER(string_field_0)) AS Country, string_field_1 AS NUTS1, 
--            CAST(string_field_2 AS STRING) AS NUTS2, string_field_3 AS NUTS2_label, 
--            '2014' AS Year, SAFE_CAST(string_field_28 AS FLOAT64) AS population_density
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, 
--            CAST(string_field_2 AS STRING), string_field_3, 
--            '2015', SAFE_CAST(string_field_29 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, 
--            CAST(string_field_2 AS STRING), string_field_3, 
--            '2016', SAFE_CAST(string_field_30 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
-- ),

-- ttlpop_chg_nuts3 AS (
--     -- Total Population Change (NUTS3) - Fixing Comma Issue in Numbers
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2, string_field_3 AS NUTS2_label, 
--         '2014' AS Year, 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_18, r'[^0-9.-]', ''), '') AS FLOAT64) AS total_population_change
--     FROM `thesis-project-451520.thesis_data.ttlpop_chg_nuts3`
--     WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, 
--         CAST(string_field_2 AS STRING), string_field_3, 
--         '2015', 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_19, r'[^0-9.-]', ''), '') AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.ttlpop_chg_nuts3`
--     WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, 
--         CAST(string_field_2 AS STRING), string_field_3, 
--         '2016', 
--         SAFE_CAST(NULLIF(REGEXP_REPLACE(string_field_20, r'[^0-9.-]', ''), '') AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.ttlpop_chg_nuts3`
--     WHERE LENGTH(string_field_2) = 4
-- ),


-- unemp_nuts2 AS (
--     -- Unemployment Rate (NUTS2)
--     SELECT TRIM(UPPER(string_field_0)) AS Country, string_field_1 AS NUTS1, 
--            CAST(string_field_2 AS STRING) AS NUTS2, string_field_3 AS NUTS2_label, 
--            '2014' AS Year, SAFE_CAST(string_field_4 AS FLOAT64) AS unemployment_rate
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, 
--            CAST(string_field_2 AS STRING), string_field_3, 
--            '2015', SAFE_CAST(string_field_5 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT TRIM(UPPER(string_field_0)), string_field_1, 
--            CAST(string_field_2 AS STRING), string_field_3, 
--            '2016', SAFE_CAST(string_field_6 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
-- )


-- -- **Final Merging of All Eurostat Data Before Merging with ESS**
-- SELECT 
--     e.Country, e.NUTS1, e.NUTS2, e.Year,
--     e.crude_net_migration, cpc.crude_pop_change,
--     g.gdp_per_capita, gc.gdppc_country,
--     pd.population_density, tp.total_population_change,
--     u.unemployment_rate, gn.gdppc_nuts2

-- FROM crdrate_net_mig e
-- LEFT JOIN crdrate_pop_chg cpc ON e.NUTS2 = cpc.NUTS2 AND e.Year = cpc.Year AND e.Country = cpc.Country
-- LEFT JOIN gdp_nuts2 g ON e.NUTS2 = g.NUTS2 AND e.Year = g.Year AND e.Country = g.Country
-- LEFT JOIN gdppc_c gc ON e.Country = gc.Country AND e.Year = gc.Year
-- LEFT JOIN pop_den_nuts3 pd ON e.NUTS2 = pd.NUTS2 AND e.Year = pd.Year AND e.Country = pd.Country
-- LEFT JOIN ttlpop_chg_nuts3 tp ON e.NUTS2 = tp.NUTS2 AND e.Year = tp.Year AND e.Country = tp.Country
-- LEFT JOIN unemp_nuts2 u ON e.NUTS2 = u.NUTS2 AND e.Year = u.Year AND e.Country = u.Country
-- LEFT JOIN gdppc_nuts2 gn ON e.NUTS2 = gn.NUTS2 AND e.Year = gn.Year AND e.Country = gn.Country;








-- WITH eurostat_data AS (
--     -- Crude Rate of Net Migration (NUTS2-Level)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_net_migration
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
-- ),

-- crude_pop_change AS (
--     -- Crude Rate of Population Change (NUTS2)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_pop_change
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
-- ),

-- gdp_data AS (
--     -- GDP per capita (NUTS2)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_6 AS FLOAT64) AS gdp_per_capita
--     FROM `thesis-project-451520.thesis_data.gdp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_7 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_8 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdp_nuts2` WHERE LENGTH(string_field_2) = 4
-- ),

-- gdp_country AS (
--     -- GDP per Capita (Country-Level)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         '2014' AS Year, SAFE_CAST(int64_field_21 AS FLOAT64) AS gdppc_country
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), '2015', SAFE_CAST(int64_field_22 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), '2016', SAFE_CAST(int64_field_21 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
-- )

-- -- **Final Merging of All Eurostat Data Before Merging with ESS**
-- SELECT 
--     e.Country, 
--     e.NUTS2, 
--     e.Year,
--     e.crude_net_migration,
--     cpc.crude_pop_change,
--     g.gdp_per_capita,
--     gc.gdppc_country

-- FROM eurostat_data e
-- LEFT JOIN crude_pop_change cpc 
-- ON e.NUTS2 = cpc.NUTS2 AND e.Year = cpc.Year AND e.Country = cpc.Country

-- LEFT JOIN gdp_data g 
-- ON e.NUTS2 = g.NUTS2 AND e.Year = g.Year AND e.Country = g.Country

-- LEFT JOIN gdp_country gc 
-- ON e.Country = gc.Country AND e.Year = gc.Year;









-- WITH eurostat_data AS (
--     -- Crude Rate of Net Migration (NUTS2-Level)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_net_migration
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
-- ),

-- gdp_data AS (
--     -- GDP per capita (NUTS2)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_6 AS FLOAT64) AS gdp_per_capita
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_7 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_8 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
-- ),

-- unemployment_data AS (
--     -- Unemployment Rate (NUTS2)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_4 AS FLOAT64) AS unemployment_rate
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_5 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_6 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
-- ),

-- crude_pop_change AS (
--     -- Crude Rate of Population Change (NUTS2)
--     SELECT 
--         TRIM(UPPER(string_field_0)) AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_pop_change
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         TRIM(UPPER(string_field_0)), string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_pop_chg` WHERE LENGTH(string_field_2) = 4
-- )

-- -- Merging ESS + Eurostat Data
-- SELECT 
--     ess.Country,
--     CAST(ess.NUTS2 AS STRING) AS NUTS2,
--     ess.idno,
--     CAST(ess.Year AS STRING) AS Year,
    
--     -- Outgroup Hostility Data
--     ess.mean_immigration_economy,
--     ess.mean_crime_impact,
--     ess.mean_economic_contribution,
--     ess.mean_hostility_marriage,
--     ess.mean_hostility_boss,
--     ess.mean_cultural_impact,

--     -- Social Contact Data
--     ess.mean_contact_frequency,
--     ess.mean_contact_quality,
--     ess.mean_contact_perception,

--     -- Economic Competition Data
--     ess.mean_job_competition,
--     ess.mean_jobs_taken,

--     -- Eurostat Regional Data (NUTS2)
--     e.crude_net_migration,
--     g.gdp_per_capita,
--     u.unemployment_rate,
--     cpc.crude_pop_change

-- FROM `thesis-project-451520.thesis_data.ess_final` ess
-- LEFT JOIN eurostat_data e 
-- ON CAST(ess.NUTS2 AS STRING) = e.NUTS2 
-- AND CAST(ess.Year AS STRING) = e.Year 
-- AND TRIM(UPPER(ess.Country)) = e.Country  

-- LEFT JOIN gdp_data g 
-- ON CAST(ess.NUTS2 AS STRING) = g.NUTS2 
-- AND CAST(ess.Year AS STRING) = g.Year 
-- AND TRIM(UPPER(ess.Country)) = g.Country  

-- LEFT JOIN unemployment_data u 
-- ON CAST(ess.NUTS2 AS STRING) = u.NUTS2 
-- AND CAST(ess.Year AS STRING) = u.Year 
-- AND TRIM(UPPER(ess.Country)) = u.Country  

-- LEFT JOIN crude_pop_change cpc 
-- ON CAST(ess.NUTS2 AS STRING) = cpc.NUTS2 
-- AND CAST(ess.Year AS STRING) = cpc.Year 
-- AND TRIM(UPPER(ess.Country)) = cpc.Country;









-- WITH eurostat_data AS (
--     -- Crude Rate of Net Migration (NUTS2-Level)
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_net_migration
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
-- ),

-- gdp_data AS (
--     -- GDP per capita (NUTS2)
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_6 AS FLOAT64) AS gdp_per_capita
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_7 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_8 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
-- ),

-- unemployment_data AS (
--     -- Unemployment Rate (NUTS2)
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_4 AS FLOAT64) AS unemployment_rate
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_5 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_6 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
-- ),

-- population_density AS (
--     -- Population Density (NUTS3)
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_28 AS FLOAT64) AS population_density
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_29 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_30 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
-- ),

-- gdppc_country AS (
--     -- Country-Level GDP per Capita (`gdppc_c`)
--     SELECT 
--         string_field_0 AS Country, 
--         '2014' AS Year, SAFE_CAST(int64_field_21 AS FLOAT64) AS gdppc_country
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
--     UNION ALL
--     SELECT 
--         string_field_0, '2015', SAFE_CAST(int64_field_22 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
--     UNION ALL
--     SELECT 
--         string_field_0, '2016', SAFE_CAST(int64_field_21 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_c`
-- )

-- -- Merging ESS + Eurostat Data
-- SELECT 
--     ess.Country,
--     CAST(ess.NUTS2 AS STRING) AS NUTS2,
--     ess.idno,
--     CAST(ess.Year AS STRING) AS Year,
    
--     -- Outgroup Hostility Data
--     ess.mean_immigration_economy,
--     ess.mean_crime_impact,
--     ess.mean_economic_contribution,
--     ess.mean_hostility_marriage,
--     ess.mean_hostility_boss,
--     ess.mean_cultural_impact,

--     -- Social Contact Data
--     ess.mean_contact_frequency,
--     ess.mean_contact_quality,
--     ess.mean_contact_perception,

--     -- Economic Competition Data
--     ess.mean_job_competition,
--     ess.mean_jobs_taken,

--     -- Eurostat Regional Data (NUTS2)
--     e.crude_net_migration,
--     g.gdp_per_capita,
--     u.unemployment_rate,
--     p.population_density,

--     -- Country-Level GDP per Capita (New Addition)
--     c.gdppc_country

-- FROM `thesis-project-451520.thesis_data.ess_final` ess
-- LEFT JOIN eurostat_data e 
-- ON CAST(ess.NUTS2 AS STRING) = e.NUTS2 
-- AND CAST(ess.Year AS STRING) = e.Year 
-- AND ess.Country = e.Country  

-- LEFT JOIN gdp_data g 
-- ON CAST(ess.NUTS2 AS STRING) = g.NUTS2 
-- AND CAST(ess.Year AS STRING) = g.Year 
-- AND ess.Country = g.Country  

-- LEFT JOIN unemployment_data u 
-- ON CAST(ess.NUTS2 AS STRING) = u.NUTS2 
-- AND CAST(ess.Year AS STRING) = u.Year 
-- AND ess.Country = u.Country  

-- LEFT JOIN population_density p 
-- ON CAST(ess.NUTS2 AS STRING) = p.NUTS2 
-- AND CAST(ess.Year AS STRING) = p.Year 
-- AND ess.Country = p.Country  

-- -- **Joining gdppc_c on Country & Year**
-- LEFT JOIN gdppc_country c 
-- ON ess.Country = c.Country 
-- AND CAST(ess.Year AS STRING) = c.Year;






-- WITH eurostat_data AS (
--     -- Crude Rate of Net Migration
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,  -- Ensure STRING type
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_18 AS FLOAT64) AS crude_net_migration
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_19 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_20 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.crdrate_net_mig` WHERE LENGTH(string_field_2) = 4
-- ),

-- gdp_data AS (
--     -- GDP per capita (NUTS2)
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_6 AS FLOAT64) AS gdp_per_capita
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_7 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_8 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.gdppc_nuts2` WHERE LENGTH(string_field_2) = 4
-- ),

-- unemployment_data AS (
--     -- Unemployment Rate (NUTS2)
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_4 AS FLOAT64) AS unemployment_rate
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_5 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_6 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.unemp_nuts2` WHERE LENGTH(string_field_2) = 4
-- ),

-- population_density AS (
--     -- Population Density (NUTS3)
--     SELECT 
--         string_field_0 AS Country, 
--         string_field_1 AS NUTS1, 
--         CAST(string_field_2 AS STRING) AS NUTS2,
--         string_field_3 AS NUTS2_label,
--         '2014' AS Year, SAFE_CAST(string_field_28 AS FLOAT64) AS population_density
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2015', SAFE_CAST(string_field_29 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
--     UNION ALL
--     SELECT 
--         string_field_0, string_field_1, CAST(string_field_2 AS STRING), string_field_3,
--         '2016', SAFE_CAST(string_field_30 AS FLOAT64)
--     FROM `thesis-project-451520.thesis_data.pop_den_nuts3` WHERE LENGTH(string_field_2) = 4
-- )

-- -- Merging ESS + Eurostat Data
-- SELECT 
--     ess.Country,
--     CAST(ess.NUTS2 AS STRING) AS NUTS2,
--     ess.idno,
--     CAST(ess.Year AS STRING) AS Year,
    
--     -- Outgroup Hostility Data
--     ess.mean_immigration_economy,
--     ess.mean_crime_impact,
--     ess.mean_economic_contribution,
--     ess.mean_hostility_marriage,
--     ess.mean_hostility_boss,
--     ess.mean_cultural_impact,

--     -- Social Contact Data
--     ess.mean_contact_frequency,
--     ess.mean_contact_quality,
--     ess.mean_contact_perception,

--     -- Economic Competition Data
--     ess.mean_job_competition,
--     ess.mean_jobs_taken,

--     -- Eurostat Regional Data
--     e.crude_net_migration,
--     g.gdp_per_capita,
--     u.unemployment_rate,
--     p.population_density

-- FROM `thesis-project-451520.thesis_data.ess_final` ess
-- LEFT JOIN eurostat_data e 
-- ON CAST(ess.NUTS2 AS STRING) = e.NUTS2 AND CAST(ess.Year AS STRING) = e.Year
-- LEFT JOIN gdp_data g 
-- ON CAST(ess.NUTS2 AS STRING) = g.NUTS2 AND CAST(ess.Year AS STRING) = g.Year
-- LEFT JOIN unemployment_data u 
-- ON CAST(ess.NUTS2 AS STRING) = u.NUTS2 AND CAST(ess.Year AS STRING) = u.Year
-- LEFT JOIN population_density p 
-- ON CAST(ess.NUTS2 AS STRING) = p.NUTS2 AND CAST(ess.Year AS STRING) = p.Year;







