-- Step 1: Extract ESS1 Data with Renaming to Match ESS7
WITH ess1 AS (
    SELECT
        TRIM(UPPER(cntry)) AS Country,
        TRIM(UPPER(region)) AS NUTS2,
        SAFE_CAST(inwyr AS INT64) AS Year,
        idno,
        -- Renaming ESS1 variables to match ESS7 schema
        CASE WHEN SAFE_CAST(imgfrnd AS INT64) BETWEEN 1 AND 3 THEN SAFE_CAST(imgfrnd AS INT64) ELSE NULL END AS social_contact,
        CASE WHEN SAFE_CAST(imueclt AS INT64) BETWEEN 0 AND 10 THEN SAFE_CAST(imueclt AS INT64) ELSE NULL END AS cultural_impact,
        CASE WHEN SAFE_CAST(imbgeco AS INT64) BETWEEN 0 AND 10 THEN SAFE_CAST(imbgeco AS INT64) ELSE NULL END AS immigration_economy,
        CASE WHEN SAFE_CAST(imbleco AS INT64) BETWEEN 0 AND 10 THEN SAFE_CAST(imbleco AS INT64) ELSE NULL END AS perceived_economic_competition,
        CASE WHEN SAFE_CAST(imtcjob AS INT64) BETWEEN 0 AND 10 THEN SAFE_CAST(imtcjob AS INT64) ELSE NULL END AS jobs_taken,
        CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN SAFE_CAST(domicil AS INT64) ELSE NULL END AS rural_urban,
        CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN SAFE_CAST(acetalv AS INT64) ELSE NULL END AS minority_presence,
        SAFE_CAST(lrscale AS INT64) AS lrscale,
        CASE WHEN SAFE_CAST(lrscale AS INT64) >= 8 THEN 1 ELSE 0 END AS far_right_support,
        CASE WHEN SAFE_CAST(imwbcnt AS INT64) BETWEEN 0 AND 10 THEN SAFE_CAST(imwbcnt AS INT64) ELSE NULL END AS imwbcnt,
        chldhm,
        agea AS age,
        gndr,
        eisced,
        CASE WHEN SAFE_CAST(hinctnt AS INT64) BETWEEN 1 AND 10 THEN SAFE_CAST(hinctnt AS INT64) ELSE NULL END AS hinctnta,
        unemployed AS immig_background,
        NULL AS crude_net_migration,
        NULL AS crude_pop_change,
        NULL AS gdp_per_capita,
        NULL AS gdppc_country,
        NULL AS population_density,
        NULL AS unemployment_rate,
        NULL AS gdppc_nuts2,
        SUBSTRING(region, 1, 3) AS NUTS1
    FROM `thesis-project-451520.thesis_data.ESS1`
),

-- Step 2: Extract ESS7 Data (Already Cleaned)
ess7 AS (
    SELECT 
        TRIM(UPPER(Country)) AS Country,
        TRIM(UPPER(NUTS2)) AS NUTS2,
        idno,
        SAFE_CAST(Year AS INT64) AS Year,
        crime_impact, cultural_impact, contact_frequency, contact_quality, 
        contact_perception, immigration_economy, perceived_economic_competition, 
        jobs_taken, rural_urban, minority_presence, lrscale, far_right_support, 
        imwbcnt, chldhm, age, gndr, eisced, hinctnta, immig_background,
        --crude_net_migration, crude_pop_change, gdp_per_capita, gdppc_country,population_density, unemployment_rate, gdppc_nuts2,
         NUTS1
    FROM `thesis-project-451520.thesis_data.ESS7`
),

-- Step 3: Identify Missing Variables in ESS1 Compared to ESS7
missing_variables AS (
    SELECT column_name
    FROM `thesis-project-451520.thesis_data.INFORMATION_SCHEMA.COLUMNS`
    WHERE table_name = 'ESS7'
    EXCEPT DISTINCT
    SELECT column_name
    FROM `thesis-project-451520.thesis_data.INFORMATION_SCHEMA.COLUMNS`
    WHERE table_name = 'ESS1'
),

-- Step 4: Merge ESS1 and ESS7 with a Full Join
merged_ess AS (
    SELECT 
        COALESCE(e1.Country, e7.Country) AS Country,
        COALESCE(e1.Year, e7.Year) AS Year,
        COALESCE(e1.NUTS2, e7.NUTS2) AS NUTS2,
        COALESCE(e1.idno, e7.idno) AS idno,
        COALESCE(SAFE_CAST(e1.cultural_impact AS INT64), e7.cultural_impact) AS cultural_impact,
        COALESCE(SAFE_CAST(e1.social_contact AS INT64), e7.contact_quality) AS social_contact,
        COALESCE(SAFE_CAST(e1.immigration_economy AS INT64), e7.immigration_economy) AS immigration_economy,
        COALESCE(SAFE_CAST(e1.perceived_economic_competition AS INT64), e7.perceived_economic_competition) AS perceived_economic_competition,
        COALESCE(SAFE_CAST(e1.jobs_taken AS INT64), e7.jobs_taken) AS jobs_taken,
        COALESCE(SAFE_CAST(e1.rural_urban AS INT64), e7.rural_urban) AS rural_urban,
        COALESCE(SAFE_CAST(e1.minority_presence AS INT64), e7.minority_presence) AS minority_presence,
        COALESCE(SAFE_CAST(e1.lrscale AS INT64), e7.lrscale) AS lrscale,
        COALESCE(e1.far_right_support, e7.far_right_support) AS far_right_support,
        COALESCE(SAFE_CAST(e1.imwbcnt AS INT64), e7.imwbcnt) AS imwbcnt,
        COALESCE(e1.chldhm, e7.chldhm) AS chldhm,
        COALESCE(e1.age, e7.age) AS age,
        COALESCE(e1.gndr, e7.gndr) AS gndr,
        COALESCE(e1.eisced, e7.eisced) AS eisced,
        COALESCE(SAFE_CAST(e1.hinctnta AS INT64), e7.hinctnta) AS hinctnta,
        COALESCE(e1.immig_background, e7.immig_background) AS immig_background,
        -- COALESCE(e1.crude_net_migration, e7.crude_net_migration) AS crude_net_migration,
        -- COALESCE(e1.crude_pop_change, e7.crude_pop_change) AS crude_pop_change,
        -- COALESCE(e1.gdp_per_capita, e7.gdp_per_capita) AS gdp_per_capita,
        -- COALESCE(e1.gdppc_country, e7.gdppc_country) AS gdppc_country,
        -- COALESCE(e1.population_density, e7.population_density) AS population_density,
        -- COALESCE(e1.unemployment_rate, e7.unemployment_rate) AS unemployment_rate,
        -- COALESCE(e1.gdppc_nuts2, e7.gdppc_nuts2) AS gdppc_nuts2,
        COALESCE(e1.NUTS1, e7.NUTS1) AS NUTS1
    FROM ess1 e1
    FULL JOIN ess7 e7
    ON e1.Country = e7.Country AND e1.Year = e7.Year AND e1.NUTS2 = e7.NUTS2 AND e1.idno = e7.idno
)

-- Step 5: Return Merged Data
SELECT * FROM merged_ess;










-- -- Merge All ESS Data and Eurostat Data

-- WITH perceived_cultural_threat AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep values between 0 and 10
--         CASE WHEN SAFE_CAST(imwbcrm AS INT64) BETWEEN 0 AND 10 THEN imwbcrm ELSE NULL END AS crime_impact,
--         CASE WHEN SAFE_CAST(imueclt AS INT64) BETWEEN 0 AND 10 THEN imueclt ELSE NULL END AS cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- perceived_economic_competition AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep values between 0 and 10
--         CASE WHEN SAFE_CAST(imbgeco AS INT64) BETWEEN 0 AND 10 THEN imbgeco ELSE NULL END AS immigration_economy,
--         CASE WHEN SAFE_CAST(imbleco AS INT64) BETWEEN 0 AND 10 THEN imbleco ELSE NULL END AS perceived_economic_competition,
--         CASE WHEN SAFE_CAST(imtcjob AS INT64) BETWEEN 0 AND 10 THEN imtcjob ELSE NULL END AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- social_contact AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep valid values in the expected range
--         CASE WHEN SAFE_CAST(dfegcon AS INT64) BETWEEN 1 AND 7 THEN dfegcon ELSE NULL END AS contact_frequency,
--         CASE WHEN SAFE_CAST(dfegcf AS INT64) BETWEEN 1 AND 3 THEN dfegcf ELSE NULL END AS contact_quality,
--         CASE WHEN SAFE_CAST(dfeghbg AS INT64) BETWEEN 0 AND 10 THEN dfeghbg ELSE NULL END AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,
--         SAFE_CAST(inwyye AS INT64) AS Year,
        
--          -- Child living at home (1: yes, 2: no)
--         CASE 
--             WHEN SAFE_CAST(chldhm AS INT64) IN (1, 2) THEN SAFE_CAST(chldhm AS INT64)
--             ELSE NULL
--         END AS chldhm,

--         -- Immigrants make country worse or better (scale 0 to 10)
--         CASE 
--             WHEN SAFE_CAST(imwbcnt AS INT64) BETWEEN 0 AND 10 THEN SAFE_CAST(imwbcnt AS INT64)
--             ELSE NULL 
--         END AS imwbcnt,
--         -- Create far-right support: 1 if lrscale >= 8, 0 otherwise
--         CASE 
--             WHEN SAFE_CAST(lrscale AS INT64) BETWEEN 0 AND 10 THEN lrscale 
--             ELSE NULL 
--         END AS lrscale,

--         CASE 
--             WHEN SAFE_CAST(lrscale AS INT64) >= 8 THEN 1 
--             ELSE 0 
--         END AS far_right_support,

--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_dependentvariable`
-- ),

-- ess_socio AS (
--     SELECT 
--         TRIM(UPPER(nuts2)) AS NUTS2,
--         idno,
--         TRIM(UPPER(country)) AS Country,
--         SAFE_CAST(year AS INT64) AS Year,
--         agea AS age,
--         gndr,
--         eisced,

--         -- Apply filtering for household income (hinctnta: 1-10)
--         CASE WHEN SAFE_CAST(hinctnta AS INT64) BETWEEN 1 AND 10 THEN hinctnta ELSE NULL END AS hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.contact_frequency,
--         s.contact_quality,
--         s.contact_perception,
--         e.immigration_economy,
--         e.perceived_economic_competition,
--         e.jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         d.lrscale,
--         d.far_right_support,
--         d.imwbcnt,
--         d.chldhm,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,

--         -- Create `immig_background` Column
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country 
--         AND p.NUTS2 = s.NUTS2 
--         AND p.Year = s.Year 
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country 
--         AND p.NUTS2 = e.NUTS2 
--         AND p.Year = e.Year 
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country 
--         AND p.NUTS2 = d.NUTS2 
--         AND p.Year = d.Year 
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio 
--         ON p.Country = socio.Country 
--         AND p.NUTS2 = socio.NUTS2 
--         AND p.Year = socio.Year 
--         AND p.idno = socio.idno
-- ),

-- final_merged_data AS (
--     -- Merge ESS Data with Eurostat Data
--     SELECT 
--         ess.*,  
--         eu.crude_net_migration,
--         eu.crude_pop_change,
--         eu.gdp_per_capita,
--         eu.gdppc_country,
--         eu.population_density,
--         eu.unemployment_rate,
--         eu.gdppc_nuts2,

--         -- Extract first 3 characters of NUTS2 to create NUTS1
--         SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1
--     FROM combined_data ess
--     LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu 
--         ON ess.Country = eu.Country 
--         AND ess.NUTS2 = eu.NUTS2 
--         AND ess.Year = eu.Year
-- )

-- -- Save the final merged dataset
-- SELECT * FROM final_merged_data;










-- -- Merge All ESS Data and Eurostat Data
-- --CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep values between 0 and 10
--         CASE WHEN SAFE_CAST(imwbcrm AS INT64) BETWEEN 0 AND 10 THEN imwbcrm ELSE NULL END AS crime_impact,
--         CASE WHEN SAFE_CAST(imueclt AS INT64) BETWEEN 0 AND 10 THEN imueclt ELSE NULL END AS cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imwbcrm AS INT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imueclt AS INT64) BETWEEN 0 AND 10
-- ),

-- perceived_economic_competition AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep values between 0 and 10
--         CASE WHEN SAFE_CAST(imbgeco AS INT64) BETWEEN 0 AND 10 THEN imbgeco ELSE NULL END AS immigration_economy,
--         CASE WHEN SAFE_CAST(imbleco AS INT64) BETWEEN 0 AND 10 THEN imbleco ELSE NULL END AS perceived_economic_competition,
--         CASE WHEN SAFE_CAST(imtcjob AS INT64) BETWEEN 0 AND 10 THEN imtcjob ELSE NULL END AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imbgeco AS INT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imbleco AS INT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imtcjob AS INT64) BETWEEN 0 AND 10
-- ),

-- social_contact AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep valid values in the expected range
--         CASE WHEN SAFE_CAST(dfegcon AS INT64) BETWEEN 1 AND 7 THEN dfegcon ELSE NULL END AS contact_frequency,
--         CASE WHEN SAFE_CAST(dfegcf AS INT64) BETWEEN 1 AND 3 THEN dfegcf ELSE NULL END AS contact_quality,
--         CASE WHEN SAFE_CAST(dfeghbg AS INT64) BETWEEN 0 AND 10 THEN dfeghbg ELSE NULL END AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(dfegcon AS INT64) BETWEEN 1 AND 7
--       AND SAFE_CAST(dfegcf AS INT64) BETWEEN 1 AND 3
--       AND SAFE_CAST(dfeghbg AS INT64) BETWEEN 0 AND 10
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         SAFE_CAST(lrscale AS INT64) BETWEEN 0 AND 10 THEN party_support,
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_socio_farright`
--     WHERE LENGTH(region) = 4 OR LENGTH(region) = 5
-- ),

-- ess_socio AS (
--     SELECT 
--         TRIM(UPPER(nuts2)) AS NUTS2,
--         idno,
--         TRIM(UPPER(country)) AS Country,
--         SAFE_CAST(year AS INT64) AS Year,
--         agea AS age,
--         gndr,
--         eisced,

--         -- Apply filtering for household income (hinctnta: 1-10)
--         CASE WHEN SAFE_CAST(hinctnta AS INT64) BETWEEN 1 AND 10 THEN hinctnta ELSE NULL END AS hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
--     WHERE LENGTH(nuts2) = 4 OR LENGTH(nuts2) = 5
--       AND SAFE_CAST(hinctnta AS INT64) BETWEEN 1 AND 10
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.contact_frequency,
--         s.contact_quality,
--         s.contact_perception,
--         e.immigration_economy,
--         e.perceived_economic_competition,
--         e.jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         d.party_support,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,


--         -- Create `immig_background` Column
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country
--         AND p.NUTS2 = d.NUTS2
--         AND p.Year = d.Year
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio
--         ON p.Country = socio.Country
--         AND p.NUTS2 = socio.NUTS2
--         AND p.Year = socio.Year
--         AND p.idno = socio.idno
-- ),

-- final_merged_data AS (
--     -- Merge ESS Data with Eurostat Data and Add NUTS1 Column
--     SELECT 
--         ess.*,  

--         -- Add Eurostat data
--         eu.crude_net_migration,
--         eu.crude_pop_change,
--         eu.gdp_per_capita,
--         eu.gdppc_country,
--         eu.population_density,
--         eu.unemployment_rate,
--         eu.gdppc_nuts2,

--         -- Extract first 3 characters of NUTS2 to create NUTS1
--         SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1

--     FROM combined_data ess
--     LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--         ON ess.Country = eu.Country
--         AND ess.NUTS2 = eu.NUTS2
--         AND ess.Year = eu.Year
-- )

-- -- Save the final merged dataset
-- SELECT * FROM final_merged_data;








-- -- Merge All ESS Data and Eurostat Data
-- --CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep values between 0 and 10
--         CASE WHEN SAFE_CAST(imwbcrm AS INT64) BETWEEN 0 AND 10 THEN imwbcrm ELSE NULL END AS crime_impact,
--         CASE WHEN SAFE_CAST(imueclt AS INT64) BETWEEN 0 AND 10 THEN imueclt ELSE NULL END AS cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imwbcrm AS INT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imueclt AS INT64) BETWEEN 0 AND 10
-- ),

-- perceived_economic_competition AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep values between 0 and 10
--         CASE WHEN SAFE_CAST(imbgeco AS INT64) BETWEEN 0 AND 10 THEN imbgeco ELSE NULL END AS immigration_economy,
--         CASE WHEN SAFE_CAST(imbleco AS INT64) BETWEEN 0 AND 10 THEN imbleco ELSE NULL END AS perceived_economic_competition,
--         CASE WHEN SAFE_CAST(imtcjob AS INT64) BETWEEN 0 AND 10 THEN imtcjob ELSE NULL END AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imbgeco AS INT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imbleco AS INT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imtcjob AS INT64) BETWEEN 0 AND 10
-- ),

-- social_contact AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,

--         -- Apply filtering: Only keep valid values in the expected range
--         CASE WHEN SAFE_CAST(dfegcon AS INT64) BETWEEN 1 AND 7 THEN dfegcon ELSE NULL END AS contact_frequency,
--         CASE WHEN SAFE_CAST(dfegcf AS INT64) BETWEEN 1 AND 3 THEN dfegcf ELSE NULL END AS contact_quality,
--         CASE WHEN SAFE_CAST(dfeghbg AS INT64) BETWEEN 0 AND 10 THEN dfeghbg ELSE NULL END AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(dfegcon AS INT64) BETWEEN 1 AND 7
--       AND SAFE_CAST(dfegcf AS INT64) BETWEEN 1 AND 3
--       AND SAFE_CAST(dfeghbg AS INT64) BETWEEN 0 AND 10
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_demographics`
--     WHERE LENGTH(region) = 4 OR LENGTH(region) = 5
-- ),

-- ess_socio AS (
--     SELECT 
--         TRIM(UPPER(nuts2)) AS NUTS2,
--         idno,
--         TRIM(UPPER(country)) AS Country,
--         SAFE_CAST(year AS INT64) AS Year,
--         agea AS age,
--         gndr,
--         eisced,
--         hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
--     WHERE LENGTH(nuts2) = 4 OR LENGTH(nuts2) = 5
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.contact_frequency,
--         s.contact_quality,
--         s.contact_perception,
--         e.immigration_economy,
--         e.perceived_economic_competition,
--         e.jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,

--         -- Create `immig_background` Column
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country
--         AND p.NUTS2 = d.NUTS2
--         AND p.Year = d.Year
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio
--         ON p.Country = socio.Country
--         AND p.NUTS2 = socio.NUTS2
--         AND p.Year = socio.Year
--         AND p.idno = socio.idno
-- ),

-- final_merged_data AS (
--     -- Merge ESS Data with Eurostat Data and Add NUTS1 Column
--     SELECT 
--         ess.*,  

--         -- Add Eurostat data
--         eu.crude_net_migration,
--         eu.crude_pop_change,
--         eu.gdp_per_capita,
--         eu.gdppc_country,
--         eu.population_density,
--         eu.unemployment_rate,
--         eu.gdppc_nuts2,

--         -- Extract first 3 characters of NUTS2 to create NUTS1
--         SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1

--     FROM combined_data ess
--     LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--         ON ess.Country = eu.Country
--         AND ess.NUTS2 = eu.NUTS2
--         AND ess.Year = eu.Year
-- )

-- -- Save the final merged dataset
-- SELECT * FROM final_merged_data;









-- -- Merge All ESS Data and Eurostat Data
-- --CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imwbcrm AS crime_impact,
--         imueclt AS cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- perceived_economic_competition AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imbgeco AS immigration_economy,
--         imbleco AS perceived_economic_competition,
--         imtcjob AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE imbgeco IS NOT NULL OR imbleco IS NOT NULL OR imtcjob IS NOT NULL
-- ),

-- social_contact AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         dfegcon AS contact_frequency,
--         dfegcf AS contact_quality,
--         dfeghbg AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE dfegcon IS NOT NULL OR dfegcf IS NOT NULL OR dfeghbg IS NOT NULL
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_demographics`
--     WHERE LENGTH(region) = 4 OR LENGTH(region) = 5
-- ),

-- ess_socio AS (
--     SELECT 
--         TRIM(UPPER(nuts2)) AS NUTS2,
--         idno,
--         TRIM(UPPER(country)) AS Country,
--         SAFE_CAST(year AS INT64) AS Year,
--         agea AS age,
--         gndr,
--         eisced,
--         hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
--     WHERE LENGTH(nuts2) = 4 OR LENGTH(nuts2) = 5
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.contact_frequency,
--         s.contact_quality,
--         s.contact_perception,
--         e.immigration_economy,
--         e.perceived_economic_competition,
--         e.jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,

--         -- Create `immig_background` Column
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country
--         AND p.NUTS2 = d.NUTS2
--         AND p.Year = d.Year
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio
--         ON p.Country = socio.Country
--         AND p.NUTS2 = socio.NUTS2
--         AND p.Year = socio.Year
--         AND p.idno = socio.idno
-- ),

-- final_merged_data AS (
--     -- Merge ESS Data with Eurostat Data and Add NUTS1 Column
--     SELECT 
--         ess.*,  -- Keep all ESS data

--         -- Add Eurostat data
--         eu.crude_net_migration,
--         eu.crude_pop_change,
--         eu.gdp_per_capita,
--         eu.gdppc_country,
--         eu.population_density,
--         eu.unemployment_rate,
--         eu.gdppc_nuts2,

--         -- Extract first 3 characters of NUTS2 to create NUTS1
--         SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1

--     FROM combined_data ess
--     LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--         ON ess.Country = eu.Country
--         AND ess.NUTS2 = eu.NUTS2
--         AND ess.Year = eu.Year
-- )

-- -- Save the final merged dataset
-- SELECT * FROM final_merged_data;










-- WITH perceived_cultural_threat AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imwbcrm AS crime_impact,
--         imueclt AS cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- perceived_economic_competition AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imbgeco AS immigration_economy,
--         imbleco AS perceived_economic_competition,
--         imtcjob AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE imbgeco IS NOT NULL OR imbleco IS NOT NULL OR imtcjob IS NOT NULL
-- ),

-- combined_data AS (
--     SELECT 
--         p.*,
--         COALESCE(e.immigration_economy) AS immigration_economy,
--         COALESCE(e.perceived_economic_competition) AS perceived_economic_competition,
--         COALESCE(e.jobs_taken) AS jobs_taken
--     FROM perceived_cultural_threat p
--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno
-- )

-- -- Save the final merged dataset
-- SELECT * FROM combined_data;










-- -- Merge All ESS Data into a Final Table
-- -- CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_complete_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT 
--         TRIM(UPPER(cntry)) AS Country,  
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imwbcrm AS crime_impact,
--         imueclt AS cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- social_contact AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         dfegcon AS contact_frequency,
--         dfegcf AS contact_quality,
--         dfeghbg AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE dfegcon IS NOT NULL OR dfegcf IS NOT NULL OR dfeghbg IS NOT NULL
-- ),

-- perceived_economic_competition AS (
--     SELECT DISTINCT
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imbgeco AS immigration_economy,
--         imbleco AS perceived_economic_competition,
--         imtcjob AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE imbgeco IS NOT NULL OR imbleco IS NOT NULL OR imtcjob IS NOT NULL
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_demographics`
--     WHERE LENGTH(region) = 4 OR LENGTH(region) = 5
-- ),

-- ess_socio AS (
--     SELECT 
--         TRIM(UPPER(nuts2)) AS NUTS2,
--         idno,
--         TRIM(UPPER(country)) AS Country,
--         SAFE_CAST(year AS INT64) AS Year,
--         agea AS age,
--         gndr,
--         eisced,
--         hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
--     WHERE LENGTH(nuts2) = 4 OR LENGTH(nuts2) = 5
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.contact_frequency,
--         s.contact_quality,
--         s.contact_perception,
--         e.immigration_economy,
--         e.perceived_economic_competition,
--         e.jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,

--         -- Create `immig_background` Column
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country
--         AND p.NUTS2 = d.NUTS2
--         AND p.Year = d.Year
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio
--         ON p.Country = socio.Country
--         AND p.NUTS2 = socio.NUTS2
--         AND p.Year = socio.Year
--         AND p.idno = socio.idno
-- )

-- -- Save the merged dataset in BigQuery, keeping all necessary columns
-- SELECT * 
-- FROM combined_data;

-- -- Merge ESS Data with Eurostat Data and Add NUTS1 Column
-- --CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- SELECT 
--     ess.*,  -- Keep all ESS data

--     -- Add Eurostat data
--     eu.crude_net_migration,
--     eu.crude_pop_change,
--     eu.gdp_per_capita,
--     eu.gdppc_country,
--     eu.population_density,
--     eu.unemployment_rate,
--     eu.gdppc_nuts2,

--     -- Extract first 3 characters of NUTS2 to create NUTS1
--     SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1

-- FROM `thesis-project-451520.thesis_data.ess_complete_merged` ess
-- LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--     ON ess.Country = eu.Country
--     AND ess.NUTS2 = eu.NUTS2
--     AND ess.Year = eu.Year;







-- -- Merge All ESS Data into a Final Table
-- --CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_complete_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imwbcrm AS crime_impact,
--         imueclt AS cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- social_contact AS (
--     -- Ensure column names are properly aligned for merging
--     SELECT 
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         dfegcon AS contact_frequency,
--         dfegcf AS contact_quality,
--         dfeghbg AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE dfegcon IS NOT NULL OR dfegcf IS NOT NULL OR dfeghbg IS NOT NULL
-- ),

-- perceived_economic_competition AS (
--     -- Economic competition-related variables
--     SELECT 
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imbgeco AS immigration_economy,
--         imbleco AS perceived_economic_competition,
--         imtcjob AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE imbgeco IS NOT NULL OR imbleco IS NOT NULL OR imtcjob IS NOT NULL
-- ),

-- demographics AS (
--     -- Demographic information
--     SELECT 
--         idno,
--         cntry AS Country,
--         region AS NUTS2,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_demographics`
--     WHERE LENGTH(region) = 4 OR LENGTH(region) = 5
-- ),

-- ess_socio AS (
--     -- Socioeconomic variables
--     SELECT 
--         nuts2 AS NUTS2,
--         idno,
--         country AS Country,
--         SAFE_CAST(year AS INT64) AS Year,
--         agea AS age,
--         gndr,
--         eisced,
--         hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
--     WHERE LENGTH(nuts2) = 4 OR LENGTH(nuts2) = 5
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.contact_frequency,
--         s.contact_quality,
--         s.contact_perception,
--         e.immigration_economy,
--         e.perceived_economic_competition,
--         e.jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,

--         -- Create `immig_background` Column
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country
--         AND p.NUTS2 = d.NUTS2
--         AND p.Year = d.Year
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio
--         ON p.Country = socio.Country
--         AND p.NUTS2 = socio.NUTS2
--         AND p.Year = socio.Year
--         AND p.idno = socio.idno
-- )

-- -- Save the merged dataset in BigQuery, keeping all necessary columns
-- SELECT * 
-- FROM combined_data;

-- Merge ESS Data with Eurostat Data and Add NUTS1 Column
--CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- SELECT 
--     ess.*,  -- Keep all ESS data

--     -- Add Eurostat data
--     eu.crude_net_migration,
--     eu.crude_pop_change,
--     eu.gdp_per_capita,
--     eu.gdppc_country,
--     eu.population_density,
--     eu.unemployment_rate,
--     eu.gdppc_nuts2,

--     -- Extract first 3 characters of NUTS2 to create NUTS1
--     SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1

-- FROM `thesis-project-451520.thesis_data.ess_complete_merged` ess
-- LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--     ON ess.Country = eu.Country
--     AND ess.NUTS2 = eu.NUTS2
--     AND ess.Year = eu.Year;






-- -- Merge All ESS Data into a Final Table
-- -- CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_complete_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         imwbcrm AS crime_impact,
--         imueclt AS cultural_impact,
--     FROM `thesis-project-451520.thesis_data.ESS`
-- ),

-- social_contact AS (
--     -- Fix column names so they align properly in the join
--     SELECT 
--         TRIM(UPPER(cntry)) AS Country,
--         TRIM(UPPER(region)) AS NUTS2,  
--         idno,
--         SAFE_CAST(inwyye AS INT64) AS Year,
--         dfegcon AS contact_frequency,
--         dfegcf AS contact_quality,
--         dfeghbg AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE dfegcon IS NOT NULL OR dfegcf IS NOT NULL OR dfeghbg IS NOT NULL
-- ),

-- combined_data AS (
--     -- Merge perceived_cultural_threat with social_contact
--     SELECT 
--         p.*,
--         s.contact_frequency,  -- Keep NULLs instead of replacing with -1  
--         s.contact_quality,
--         s.contact_perception
--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s  -- Now matches correctly
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno
-- )

-- -- Save the merged dataset in BigQuery
-- SELECT * 
-- FROM combined_data;








-- -- Merge All ESS Data into a Final Table
-- CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_complete_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         imwbcrm AS crime_impact,
--         imueclt AS cultural_impact,
--         imdetmr AS hostility_marriage,
--         imdetbs AS hostility_boss
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imwbcrm AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imueclt AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imdetmr AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imdetbs AS FLOAT64) BETWEEN 0 AND 10
-- ),

-- social_contact AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         dfegcon AS contact_frequency,
--         dfegcf AS contact_quality,
--         dfeghbg AS contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(dfegcon AS FLOAT64) BETWEEN 1 AND 7
--       AND SAFE_CAST(dfegcf AS FLOAT64) BETWEEN 1 AND 3
--       AND SAFE_CAST(dfeghbg AS FLOAT64) BETWEEN 0 AND 10
-- ),

-- perceived_economic_competition AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         imbgeco AS immigration_economy,
--         imbleco AS perceived_economic_competition,
--         imtcjob AS jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imbgeco AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imbleco AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imtcjob AS FLOAT64) BETWEEN 0 AND 10
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         cntry AS Country,
--         region AS NUTS2,
--         inwyye AS Year,
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_demographics`
--     WHERE LENGTH(region) = 4 OR LENGTH(region) = 5
-- ),

-- ess_socio AS (
--     SELECT 
--         nuts2 AS NUTS2,
--         idno,
--         country AS Country,
--         year AS Year,
--         agea AS age,
--         gndr,
--         eisced,
--         hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
--     WHERE LENGTH(nuts2) = 4 OR LENGTH(nuts2) = 5
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.contact_frequency,
--         s.contact_quality,
--         s.contact_perception,
--         e.immigration_economy,
--         e.perceived_economic_competition,
--         e.jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,

--         -- Create `immig_background` Column
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country
--         AND p.NUTS2 = d.NUTS2
--         AND p.Year = d.Year
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio
--         ON p.Country = socio.Country
--         AND p.NUTS2 = socio.NUTS2
--         AND p.Year = socio.Year
--         AND p.idno = socio.idno
-- )

-- -- Save the merged dataset in BigQuery, dropping unnecessary columns
-- SELECT * -- EXCEPT(born_in_country, mother_born_in_country, father_born_in_country)
-- FROM combined_data;


-- -- Merge ESS Data with Eurostat Data and Add NUTS1 Column
-- --CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- SELECT 
--     ess.*,  -- Keep all ESS data

--     -- Add Eurostat data
--     eu.crude_net_migration,
--     eu.crude_pop_change,
--     eu.gdp_per_capita,
--     eu.gdppc_country,
--     eu.population_density,
-- --    eu.total_population_change,
--     eu.unemployment_rate,
--     eu.gdppc_nuts2,

--     -- Extract first 3 characters of NUTS2 to create NUTS1
--     SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1

-- FROM `thesis-project-451520.thesis_data.ess_complete_merged` ess
-- LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--     ON ess.Country = eu.Country
--     AND ess.NUTS2 = eu.NUTS2
--     AND ess.Year = eu.Year;






-- -- Merge ESS Data with Eurostat Data and Add NUTS1 Column
-- CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- SELECT 
--     ess.*,  -- Keep all ESS data

--     -- Add Eurostat data
--     eu.crude_net_migration,
--     eu.crude_pop_change,
--     eu.gdp_per_capita,
--     eu.gdppc_country,
--     eu.population_density,
-- --    eu.total_population_change,
--     eu.unemployment_rate,
--     eu.gdppc_nuts2,

--     -- Extract first 3 characters of NUTS2 to create NUTS1
--     SUBSTRING(ess.NUTS2, 1, 3) AS NUTS1

-- FROM `thesis-project-451520.thesis_data.ess_complete_merged` ess
-- LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--     ON ess.Country = eu.Country
--     AND ess.NUTS2 = eu.NUTS2
--     AND ess.Year = eu.Year;







-- -- Merge ESS Data with Eurostat Data
-- CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_eurostat_merged` AS

-- SELECT 
--     ess.*,  -- Keep all ESS data
    
--     -- Add Eurostat data
--     eu.crude_net_migration,
--     eu.crude_pop_change,
--     eu.gdp_per_capita,
--     eu.gdppc_country,
--     eu.population_density,
-- --    eu.total_population_change,
--     eu.unemployment_rate,
--     eu.gdppc_nuts2

-- FROM `thesis-project-451520.thesis_data.ess_complete_merged` ess
-- LEFT JOIN `thesis-project-451520.thesis_data.merged_eurostat` eu
--     ON ess.Country = eu.Country
--     AND ess.NUTS2 = eu.NUTS2
--     AND ess.Year = eu.Year;

-- Merge All ESS Data into a Final Table
-- CREATE OR REPLACE TABLE `thesis-project-451520.thesis_data.ess_complete_merged` AS

-- WITH perceived_cultural_threat AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         AVG(CAST(imwbcrm AS FLOAT64)) AS mean_crime_impact,
--         AVG(CAST(imueclt AS FLOAT64)) AS mean_cultural_impact,
--         AVG(CAST(imdetmr AS FLOAT64)) AS mean_hostility_marriage,
--         AVG(CAST(imdetbs AS FLOAT64)) AS mean_hostility_boss
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imwbcrm AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imueclt AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imdetmr AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imdetbs AS FLOAT64) BETWEEN 0 AND 10
--     GROUP BY Country, NUTS2, idno, Year
-- ),

-- social_contact AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         AVG(CAST(dfegcon AS FLOAT64)) AS mean_contact_frequency,
--         AVG(CAST(dfegcf AS FLOAT64)) AS mean_contact_quality,
--         AVG(CAST(dfeghbg AS FLOAT64)) AS mean_contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(dfegcon AS FLOAT64) BETWEEN 1 AND 7
--       AND SAFE_CAST(dfegcf AS FLOAT64) BETWEEN 1 AND 3
--       AND SAFE_CAST(dfeghbg AS FLOAT64) BETWEEN 0 AND 10
--     GROUP BY Country, NUTS2, idno, Year
-- ),

-- perceived_economic_competition AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         AVG(CAST(imbgeco AS FLOAT64)) AS mean_immigration_economy,
--         AVG(CAST(imbleco AS FLOAT64)) AS perceived_economic_competition,
--         AVG(CAST(imtcjob AS FLOAT64)) AS mean_jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imbgeco AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imbleco AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imtcjob AS FLOAT64) BETWEEN 0 AND 10
--     GROUP BY Country, NUTS2, idno, Year
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         cntry AS Country,
--         region AS NUTS2,
--         inwyye AS Year,

--         -- Immigrant Background
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,

--         -- Rural or Urban Background
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,

--         -- Minority Presence in Area
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence

--     FROM `thesis-project-451520.thesis_data.ess_demographics`
--     WHERE LENGTH(region) = 4 OR LENGTH(region) = 5
-- ),

-- ess_socio AS (
--     SELECT 
--         nuts2 AS NUTS2,
--         idno,
--         country AS Country,
--         year AS Year,
--         agea AS age,
--         gndr,
--         eisced,
--         hinctnta
--     FROM `thesis-project-451520.thesis_data.ess_socio`
--     WHERE LENGTH(nuts2) = 4 OR LENGTH(nuts2) = 5
-- ),

-- combined_data AS (
--     -- Merge All ESS Datasets on Country, NUTS2, Year, and IDNO
--     SELECT 
--         p.*,
--         s.mean_contact_frequency,
--         s.mean_contact_quality,
--         s.mean_contact_perception,
--         e.mean_immigration_economy,
--         e.perceived_economic_competition,
--         e.mean_jobs_taken,
--         d.rural_urban,
--         d.minority_presence,
--         socio.age,
--         socio.gndr,
--         socio.eisced,
--         socio.hinctnta,

--         -- Create `immig_background` Column: 
--         -- If born in country, mother born in country, father born in country = 1 → Set to 0
--         -- If any of them is 2 → Set to 1
--         CASE 
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.mother_born_in_country AS INT64) = 1 
--                  AND SAFE_CAST(d.father_born_in_country AS INT64) = 1 
--             THEN 0
--             WHEN SAFE_CAST(d.born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.mother_born_in_country AS INT64) = 2 
--                  OR SAFE_CAST(d.father_born_in_country AS INT64) = 2 
--             THEN 1
--             ELSE NULL 
--         END AS immig_background,

--         -- Check if unemployment already exists; if not, recalculate it
--         COALESCE(
--             d.unemployment,  
--             CASE 
--                 WHEN SAFE_CAST(d.unemployed AS INT64) = 1 OR SAFE_CAST(d.looking_for_job AS INT64) = 1 THEN 1
--                 WHEN SAFE_CAST(d.unemployed AS INT64) = 0 AND SAFE_CAST(d.looking_for_job AS INT64) = 0 THEN 0
--                 ELSE NULL 
--             END
--         ) AS unemployment

--     FROM perceived_cultural_threat p
--     LEFT JOIN social_contact s 
--         ON p.Country = s.Country
--         AND p.NUTS2 = s.NUTS2
--         AND p.Year = s.Year
--         AND p.idno = s.idno

--     LEFT JOIN perceived_economic_competition e 
--         ON p.Country = e.Country
--         AND p.NUTS2 = e.NUTS2
--         AND p.Year = e.Year
--         AND p.idno = e.idno

--     LEFT JOIN demographics d 
--         ON p.Country = d.Country
--         AND p.NUTS2 = d.NUTS2
--         AND p.Year = d.Year
--         AND p.idno = d.idno

--     LEFT JOIN ess_socio socio
--         ON p.Country = socio.Country
--         AND p.NUTS2 = socio.NUTS2
--         AND p.Year = socio.Year
--         AND p.idno = socio.idno
-- )

-- -- Save the merged dataset in BigQuery, dropping unnecessary columns
-- SELECT * EXCEPT(born_in_country, mother_born_in_country, father_born_in_country, unemployed, looking_for_job)
-- FROM combined_data;








-- WITH perceived_cultural_threat AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         AVG(CAST(imwbcrm AS FLOAT64)) AS mean_crime_impact,
--         AVG(CAST(imueclt AS FLOAT64)) AS mean_cultural_impact,
--         AVG(CAST(imdetmr AS FLOAT64)) AS mean_hostility_marriage,
--         AVG(CAST(imdetbs AS FLOAT64)) AS mean_hostility_boss
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imwbcrm AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imueclt AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imdetmr AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imdetbs AS FLOAT64) BETWEEN 0 AND 10
--     GROUP BY Country, NUTS2, idno, Year
-- ),

-- social_contact AS (
--     SELECT 
--         region AS NUTS2,
--         AVG(CAST(dfegcon AS FLOAT64)) AS mean_contact_frequency,
--         AVG(CAST(dfegcf AS FLOAT64)) AS mean_contact_quality,
--         AVG(CAST(dfeghbg AS FLOAT64)) AS mean_contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(dfegcon AS FLOAT64) BETWEEN 1 AND 7
--       AND SAFE_CAST(dfegcf AS FLOAT64) BETWEEN 1 AND 3
--       AND SAFE_CAST(dfeghbg AS FLOAT64) BETWEEN 0 AND 10
--     GROUP BY NUTS2
-- ),

-- perceived_economic_competition AS (
--     SELECT 
--         region AS NUTS2,
--         AVG(CAST(imbgeco AS FLOAT64)) AS mean_immigration_economy,
--         AVG(CAST(imbleco AS FLOAT64)) AS perceived_economic_competition,
--         AVG(CAST(imtcjob AS FLOAT64)) AS mean_jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imbgeco AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imbleco AS FLOAT64) BETWEEN 0 AND 10
--       AND SAFE_CAST(imtcjob AS FLOAT64) BETWEEN 0 AND 10
--     GROUP BY NUTS2
-- ),

-- demographics AS (
--     SELECT 
--         idno,
--         cntry AS Country,
--         region AS NUTS2,
--         inwyye AS Year,

--         -- Immigrant Background
--         CASE WHEN SAFE_CAST(brncntr AS INT64) IN (1,2) THEN brncntr ELSE NULL END AS born_in_country,
--         CASE WHEN SAFE_CAST(mocntr AS INT64) IN (1,2) THEN mocntr ELSE NULL END AS mother_born_in_country,
--         CASE WHEN SAFE_CAST(facntr AS INT64) IN (1,2) THEN facntr ELSE NULL END AS father_born_in_country,

--         -- Unemployment Status
--         CASE WHEN SAFE_CAST(uempla AS INT64) IN (0,1) THEN uempla ELSE NULL END AS unemployed,
--         CASE WHEN SAFE_CAST(uempli AS INT64) IN (0,1) THEN uempli ELSE NULL END AS looking_for_job,

--         -- Rural or Urban Background
--         CASE WHEN SAFE_CAST(domicil AS INT64) BETWEEN 1 AND 5 THEN domicil ELSE NULL END AS rural_urban,
--         -- Minority Presence in Area
--         CASE WHEN SAFE_CAST(acetalv AS INT64) BETWEEN 1 AND 3 THEN acetalv ELSE NULL END AS minority_presence
--     FROM `thesis-project-451520.thesis_data.ess_demographics`
-- )

-- -- Final Merge
-- SELECT 
--     p.Country,
--     p.NUTS2,
--     p.idno,
--     p.Year,

--     -- Perceived Cultural Threat Data
--     p.mean_crime_impact,
--     p.mean_cultural_impact,
--     p.mean_hostility_marriage,
--     p.mean_hostility_boss,

--     -- Social Contact Data
--     s.mean_contact_frequency,
--     s.mean_contact_quality,
--     s.mean_contact_perception,

--     -- Perceived Economic Competition Data
--     e.mean_immigration_economy,
--     e.perceived_economic_competition,
--     e.mean_jobs_taken,

--     -- Demographics Data
--     d.born_in_country,
--     d.mother_born_in_country,
--     d.father_born_in_country,
--     d.unemployed,
--     d.looking_for_job,
--     d.rural_urban,
--     d.minority_presence  -- Added acetalv variable

-- FROM perceived_cultural_threat p
-- LEFT JOIN social_contact s 
--     ON p.NUTS2 = s.NUTS2
--     AND p.Year = s.Year  -- Ensure merging on the same Year

-- LEFT JOIN perceived_economic_competition e 
--     ON p.NUTS2 = e.NUTS2
--     AND p.Year = e.Year  -- Ensure merging on the same Year

-- LEFT JOIN demographics d 
--     ON p.Country = d.Country
--     AND p.NUTS2 = d.NUTS2
--     AND p.Year = d.Year;  -- Ensure merging correctly on Country, NUTS2, and Year



-- WITH outgroup_hostility AS (
--     SELECT 
--         cntry AS Country,
--         region AS NUTS2,
--         idno,
--         inwyye AS Year,
--         AVG(CAST(imbgeco AS FLOAT64)) AS mean_immigration_economy,
--         AVG(CAST(imwbcrm AS FLOAT64)) AS mean_crime_impact,
--         AVG(CAST(imbleco AS FLOAT64)) AS mean_economic_contribution,
--         AVG(CAST(imdetmr AS FLOAT64)) AS mean_hostility_marriage,
--         AVG(CAST(imdetbs AS FLOAT64)) AS mean_hostility_boss,
--         AVG(CAST(imueclt AS FLOAT64)) AS mean_cultural_impact
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(imbgeco AS FLOAT64) IS NOT NULL  
--       AND SAFE_CAST(imwbcrm AS FLOAT64) IS NOT NULL
--       AND SAFE_CAST(imbleco AS FLOAT64) IS NOT NULL
--       AND SAFE_CAST(imdetmr AS FLOAT64) IS NOT NULL
--       AND SAFE_CAST(imdetbs AS FLOAT64) IS NOT NULL
--       AND SAFE_CAST(imueclt AS FLOAT64) IS NOT NULL
--     GROUP BY Country, NUTS2, idno, Year
-- ),

-- social_contact AS (
--     SELECT 
--         region AS NUTS2,
--         AVG(CAST(dfegcon AS FLOAT64)) AS mean_contact_frequency,
--         AVG(CAST(dfegcf AS FLOAT64)) AS mean_contact_quality,
--         AVG(CAST(dfeghbg AS FLOAT64)) AS mean_contact_perception
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(dfegcon AS FLOAT64) IS NOT NULL
--       AND SAFE_CAST(dfegcf AS FLOAT64) IS NOT NULL
--       AND SAFE_CAST(dfeghbg AS FLOAT64) IS NOT NULL
--     GROUP BY NUTS2
-- ),

-- economic_competition AS (
--     SELECT 
--         region AS NUTS2,
--         AVG(CAST(eimpcnt AS FLOAT64)) AS mean_job_competition,
--         AVG(CAST(imtcjob AS FLOAT64)) AS mean_jobs_taken
--     FROM `thesis-project-451520.thesis_data.ESS`
--     WHERE SAFE_CAST(eimpcnt AS FLOAT64) IS NOT NULL
--       AND SAFE_CAST(imtcjob AS FLOAT64) IS NOT NULL
--     GROUP BY NUTS2
-- )

-- -- Final Merge
-- SELECT 
--     o.Country,
--     o.NUTS2,
--     o.idno,
--     o.Year,

--     -- Outgroup Hostility Data
--     o.mean_immigration_economy,
--     o.mean_crime_impact,
--     o.mean_economic_contribution,
--     o.mean_hostility_marriage,
--     o.mean_hostility_boss,
--     o.mean_cultural_impact,

--     -- Social Contact Data
--     s.mean_contact_frequency,
--     s.mean_contact_quality,
--     s.mean_contact_perception,

--     -- Economic Competition Data
--     e.mean_job_competition,
--     e.mean_jobs_taken

-- FROM outgroup_hostility o
-- LEFT JOIN social_contact s ON o.NUTS2 = s.NUTS2
-- LEFT JOIN economic_competition e ON o.NUTS2 = e.NUTS2;







-- SELECT 
--     region AS NUTS2,  -- Standardizing column name
--     AVG(CAST(imbgeco AS FLOAT64)) AS mean_immigration_economy,  -- Convert to FLOAT64
--     AVG(CAST(imwbcrm AS FLOAT64)) AS mean_crime_impact,
--     AVG(CAST(imbleco AS FLOAT64)) AS mean_economic_contribution,
--     AVG(CAST(imdetmr AS FLOAT64)) AS mean_hostility_marriage,
--     AVG(CAST(imdetbs AS FLOAT64)) AS mean_hostility_boss,
--     AVG(CAST(imueclt AS FLOAT64)) AS mean_cultural_impact
-- FROM `thesis-project-451520.thesis_data.ESS`
-- WHERE SAFE_CAST(imbgeco AS FLOAT64) IS NOT NULL  -- Ensures only numeric values
--   AND SAFE_CAST(imwbcrm AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(imbleco AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(imdetmr AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(imdetbs AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(imueclt AS FLOAT64) IS NOT NULL
-- GROUP BY NUTS2;

-- SELECT 
--     region AS NUTS2,  -- Standardized column name
--     AVG(CAST(dfegcon AS FLOAT64)) AS mean_contact_frequency,  -- Frequency of contact (1-7 scale)
--     AVG(CAST(dfegcf AS FLOAT64)) AS mean_contact_quality,  -- Quality of contact (1-3 scale)
--     AVG(CAST(dfeghbg AS FLOAT64)) AS mean_contact_perception  -- Perceived quality (0-10 scale)
-- FROM `thesis-project-451520.thesis_data.ESS`
-- WHERE SAFE_CAST(dfegcon AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(dfegcf AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(dfeghbg AS FLOAT64) IS NOT NULL
-- GROUP BY NUTS2;

-- SELECT 
--     region AS NUTS2,  -- Standardized column name
--     AVG(CAST(eimpcnt AS FLOAT64)) AS mean_job_competition,  -- Perception of job competition (0-10 scale)
--     AVG(CAST(imtcjob AS FLOAT64)) AS mean_jobs_taken  -- Perception of job availability impact (0-10 scale)
-- FROM `thesis-project-451520.thesis_data.ESS`
-- WHERE SAFE_CAST(eimpcnt AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(imtcjob AS FLOAT64) IS NOT NULL
-- GROUP BY NUTS2;

-- SELECT 
--     o.cntry AS Country,
--     o.region AS NUTS2,
--     o.idno,
--     o.inwyye AS Year,

--     -- Outgroup Hostility Data
--     o.mean_immigration_economy,
--     o.mean_crime_impact,
--     o.mean_economic_contribution,
--     o.mean_hostility_marriage,
--     o.mean_hostility_boss,
--     o.mean_cultural_impact,

--     -- Social Contact Data
--     s.mean_contact_frequency,
--     s.mean_contact_quality,
--     s.mean_contact_perception,

--     -- Economic Competition Data
--     e.mean_job_competition,
--     e.mean_jobs_taken

-- FROM `thesis-project-451520.thesis_data.outgroup_hostility` o
-- LEFT JOIN `thesis-project-451520.thesis_data.social_contact` s 
-- ON o.NUTS2 = s.NUTS2
-- LEFT JOIN `thesis-project-451520.thesis_data.economic_competition` e 
-- ON o.NUTS2 = e.NUTS2;


-- SELECT 
--     region AS NUTS2,  -- Standardized column name
--     AVG(CAST(agea AS FLOAT64)) AS mean_age,  -- Average age by region
--     COUNTIF(gndr = '1') / COUNT(*) AS male_ratio,  -- Percentage of males
--     COUNTIF(gndr = '2') / COUNT(*) AS female_ratio,  -- Percentage of females
--     AVG(CAST(eisced AS FLOAT64)) AS mean_education_level,  -- Mean education level (ISCED scale)
--     AVG(CAST(hinctnta AS FLOAT64)) AS mean_income_decile  -- Mean income decile (1-10 scale)
-- FROM `thesis-project-451520.thesis_data.ess_selected`
-- WHERE SAFE_CAST(agea AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(gndr AS STRING) IN ('1', '2')  -- Ensure gender is only '1' (Male) or '2' (Female)
--   AND SAFE_CAST(eisced AS FLOAT64) IS NOT NULL
--   AND SAFE_CAST(hinctnta AS FLOAT64) IS NOT NULL
-- GROUP BY NUTS2;




-- SELECT cntry AS Country, region AS NUTS2, idno, inwyye AS Year
-- FROM `thesis-project-451520.thesis_data.ESS`;






-- SELECT 
--     region AS NUTS2,  -- Standardizing to match your naming convention
--     AVG(imbgeco) AS mean_immigration_economy,  -- Economic impact of immigration (0-10 scale)
--     AVG(imwbcrm) AS mean_crime_impact,  -- Immigration effect on crime (0-10 scale)
--     AVG(imbleco) AS mean_economic_contribution,  -- Economic contribution of immigrants (0-10 scale)
--     AVG(imdetmr) AS mean_hostility_marriage,  -- Hostility: immigrant marrying a relative (0-10 scale)
--     AVG(imdetbs) AS mean_hostility_boss,  -- Hostility: immigrant as a boss (0-10 scale)
--     AVG(imueclt) AS mean_cultural_impact  -- Cultural enrichment or threat (0-10 scale)
-- FROM `thesis-project-451520.thesis_data.ESS`
-- GROUP BY NUTS2;



