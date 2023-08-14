library(tidyverse)
library(gtsummary)

nlsy_cols <- c("glasses", "eyesight", "sleep_wkdy", "sleep_wknd",
							 "id", "nsibs", "samp", "race_eth", "sex", "region",
							 "income", "res_1980", "res_2002", "age_bir")
nlsy <- read_csv(here::here("data", "raw", "nlsy.csv"),
								 na = c("-1", "-2", "-3", "-4", "-5", "-998"),
								 skip = 1, col_names = nlsy_cols) |>
	mutate(region_cat = factor(region, labels = c("Northeast", "North Central", "South", "West")),
				 sex_cat = factor(sex, labels = c("Male", "Female")),
				 race_eth_cat = factor(race_eth, labels = c("Hispanic", "Black", "Non-Black, Non-Hispanic")),
				 eyesight_cat = factor(eyesight, labels = c("Excellent", "Very good", "Good", "Fair", "Poor")),
				 glasses_cat = factor(glasses, labels = c("No", "Yes")))


# Customization of `tbl_summary()`

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir))

#label makes it look nicer - make the variable names show up in the table
tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat, region_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		region_cat ~ "Region",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing")

#adds lots of different functions
#original table summary first

tbl_summary(
	nlsy,
	by = sex_cat,
	include = c(sex_cat, race_eth_cat,
							eyesight_cat, glasses, age_bir),
	label = list(
		race_eth_cat ~ "Race/ethnicity",
		eyesight_cat ~ "Eyesight",
		glasses ~ "Wears glasses",
		age_bir ~ "Age at first birth"
	),
	missing_text = "Missing") |> #PIKE = takes what you made and passes it to another function "add on"
	add_p(test = list(all_continuous() ~ "t.test", #add p value and tells R how you want them. Change the test you want in quotes
										all_categorical() ~ "chisq.test")) |>
	add_overall(col_label = "**Total**") |>
	bold_labels() |>
	modify_footnote(update = everything() ~ NA) |> #always do this - get rid of footnotes or add
	modify_header(label = "**Variable**", p.value = "**P**") #modify the header and bold them

#in class activity part
tbl_summary(nlsy,
						by = sex_cat,
						include = c(sex_cat, race_eth_cat,
											 region_cat, sleep_wkdy, sleep_wknd, income), #or starts_with("sleep") and then dont need to add both
						label = list(
							race_eth_cat ~ "Race/ethnicity",
							region_cat ~ "Region",
							income ~ "Income",
							sleep_wkdy ~ "Weekend Sleep Hours",
							sleep_wknd ~ "Weekday Sleep Hours"
						),
						statistic = list(income ~ "10% = {p10}; 90% = {p90}",
														 sleep_wkdy ~ "min =  {min}; max = {max}",
														 sleep_wknd ~ "min =  {min}; max = {max}"),
						digits = list(income ~ c(3, 3),
													sleep_wkdy~ c(1, 1),
													sleep_wknd~ c(1, 1)),
						missing_text = "Missing")|>
	add_p(test = list(all_continuous() ~ "t.test", #add p value and tells R how you want them. Change the test you want in quotes
										all_categorical() ~ "chisq.test")) |>

	add_overall(col_label = "**Total**") |>
	bold_labels() |>

	modify_table_styling(
		columns = label, rows = label == "Race/ethnicity",
		footnote = " see https://www.nlsinfo.org/content/cohorts/nlsy79/topical-guide/household/race-ethnicity-immigration-data"
	)
	modify_header(label = "**Variable**", p.value = "**P**") #modify the header and bold them
