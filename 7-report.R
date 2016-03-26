# report.R

source("0-library.R")

if (!exists("analyze.demographics")) analyze.demographics <- readRDS(paste(analysis.dir, "demographics.Rds", sep = "/"))
if (!exists("analyze.diagnosis")) analyze.diagnosis <- readRDS(paste(analysis.dir, "diagnosis.Rds", sep = "/"))
if (!exists("analyze.home.meds")) analyze.home.meds <- readRDS(paste(analysis.dir, "home_meds.Rds", sep = "/"))
if (!exists("analyze.bleed")) analyze.bleed <- readRDS(paste(analysis.dir, "bleed.Rds", sep = "/"))
if (!exists("analyze.thrombosis")) analyze.thrombosis <- readRDS(paste(analysis.dir, "thrombosis.Rds", sep = "/"))

# create docx object with project title and authors
project <- "Bleeding Events with Enoxaparin in Patients with Moderate Renal Dysfunction"
authors <- "Stephanie Kuhl, Brian Gulbis, Andrea C. Hall"

mydoc <- result_docx(project, authors)

# add results tables
mydoc <- result_table(mydoc, analyze.demographics, "Demographics")
mydoc <- result_table(mydoc, analyze.diagnosis, "Past Medical History")
mydoc <- result_table(mydoc, analyze.home.meds, "Home Medications")
cram <- c("drop.after.procedure", "prbc.after.procedure", "prbc.after.drop")
mydoc <- result_table(mydoc, analyze.bleed, "Bleeding Events", cram = cram)
cram <- c("stroke.new", "pe.new")
mydoc <- result_table(mydoc, analyze.thrombosis, "New Thrombosis", cram = cram)

# add result table for each continuous agent
# mydoc <- result_table2(mydoc, analyze.sedatives, "med", "Continuous Medications")

# add citation and write docx to Word
write_docx(mydoc, file = paste(analysis.dir, "alcalde_results.docx", sep = "/"))
