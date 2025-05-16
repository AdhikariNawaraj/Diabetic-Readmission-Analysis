🏥 Diabetic Readmission Analysis

📘 Overview:

This project investigates 30-day readmission patterns among diabetic patients using a dataset from 130 U.S. hospitals (1999–2008). By combining data cleaning, visualization, and machine learning, we aim to identify key factors contributing to avoidable readmissions and provide actionable insights for healthcare improvement.

🎯 Goals:

Identify clinical and demographic factors linked to 30-day readmissions.
Develop classification models to predict high-risk patient encounters.
Analyze the effect of medication changes and discharge planning.
Reduce healthcare costs by helping prevent unnecessary hospital readmissions.

📊 Dataset:

Source: UCI ML Repository - Diabetes 130-US Hospitals Dataset
Records: ~101,766 inpatient encounters
Features: 50 total — 12 numerical and 38 categorical
Target: readmitted (classes: <30, >30, NO)



🔧 Data Preprocessing:

Replaced ? with NA and removed columns with >35% missing values.
Encoded categorical features (Label Encoding and One-Hot Encoding).
Performed log and square-root transformation on skewed numeric features.
Reduced category granularity for features like discharge_disposition_id.
Addressed class imbalance for <30 readmissions during model evaluation.

📊 Exploratory Data Analysis:

Visualization with ggplot2, ggcorrplot, and cowplot in R.


Key insights:

Majority of admissions were emergency-based.
Caucasian race and age groups [60–70), [70–80) had high prevalence.
Medication change and dosage adjustments impacted readmission likelihood.



Model	Highlights:
Multinomial Logistic	Baseline model with ~58% accuracy; struggled with <30.
CART	Failed to classify <30 class altogether.
Naive Bayes	Better on NO class, poor on <30.
Random Forest	Good precision on NO, decent for <30, but overfit.
XGBoost	High accuracy, but suspiciously perfect performance – indicating overfitting or data leakage.
Note: Confusion matrices were used extensively to evaluate per-class performance.

📌 Key Takeaways:

Most readmissions occur among patients with no change in medications.
Number of inpatient visits and medications are top predictors.
Class imbalance remains a major challenge for early readmission prediction.
Medication adjustments (e.g., insulin, metformin) correlate with lower readmission rates.

🔮 Future Work:

Apply SMOTE or ADASYN to address class imbalance.
Investigate deep learning models (e.g., LSTM for temporal modeling).
Add external health indicators (e.g., HbA1c scores).
Conduct bias and fairness audits to assess demographic equity.
Deploy an interactive dashboard for healthcare providers.


📚 References:

UCI ML Repository – Diabetes Dataset
Strack, B. et al. (2014). Impact of HbA1c Measurement on Hospital Readmission Rates
Wiley Online Library
