# Evaluation Metrics for Classification

# Sample data set
X = sample(x = c("TP","FP","TN","FN"), size = 1000, replace = T, prob = c(0.5,0.25,0.15,0.1))
table(X)

tp = length(which(X == "TP"))
fp = length(which(X == "FP"))
tn = length(which(X == "TN"))
fn = length(which(X == "FN"))


precision = tp / (tp + fp); print(precision)
recall = tp / (tp + fn); print(recall)
tpr = tp / (tp + fn); print(tpr) # Sensitivity, Recall, Hit Rate, True Positive Rate
tnr = tn / (tn + fp); print(tnr) # Specificity, Selectivity, True Negative Rate
ppv = tp / (tp + fp); print(ppv) # Precision, Positive Predictive Value
npv = tn / (tn + fn); print(npv) # Negative Predictive Value
fnr = fn / (fn + tp); print(fnr) # False Negative Rate, Miss Rate
fpr = fp / (fp + tn); print(fpr) # False Positive Rate, Fall-out
fdr = fp / (fp + tp); print(fdr) # False Discovery Rate
fr = fn / (fn + tn); print(fr) # = False Omission Rate
ts = tp / (tp + fn + fp); print(ts) # Threat Score, Critical Success Index
acc = (tp + tn) / (tp + tn + fp + fn); print(acc) # Accuracy
f1 = (2*tp) / (2*tp + fn + fp); print(f1) # F1 Score
