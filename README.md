# PricePredic

Objective: To Identify the potential indicators behind the Medicare prices for the treatment of acute myocardial
infraction and heart failure.

Methods: Data preprocessing was done by merging the IPPS dataset, Inpatient dataset and hospital general
information dataset using the Provider ID. Then, the next step was the ZIP code to GEO-ID translation which was
achieved with a supplementary dataset not used for further analysis. Finally, GEO-ID merge was used to get the
final dataset which was used for further analysis. Then the data was divided into two parts i.e., (Heart failure data
and AMI data) for ease of analysis. After pre-processing the dataset heatmaps, histograms were plotted to observe
trends in the datasets. Finally, data was trained and tested using different models such as Linear Regression,
Classification and Regression Trees, Random Forests and Artificial Neuron Networks to find the best suited
method which would predict the price change effectively and RMSE and R squared values are used as quality
indicators for these models. After examining the results, we found out that our Random forest model showed the
best results with better RMSE and R2 values followed by Linear Regression, CART and Artificial Neural
Network.
