#Data preparation:
* During the following lab, you are supposed to know some of the most important steps about data preparation. There are three labs, the first one is about classification evaluation, the second one is about processing missing values and the last one is about processing outliers.

* All labs will be elaborated in Python, except the last study about MCA, which will be in R.
* I have included all necessary datasets used in my implementations.

## What API used to run codes ? 
* Jupyter notebook ( python 3.0 and more ): for lab 1, lab 2 and the first part of the last lab. 
* R notebook : for the  last part lab 3.
## Needed libraries :
Some times you will need to install some libraries (python) or packages (R) : 
* For python : !pip install library_name
* For R      : install.packages("package_name")


##Lab 1: Classifier evaluation
Preparing, preprocessing, understanding data, feature engineering and modeling are considered as a very important steps in machine learning. However, a Data scientist should be very aware about the results of his model and the way how to interpret its results. This lab covers evaluation and model selection methods that you can use to help understand and optimize the performance of your machine learning models.

###Objectives :
	1. Understand why accuracy alone can be an inadequate metric for getting a more complete picture of a classifier's performance

	2. Understand the motivation and definition of a variety of important evaluation metrics in machine learning and how to interpret the results of using a given evaluation metric

	3. Optimize a machine learning algorithm using a specific evaluation metric appropriate for a given task

##Lab 2 : Missing data preprocessing
Missing data is a very popular and challenging problem in data mining and machine learning. Data can be missed for many reasons which I will not cover here in the introduction, but to give a tangible example : "a person that we have questioned did not want to tell about his salary". We will explain all missing data mechanisms later on. As a consequence handling missing data is an important step in data mining since many algorithms can not support data with missing values. As a fruit we have a good accuracy and so a good decision making.
This lab will be composed of two major parts : a part that talks about the fundamentals of missing values and a practical part where we will study an example in detail.

##Lab3 : Processing outliers
Outliers data is a very popular and challenging problem in data mining and machine learning. Data can be an outlier for many reasons which I will not cover here in the introduction, but to give a tangible example : "All individuals have a salary between \$15 000 and \$25 000 except one person who has $100 000. This person will be considered as an outlier". We will explain all outlier data mechanisms later on. Hence, Handling outlier data is an important step in data mining since the majority of algorithms can not handle accurately outlier values. As a fruit we have a good accuracy and so a good decision making.
During this lab, we will cover two principal parts: In the first part we will go through some general definitions supported by examples, then a second part which will be a case study with the language R ( MCA : Multiple Correspondence Analysis).
