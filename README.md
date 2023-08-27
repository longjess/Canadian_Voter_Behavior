# Canadian_Voter_Behavior

Using data from the Canada Election Study (CES) survey for the 2004 and 2021 Canadian federal elections,
I analyzed the influence of class (through union membership and education level) and religion on voter
preference between the Liberal, Conservative, and NDP parties. In particular, I investigated if there was a
shift in the party preference of the different groups under consideration between 2004 and 2021. I used a
weighted multinomial logistic regression model with interaction terms to look at the effects of membership
in each group, and the difference in effect between the years. I found that union members prefer the Liberals
to the Conservatives, and the NDP to the Liberals. There is some preference for the Conservatives over the
Liberals among non-university educated voters. Catholics generally prefer the Liberal party, but there was
a notable increase in support for the Conservatives in 2021. Both mainline and non-mainline Protestants
prefer the Conservative party over the Liberals, with the non-mainline Protestants showing stronger support.

This was my final project for Stanford's STATS305B Applied Statistics II course in Winter 2023.

The repository includes:
* R code for cleaning the 2004 and 2021 CES survey data, exploration of if the demographics in the survey were proportional with the population,
and the code for fitting the multinomial logistic regression
* The final report on the project, including an introduction to the issue, the hypotheses being tested,
  the data and methodology, the results of the logistic regression, and conclusions
