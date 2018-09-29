---
geometry: "left=2.5cm,right=4cm,top=2.5cm,bottom=2cm"
output: 
  bookdown::pdf_book:
    toc: false
    includes:
     in_header: header.tex
     before_body: title_page.tex
     after_body: appendix.tex
documentclass: article
bibliography: references.bib
csl: university-of-stuttgart-sowi-standards.csl
link-citations: yes
---




<!-- \onehalfspacing -->
\setstretch{1.5}

\section{Introduction}







The following Chapter \ref{theory} discusses our theoretical framework and classification of European populist parties, as well as some of the general challenges when it comes to quantifying and analyzing populism in Europe. In Chapter \ref{methods}, the used research methods are introduced and the classification of European parties is done with the help of k-means clustering. Afterwards, Chapter \ref{analysis} reports on the multinomial logistic regression and its results. Finally, the conclusion (Chapter \ref{conclusion}) gives a summary of the results and derives implications for further research.

\newpage
\section{Theory}  \label{theory}




\newpage
\section{Methods and Data} \label{methods}



## Online Experiment

The online survey-experiment was conducted over a period of one week. The studied subjects were chosen beforehand through dispension of digital invitations by members of the project group. This way three hundred subjects were selected. As a result of the selection method the sample reflects a very high percentage of students as well as a very low age and a political centre-left attitude on average (see below). 

The pooled subjects were randomized into three groups each receiving a different treatment in between the first and the second query of a decision in one of two ethcial dilemmas. The two different dilemmas were the described ‚trolley-problem‘ and the ‚fat man-problem‘ (see attachement for descriptions). These two dilemmas were distributed randomly among the three treatment-groups. 

It should be noted though that the randomized assignment of subjects to one of the three treatment-/control-groups was flawed. This can be derived from a comparison of the mean-values of this studies main-variables: The compared mean-values by groups for the dependent variable in dichotomous (?) and continous forms, as well as the variables for gender, age and the primary indepent variables Idealism and Relativism show some significant differences (see below). The following analysis should therefore be interpreted with caution and keep in mind possible effects of non-random group-assignment. 

Chosen subjects got to read a insertion text and had to fill in a survey regarding their general ethical position [@forsyth1980taxonomy] as well as their personality and political attitude. The they were asked to make a first decision in one of the two ethical dilemmas via dichotomous Yes/No choice and continous tendency-rating (towards intervening or staying passive). Afterwards each of the three groups received a treatment:


+ The first group, which comes closest to a ‚control group‘, had to read a short text conerning ethics (see attachement). 
+ In the second group each subject received identical utilitarian and deontological arguments. Three arguments each argued for or against an intervention in the ethical dilemmas (see attachement). 
+ The third group participated in a one-week-long asynchronous dialog on a digital platform. "Smartopinion" allowed the participants to post and comment their own arguments for or against any decision in the ethical dilemma or comment on one of 6 pro/contra-arguments (3 each utilitarian/deontological) which were posted beforehand by the project group. The discussion was moderated by project members who didn’t intervene in the dialog at any given point. 

After these treatments the subjects were once again asked for their decision in their respective ehtical dilemma in the forms of choice and rating and answered a questionnaire concerning their socio-dempographic background.

## Dataset

The resulting dataset consists of 93 variables and data from 300 subjects. The studied sample depicts a high percentage of students (approximately 92 percent) and therefore reflects a low age (24 years) a political centre-left attitude (4.7 on a scale from 1 to 10) and a relatively high political interest (3.2 on a scale from 1 to 4) on average. The exact population cannot be determined, because of arbitrary selection mechanisms within some steps of the research process (dispension of invitations, reaction to invitations etc.). The interpretation of any results from this study should therefore be carried out with caution, even with respect to the population of german students/students from Stuttgart.

## Operationialization and  Factor Analysis

The factors under study were operationalized as follows: 

The dependent variable, the decision in one of two ethical dilemmas, was gathered in the form of a dichotomous choice between Interevention and Passivity and a continous tendency on a scale from 1 (Pro-Passivity) to 11 (Pro-Intervention). The main independent variables, the different ethical ideologies of the subjects under study, were gathered as german versions of the twenty ‚Ethical-Position‘-items originally developed by Forsyth [-@forsyth1980taxonomy]. 

These items ask the respondent for their reaction to ethical statements concerning one of the two dimensions (Idealism and Relativism) identified by Forsyth, on a scale from 1 ‚Completely disagree‘ to 9 ‚Completely agree‘ [@forsyth1980taxonomy: 178].

One example for such a (Idealism-)statement is:

> "A person should make certain that their actions never intentionally harm another even to a small degree" [@forsyth1980taxonomy: 178].

The used translation of the ‚Ethical Position Questionnaire‘ was taken from Strack and Gennerich [-@strack2007erfahrung: 13] (see attachement). To validate and extract the two dimensions, ‚Idealism‘ and ‚Relativism‘, from the data, a Explorative Factor Analysis (Principal Component Analysis) was conducted and indices, each consisting of their dimensions 10 variables, were formed (see below). The selected control variables were age (continuous), gender (dichotomous) and frequency of church attendance from 1 ‚More than once a week‘ to 6 ‚Never‘.

In order to validate the two dimensions Idealism and Relativism, from the Ethical Positions Questionnaire, a factor analysis with the rotation method varimax was performed.

\begin{figure}[!h]
	\caption{Principal Component Analysis}
	\includegraphics[width=\textwidth]{images/factor_analysis}
	\flushright
{\scriptsize N = 278. Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

The factor analysis (Figure 1) shows that the variables of the ethical positions questionaire load very well without any cross-loads (cut off value: 0.40). Only the loading of eqp11 is too low (-0.12) to take into account. With the remaining variables the variables Idealism and Relativism were created.


## Randomization and Descriptive Statistics

This section will introduce some basic descriptive statistics of the used variables. Table 1 shows summary statistics for the used variables.



\begin{table}[ht]
\caption{Summary Statistics}
\centering
\begin{tabular}{rrrrrrrrr}
  \hline
 & N & Mean & SD & Median & Min & Max & Skew & Kurt \\ 
  \hline
Switch Track T1 & 290 & 5.76 & 2.91 & 6 & 1 & 11 & -0.03 & -0.90 \\ 
  Switch Track T2 & 290 & 5.56 & 2.91 & 6 & 1 & 11 & 0.01 & -0.94 \\ 
  Push Person T1 & 290 & 4.29 & 2.83 & 4 & 1 & 11 & 0.55 & -0.72 \\ 
  Push Person T2 & 290 & 4.18 & 2.86 & 4 & 1 & 11 & 0.60 & -0.64 \\ 
  Idealism & 290 & 0 & 1 & 0.18 & -3.40 & 1.73 & -0.86 & 0.37 \\ 
  Relativism & 290 & 0 & 1 & 0.11 & -3.21 & 2.34 & -0.35 & 0.02 \\ 
  Gender & 290 & 1.46 & 0.50 & 1 & 1 & 2 & 0.17 & -1.98 \\ 
  Age & 289 & 24.09 & 3.86 & 23 & 18 & 53 & 2.14 & 10.52 \\ 
  Church Attendance & 278 & 1.87 & 1.04 & 2 & 1 & 6 & 1.38 & 2.06 \\ 
  Control Group & 290 & 0.32 & 0.47 & 0 & 0 & 1 & 0.78 & -1.39 \\ 
  Discussion Group & 290 & 0.34 & 0.47 & 0 & 0 & 1 & 0.67 & -1.56 \\ 
  Information Group & 290 & 0.34 & 0.47 & 0 & 0 & 1 & 0.67 & -1.56 \\ 
   \hline
\end{tabular}
\end{table}

Figure 2 shows a comparison between the experimental groups by age, gender, Idealism and Relativism. As mentioned before, the randomization of the groups was flawed. Regarding the comparison between the experimental groups by age and gender, the differences are significant. The control group has a women proportion of over 52%, whereas only 41% of the discussion group are women. In addition, there is a strong outlier in the control group with over 50 years in the age. 


\begin{figure}
	\caption{Randomization - Sociodemographics}
	\includegraphics[width=\textwidth]{images/dem_compare}
	\flushright
{\scriptsize N = 278. P-Values are reported. Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

Moreover, there are also significant differences between the experimental groups regarding the main independent variables Idealism and Relativism. Concerning this issue, the following analysis should be interpreted with caution.

When it comes to the question of whether it is morally justifiable to switch the track or even push a person, there are differences in age and gender (see Figure 3).


\begin{figure}[!h]
	\caption{Randomization - Dependent Variables}
	\includegraphics[width=\textwidth]{images/gender_av_compare}
	\flushright
{\scriptsize N = 278. Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

Older people as well as female persons consider both scenarios less morally justified. A comparison of the means, regarding the gender, the differences between men and women are clearly significant in both scenarios. However, comparing the two scenarios shows that switching tracks is more likely to be morally justified than pushing a person. 


\begin{figure}[!h]
	\caption{Randomization - Independent Variables}
	\includegraphics[width=\textwidth]{images/uv_compare}
	\flushright
{\scriptsize N = 278. P-Values are reported. Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

\newpage
\section{Analysis} \label{analysis}



In this Section, the regression models are reported and interpreted in regard to their implications for the previously derived hypotheses. In sum, XX models were estimated, which are depicted in coefficient plots (see plot XX to XX). 

\begin{figure}[!h]
	\caption{Models 1}
	\includegraphics[width=\textwidth]{images/reg1_combined}
	\flushright
{\scriptsize $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$.  Unstandardized regression coefficients. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

First, the models with the dependent variable at time t1 (before the treatment) are analyzed. Model 1a shows the results for scenario 1, model 1b for scenario 2.  The factor idealism has a strong negative effect, meaning that the more idealistic a person is, the less they tend to switch and push (Model 1a: b = xx , p< 0.001 and Model 1b: b = xx , p< 0.001) . Relativism in turn has a positive sign, but is statistically insignificant and has only a marginal coefficient size (Model 1a: b =  and Model 1b: b =  ). It has to be noted that the R^2 statistics are not particularly large, indicating only 9, 6% and 14,5% explained variance of the dependent variable for the Models 1a and 1b, respectively. Nevertheless, for the previously derived hypotheses H1a, which states that, the more idealist an individual, the less likely it will switch/push, confirming evidence can be noted. The same holds true for H1b, which assumes that relativism has no effect on an individual’s decision to switch/push. 

\begin{figure}[!h]
	\caption{Models 2 - Idealism}
	\includegraphics[width=\textwidth]{images/reg2_c1_idealism}
	\flushright
{\scriptsize $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$.  N = 278. Unstandardized regression coefficients. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

As a next step, models with the dependent variable after the treatment are estimated (t2). Here, we control for the variable  at the time t1, which means that the other independent variables only predict the variance of the dependent variable which differs from the one at t1.  Positive coefficients indicate opinion change in the direction of higher values on the dependent variable, negative coefficients in turn indicate negative opinion change. In general, the R^2 values in all of the t2-models therefore indicate a high amount of explained variance, due to the fact that the judgment at the time t2 is highly dependent on the judgment at t1, wich is included in the models. 

\begin{figure}[!h]
	\caption{Models 2 - Idealism Interaction Plots}
	\includegraphics[width=\textwidth]{images/reg2_c2_idealism}
	\flushright
{\scriptsize N = 278. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

Model 2a depicts the results for the scenario 1 dependent variable, Model 2b for scenario 2 (see plot XX and XX). First, it comes to attention that the treatments have a strong effect. The discussion group has negative coefficients for both scenarios, indicating that persons which engaged in discussion had, compared to persons of the control group, a opinion change in the direction of “not switching/pushing” (Model 2a: b = -0.73, p< 0.01 and Model 2b: b = -0.81,p< 0.01). Regarding the information group, these effects can be observed as well, although not as strong (Model 2a: b = -0.46, p< 0.05 and Model 2b: b = -0.56, p< 0.05). The opinion change in the discussion group is more negative, but the differences to the information group are not as large and not statistically significant. Regarding idealism, for scenario 1 a rather weak and insignificant effect can be observed (b = -0.15), while relativism has no observable effect on opinion change at all (b = 0.00). The results for scenario 2 are insignificant as well, but differ in regard to relativism, which now has a weak negative effect (b = -0.14), while the effect of idealism did not change much (b = -0.09). 

\begin{figure}[!h]
	\caption{Models 3 - Relativism}
	\includegraphics[width=\textwidth]{images/reg3_c1_relativism}
	\flushright
{\scriptsize $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$. N = 278. Unstandardized regression coefficients. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

In Models 3a and 3b, an interaction effect with the treatment and idealism is estimated to test hypothesis H2a and H2b ( Compared to the control group, information and discussion treatment, respectively,  strengthen the effect of idealism on decision to not switch/push) . Plot xx and XX show the marginal effects for the interactions, respectively. The reported results do not point in the direction assumed in the hypotheses. XXX Hier wären die Interaction plots toll.XXXX

\begin{figure}[!h]
	\caption{Models 3 - Relativism Interaction Plots}
	\includegraphics[width=\textwidth]{images/reg3_c2_relativism}
	\flushright
{\scriptsize N = 278. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

Hier am Samstag weiter arbeiten

In Models 4a and 4b, an interaction effect with the treatment and relativism is estimated. Plot xx and XX show the marginal effects for the interactions, respectively. It appears that for relativism, there is… XXX Auch hier wären die Interaction plots toll.XXXX

\begin{figure}[!h]
	\caption{Models 4 - Opinion Change}
	\includegraphics[width=\textwidth]{images/reg4_combined}
	\flushright
{\scriptsize $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$. N = 278. Unstandardized regression coefficients. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

Hier am Samstag weiter arbeiten
H3a: In general, relativism has no effect on the direction of opinion change. -> nicht ganz so bestätigt, bisschen durcheinander die effekte, aber alle nicht signifikant
H3b: The more relativist an individual is, the more likely it will switch/push after receiving discussion treatment. -> effektrichtung stimmt, aber nicht signifikant und nicht sehr starker effekt

\begin{figure}[!h]
	\caption{Models 5 - Idealism}
	\includegraphics[width=\textwidth]{images/reg5_c1_idealism}
	\flushright
{\scriptsize $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$.  Unstandardized regression coefficients. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}


Regarding the control variables for the dependent t1 variable, none of them reaches statistical significance. In both models, a negative effect is observed for gender. Women score lower on the dependent variables, meaning that they are less inclined to switch/push. For both scenarios, church attendance has a really small positive effect, while age has no observable effect at all. Regarding the the control variables in the t2 models, gender has a negative and weakly significant effect, indicating that opinion change was more negative for woman than man. The other control variables have only weak and insignificant effects. 

\begin{figure}[!h]
	\caption{Models 5 - Idealism Interaction Plots}
	\includegraphics[width=\textwidth]{images/reg5_c2_idealism}
	\flushright
{\scriptsize N = 278. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

\begin{figure}[!h]
	\caption{Models 6 - Relativism}
	\includegraphics[width=\textwidth]{images/reg6_c1_relativism}
	\flushright
{\scriptsize $^{***}p<0.001$, $^{**}p<0.01$, $^*p<0.05$. N = 278. Unstandardized regression coefficients. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

\begin{figure}[!h]
	\caption{Models 6 - Relativism Interaction Plots}
	\includegraphics[width=\textwidth]{images/reg6_c2_relativism}
	\flushright
{\scriptsize N = 278. 90\% confidence intervals are shown. \\ Own calculations based on data from Online-Survey Experiment.\par}
\end{figure}

\newpage
\section{Conclusions} \label{conclusion}



Hypothesen die bestätigt wurden kurz erwähnen

Überlegen, warum Hypothesen ggf. nicht bestätigt wurden

Hier die Theorie zu Geschlecht einfügen? → yoh da haben ma doch wat. Kann man zsm diskutieren


\setstretch{1}

\clearpage
\newpage


# References

\setlength{\parindent}{-0.2in}
\setlength{\leftskip}{0.2in}
\setlength{\parskip}{8pt}
\noindent
