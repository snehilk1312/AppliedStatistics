Content: ANOVA
================

## **Packages amd functions Used:**

  - pandas
  - matplotlib.pyplot
  - seaborn
  - numpy
  - scipy.stats
  - pingouin
  - statsmodels.api
  - statsmodels.formula.api
  - statsmodels.stats.outliers_influence
  - from statsmodels.tools.tools import add_constant
  - from patsy.contrasts import ContrastMatrix
  - from patsy.contrasts import Poly
  - scikit_posthocs
  - from statsmodels.sandbox.stats.multicomp import MultiComparison
  

## **Content Covered:**

1.  Levene Test, using scipy.stats.levene
2.  ols(), using from statsmodels.formula.api import ols
3.  Anova
    1.  using statsmodels.api.stats.anova_lm()
4.  Welch Anova, using pingouin.welch_anova()
5.  Robust Anova, using scipy.stats.kruskal()
6.  Contrasts
7.  Planned Comparison, using various Contrasts
8.  Trend analysis , using contrasts
9.  Post-hoc tests, using
    1.  allpairtest
    2.  scikit_posthocs
10. Robust Posthoc test, using scikit_posthocs.posthoc_wilcoxon()
11. Effect Size ,
    1.  R Squared (eta-squared)
    2.  omega squared
    3.  using mes(), for differnt pair of groups
    4.  effect size of contrasts
