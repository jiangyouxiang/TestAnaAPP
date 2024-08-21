generateIRTInfo <- function(Model) {
  info <- NULL
  if(Model == "Rasch model (1PL)" | Model == "Two parameters logistic model (2PL)" |
     Model == "Three parameters logistic model (3PL)" | Model == "Four parameters logistic model (4PL)"){
    info <- withMathJax(
      helpText("The formula of item response theory model for dichotomous response data can be expressed as,"),
      helpText("$$P(X_{ij}=1 | \\theta_{j}) = c_i+(u_i-c_i)\\frac{1}{1+e^{(-\\alpha_i(\\theta_j-\\beta_i))}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ij}\\): The response of \\(j\\) (where \\(j = 1,2,3,...,J\\)) to item \\(i\\) (where \\(i = 1,2,3,...,I\\));"),
      helpText("\\(P(X_{ij}=1 | \\theta_{j})\\): The probability of a correct response;"),
      helpText("\\(\\theta_{j}\\): The latent trait (ability) of respondent \\(j\\);"),
      helpText("\\(\\alpha_i\\): The discrimination parameter of item \\(i\\);"),
      helpText("\\(\\beta_i\\): The difficulty parameter of item \\(i\\);"),
      helpText("\\(c_i\\): The guessing parameter of item \\(i\\);"),
      helpText("\\(u_i\\): The upper asymptote of item \\(i\\), and the slip parameter can calculated by \\(1-u_i\\);"),
      helpText("The formula (1) is a four-parameter logistic model (4PL)."),

      helpText("The 4PL model reduces to a three-parameter logistic model (3PL) when \\(u_i = 1\\):"),
      helpText("$$P(X_{ij}=1 | \\theta_{j}) = c_i+\\frac{1-c_i}{1+e^{-\\alpha_i(\\theta_j-\\beta_i)}}\\tag{2}$$"),

      helpText("It further reduces to a two-parameter logistic model (2PL) when \\(u_i = 1\\) and \\(c_i = 0\\):"),
      helpText("$$P(X_{ij}=1 | \\theta_{j}) = \\frac{1}{1+e^{-\\alpha_i(\\theta_j-\\beta_i)}}\\tag{3}$$"),

      helpText("Finally, the model simplifies to the Rasch model (1PL) when \\(u_i = 1\\), \\(c_i = 0\\), and \\(\\alpha_i = 1\\):"),
      helpText("$$P(X_{ij}=1 | \\theta_{j}) = \\frac{1}{1+e^{-(\\theta_j-\\beta_i)}}\\tag{4}$$")
    )
  }else if(Model == "Partial credit model (PCM)" ){
    info <- withMathJax(
      helpText("The partial credit model (PCM) is a variant of the Rasch model (Masters, 1982) specifically designed to analyze response data with more than two categories."),
      helpText("The item response function (IRF) for the PCM is represented by the following equation:"),
      helpText("$$P(X_{ikj}=k | \\theta_{j}) = \\frac{e^{\\sum_{h=0}^{k}(\\theta_j-\\beta_{ih})}}{\\sum_{c=0}^{m_i}e^{\\sum_{h=0}^{c}(\\theta_j-\\beta_{ih})}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ikj}\\): The response of \\(j\\) to item \\(i\\) in category \\(k\\);"),
      helpText("\\(P(X_{ikj}=k | \\theta_{j})\\): The probability of subject \\(j\\) responds to item \\(i\\) with category \\(k\\);"),
      helpText("\\(\\theta_{j}\\): The latent trait (ability) of respondent \\(j\\);"),
      helpText("\\(\\beta_{ih}\\): The threshold parameter (difficulty) for category \\(k\\) on item \\(j\\);"),
      helpText("\\(m_i\\): The number of categories for item \\(i\\).")
    )

  }else if(Model == "Generalized partial credit model (GPCM)"){
    info <- withMathJax(
      helpText("The generalized partial credit model (GPCM) is an extension of the partial credit model (PCM) that allows for different discrimination parameters for each category of an item."),
      helpText("The item response function (IRF) for the GPCM is represented by the following equation:"),
      helpText("$$P(X_{ikj}=k | \\theta_{j}) = \\frac{e^{\\sum_{h=0}^{k}[\\alpha_i(\\theta_j-\\beta_{ih})]}}{\\sum_{c=0}^{m_i}e^{\\sum_{h=0}^{c}[\\alpha_i(\\theta_j-\\beta_{ih})]}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ikj}\\): The response of \\(j\\) to item \\(i\\) in category \\(k\\);"),
      helpText("\\(P(X_{ikj}=k | \\theta_{j})\\): The probability of subject \\(j\\) responds to item \\(i\\) with category \\(k\\);"),
      helpText("\\(\\theta_{j}\\): The latent trait (ability) of respondent \\(j\\);"),
      helpText("\\(\\alpha_{i}\\): The discrimination parameter of item \\(i\\);"),
      helpText("\\(\\beta_{ih}\\): The threshold parameter (difficulty) for category \\(k\\) on item \\(j\\);"),
      helpText("\\(m_i\\): The number of categories for item \\(i\\).")
    )
  }else if(Model == "Graded response model (GRM)"){
    info <- withMathJax(
      helpText("The graded response model (GRM) estimates the cumulative probability of response categories \\(k\\) or higher."),
      helpText("The cumulative response probability function for the GRM model can be expressed as:"),
      helpText("$$P(X_{ij}{\\geq} k | \\theta_{j}) = \\frac{e^{[\\alpha_i(\\theta_j-\\beta_{ik})]}}{1+e^{[\\alpha_i(\\theta_j-\\beta_{ik})]}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ikj}\\): The response of \\(j\\) to item \\(i\\) in category \\(k\\);"),
      helpText("\\(P(X_{ij}{\\geq} k | \\theta_{j})\\): The probability of respondent \\(j\\) responds to item \\(i\\) with category \\(k\\) or a higher category;"),
      helpText("\\(\\theta_{j}\\): The latent trait (ability) of respondent \\(j\\);"),
      helpText("\\(\\alpha_i\\): The discrimination parameter of item \\(i\\);"),
      helpText("\\(\\beta_{ih}\\): The threshold parameter (difficulty) for category \\(k\\) on item \\(i\\);"),
      helpText("\\(m_i\\): The number of categories for item \\(i\\)."),

      helpText("To calculate the probability of a specific response category, it is necessary to subtract the adjoining cumulative probabilities:"),
      helpText("$$P(X_{ij}= k | \\theta_{j})=P(X_{ij}{\\geq} k | \\theta_{j})-P(X_{ij}{\\geq} k+1 | \\theta_{j})\\tag{2}$$"),
      helpText("It is important to note that the GRM model, in contrast to the GPCM and PCM models, assumes the inequality \\(\\beta_{ik}{\\geq}{\\beta_{i(k-1)}}\\). This inequality indicates that the difficulty of higher response categories is consistently greater than that of lower categories.")
    )
  }
  return(info)
}

generateMIRTInfo <- function(Model){
  info <- NULL
  if(Model == "Rasch model (1PL)" | Model == "Two parameters logistic model (2PL)" |
     Model == "Three parameters logistic model (3PL)" | Model == "Four parameters logistic model (4PL)"){
    info <- withMathJax(
      helpText("The formula of multidimensional item response theory (MIRT) model for dichotomous response data can be expressed as,"),
      helpText("$$P(X_{ij}=1 | \\theta_{jq}) = c_i+(u_i-c_i)\\frac{1}{1+e^{(-(\\alpha_{iq}\\theta_{jq}+d_i))}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ij}\\): The response of \\(j\\) (where \\(j = 1,2,3,...,J\\)) to item \\(i\\) (where \\(i = 1,2,3,...,I\\));"),
      helpText("\\(q (q = c(1,2,3,...,Q))\\): The dimension of the latent trait (ability);"),
      helpText("\\(P(X_{ij}=1 | \\theta_{jq})\\): The probability of a correct response;"),
      helpText("\\(\\theta_{jq}\\): The latent trait (ability) of respondent \\(j\\) on dimension \\(q\\);"),
      helpText("\\(\\alpha_{iq}\\): The discrimination parameter of item \\(i\\) on dimension \\(q\\);"),
      helpText("\\(d_i\\): The threshold parameter of item \\(i\\), which is associated with the item difficult parameter \\(\\beta_i\\) and item discrimination \\(\\alpha_{iq}\\);"),
      helpText("\\(c_i\\): The guessing parameter of item \\(i\\);"),
      helpText("\\(u_i\\): The upper asymptote of item \\(i\\), and slip parameter denoted as \\(1-u_i\\)."),
      helpText("The formula (1) is a multidimensional four-parameter logistic model (4PL)."),
      helpText("The model reduces to a multidimensional three-parameter logistic model (3PL) when \\(u_i = 1\\):"),
      helpText("$$P(X_{ij}=1 | \\theta_{jq}) = c_i+(1-c_i)\\frac{1}{1 + e^{(-(\\alpha_{iq}\\theta_{jq} + d_i))}}\\tag{2}$$"),
      helpText("It further reduces to a multidimensional two-parameter logistic model (2PL) when \\(u_i = 1\\) and \\(c_i = 0\\):"),
      helpText("$$P(X_{ij} = 1 | \\theta_{jq}) = \\frac{1}{1 + e^{(-(\\alpha_{iq}\\theta_{jq} + d_i))}}\\tag{3}$$"),
      helpText("Finally, the model simplifies to the multidimensional Rasch model (1PL) when \\(u_i = 1\\), \\(c_i = 0\\), and \\(\\alpha_{iq} = 1\\):"),
      helpText("$$P(X_{ij} = 1 | \\theta_{jq}) = \\frac{1}{1+e^{(-(\\theta_{jq} - \\beta_i))}}\\tag{4}$$"),
      helpText("Where \\(\\beta_i=-d_i\\) is the item difficulty parameter."),
      helpText("          "),
      helpText("It is important to note that the parameter \\(d_i\\) cannot represent item difficulty, and it functions solely as a threshold parameter. In order to achieve a similar interpretation of item parameters as the unidimensional IRT model, researchers (Reckase, 2009; Zhang & Stone, 2008) have proposed the multidimensional discrimination index \\(MDISC_i\\) (Multidimensional Discrimination) to evaluate discrimination in MIRT. The item difficulty parameter \\(\\beta_i\\) can be calculated using \\(MDISC_i\\) and \\(d_i\\)."),
      helpText("$$MDISC_i = \\sqrt{\\sum_{q=1}^Q(\\alpha_{iq})^2}\\tag{5}$$"),
      helpText("$$\\beta_i = \\frac{-d_i}{MDISC_{i}}\\tag{6}$$"),
      helpText("TestAnaAPP calculates the item difficulty parameter \\(\\beta_i\\) using the formula (6) and the discrimination index \\(MDISC_i\\).")
    )
  }else if(Model == "Partial credit model (PCM)"){
    info <- withMathJax(
      helpText("The item parameters of the multidimensional partial credit model (MPCM) consist solely of difficulty parameters, denoted as \\(\\beta_{ih}\\). Here, \\(\\beta_{ih}\\) represents the difficulty for category \\(k\\) on item \\(i\\)."),
      helpText("The item response function (IRF) of the MPCM model is as follows:"),
      helpText("$$P(X_{ikj}=k | \\theta_{jq}) = \\frac{e^{\\sum_{h=0}^{k}(\\theta_{jq}-\\beta_{ih})}}{\\sum_{c=0}^{m_i}e^{\\sum_{h=0}^{c}(\\theta_{jq}-\\beta_{ih})}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ikj}\\): The response of \\(j\\) to item \\(i\\) in category \\(k\\);"),
      helpText("\\(q (q = c(1,2,3,...,Q))\\): The dimension of the latent trait (ability);"),
      helpText("\\(P(X_{ikj}=k | \\theta_{jq})\\): The probability of subject \\(j\\) responds to item \\(i\\) with category \\(k\\);"),
      helpText("\\(\\theta_{jq}\\): The latent trait (ability) of respondent \\(j\\) on dimension \\(q\\);"),
      helpText("\\(\\beta_{ih}\\): The threshold parameter (difficulty) for category \\(k\\) on item \\(j\\);"),
      helpText("\\(m_i\\): The number of categories for item \\(i\\).")
    )
  }else if(Model == "Generalized partial credit model (GPCM)"){
    info <- withMathJax(
      helpText("In the case of the multidimensional generalized partial credit model (MGPCM), the item response function (IRF) can be described as follows:"),
      helpText("$$P(X_{ikj}=k | \\theta_{jq}) = \\frac{e^{\\sum_{h=0}^{k}[(\\alpha_{iq}\\theta_{jq}+d_{ih})]}}{\\sum_{c=0}^{m_i}e^{\\sum_{h=0}^{c}[(\\alpha_{iq}\\theta_{jq}+d_{ih})]}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ikj}\\): The response of \\(j\\) to item \\(i\\) in category \\(k\\);"),
      helpText("\\(q (q = c(1,2,3,...,Q))\\): The dimension of the latent trait (ability);"),
      helpText("\\(P(X_{ikj}=k | \\theta_{jq})\\): The probability of subject \\(j\\) responds to item \\(i\\) with category \\(k\\);"),
      helpText("\\(\\theta_{jq}\\): The latent trait (ability) of respondent \\(j\\) on dimension \\(q\\);"),
      helpText("\\(\\alpha_{iq}\\): The discrimination parameter for category \\(k\\) on item \\(i\\);"),
      helpText("\\(d_{ih}\\): The threshold parameter for category \\(k\\) on item \\(i\\);"),
      helpText("\\(m_i\\): The number of categories for item \\(i\\)."),
      helpText("                 "),
      helpText("It is important to note that the parameter \\(d_i\\) cannot represent item difficulty, and it functions solely as a threshold parameter. In order to achieve a similar interpretation of item parameters as the unidimensional IRT model, researchers (Reckase, 2009; Zhang & Stone, 2008) have proposed the multidimensional discrimination index \\(MDISC_i\\) (Multidimensional Discrimination) to evaluate discrimination in MIRT. The item difficulty parameter \\(\\beta_i\\) can be calculated using \\(MDISC_i\\) and \\(d_i\\)."),
      helpText("$$MDISC_i = \\sqrt{\\sum_{q=1}^Q(\\alpha_{iq})^2}\\tag{2}$$"),
      helpText("$$\\beta_i = \\frac{-d_i}{MDISC_{i}}\\tag{3}$$"),
      helpText("TestAnaAPP calculates the item difficulty parameter \\(\\beta_i\\) using the formula (3) and the discrimination index \\(MDISC_i\\).")
    )

  }else if(Model == "Graded response model (GRM)"){
    info <- withMathJax(
      helpText("The multidimensional graded response model (MGRM) employs the following function to represent the cumulative response probability:"),
      helpText("$$P(X_{ij}{\\geq} k | \\theta_{jq}) = \\frac{e^{[(\\alpha_{iq}\\theta_{jq}+d_{ik})]}}{1+e^{[(\\alpha_{iq}\\theta_{jq}+d_{ik})]}}\\tag{1}$$"),
      helpText("where:"),
      helpText("\\(X_{ikj}\\): The response of \\(j\\) to item \\(i\\) in category \\(k\\);"),
      helpText("\\(q (q = c(1,2,3,...,Q))\\): The dimension of the latent trait (ability);"),
      helpText("\\(P(X_{ij}{\\geq} k | \\theta_{jq})\\): The probability of respondent \\(j\\) responds to item \\(i\\) with category \\(k\\) or a higher category;"),
      helpText("\\(\\theta_{jq}\\): The latent trait (ability) of respondent \\(j\\) on dimension \\(q\\);"),
      helpText("\\(\\alpha_{iq}\\): The discrimination parameter for category \\(k\\) on item \\(i\\);"),
      helpText("\\(d_{ik}\\): The threshold parameter for category \\(k\\) on item \\(i\\);"),
      helpText("\\(m_i\\): The number of categories for item \\(i\\)."),
      helpText("                 "),
      helpText("To calculate the probability of a specific response category, it is necessary to subtract the adjoining cumulative probabilities:"),
      helpText("$$P(X_{ij}= k | \\theta_{jq})=P(X_{ij}{\\geq} k | \\theta_{jq})-P(X_{ij}{\\geq} k+1 | \\theta_{jq})\\tag{2}$$"),
      helpText("In contrast to the MGPCM and MPCM models, MGRM model assumes the inequality \\(d_{ik}{\\geq}{d_{i(k-1)}}\\). This inequality indicates that the difficulty of higher response categories is consistently greater than that of lower categories."),
      helpText("                 "),
      helpText("It is important to note that the parameter \\(d_i\\) cannot represent item difficulty, and it functions solely as a threshold parameter. In order to achieve a similar interpretation of item parameters as the unidimensional IRT model, researchers (Reckase, 2009; Zhang & Stone, 2008) have proposed the multidimensional discrimination index \\(MDISC_i\\) (Multidimensional Discrimination) to evaluate discrimination in MIRT. The item difficulty parameter \\(\\beta_i\\) can be calculated using \\(MDISC_i\\) and \\(d_i\\)."),
      helpText("$$MDISC_i = \\sqrt{\\sum_{q=1}^Q(\\alpha_{iq})^2}\\tag{3}$$"),
      helpText("$$\\beta_i = \\frac{-d_i}{MDISC_{i}}\\tag{4}$$"),
      helpText("TestAnaAPP calculates the item difficulty parameter \\(\\beta_i\\) using the formula (4) and the discrimination index \\(MDISC_i\\).")
    )
  }
  return(info)
}
generateCRMInfo <- function(){
  info <- NULL
  info <- withMathJax(
    helpText("As shown in Zopluoglu C (2022), the probability that the observed score \\(X_{ij}\\) is greater than or equal to some value \\(x\\) is given by:"),
    helpText("$$P(X_{ij} \\geq x | \\theta_i, a_j, b_j, \\alpha_j) = \\frac{1}{\\sqrt{2\\pi}} \\int_{-\\infty}^{v} e^{-\\frac{t^2}{2}} dt$$"),
    helpText("where:"),
    helpText("$$v = a_j(\\theta_i - b_j - \\frac{1}{\\alpha_j} \\ln \\frac{x_{ij}}{k_j - x_{ij}})$$"),
    helpText("Here, \\(a\\) is the discrimination parameter, \\(b\\) is the difficulty parameter, and \\(\\alpha\\) is a scaling parameter. The scaling parameter \\(\\alpha\\) defines the transformation from the observed score scale to the \\(\\theta\\) scale. \\(k_j\\) is the maximum possible score for the item \\(j\\)."),

    helpText("Next, the observed scores \\(X\\) are transformed to a random variable \\(Z\\) using the following equation:"),
    helpText("$$Z_{ij} = \\ln\\left(\\frac{X_{ij}}{k_j - X_{ij}}\\right)$$"),

    helpText("The conditional probability density function of the random variable \\(Z\\) is then given by:"),
    helpText("$$f(z_{ij}|\\theta_i, a_j, b_j, \\alpha_j) = \\frac{a_j}{\\sqrt{2\\pi \\alpha_j}} \\exp \\left(-\\frac{[a_j(\\theta_i - b_j - \\frac{z_{ij}}{\\alpha_j}]^2)}{2}\\right)$$"),
    helpText("This is a normal density function with a mean of \\(\\alpha(\\theta - b)\\) and a variance of \\(\\alpha^2/a^2\\).")
  )
  return(info)
}
