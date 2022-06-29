\name{Agente}
\alias{Agente}
\title{
Probabilidad de Default (un agente)
}
\description{
Nos muestra la probabilidad de que un agente del MEM tenga problemas con el pago de sus obligaciones financieras dentro del mercado.
}
\usage{
Agente()
}




\examples{


## The function is currently defined as
function ()
{
    library(svDialogs)
    library(formattable)
    ASIC = dlgInput(message = "Sigla ASIC agente: ")$res
    Agen = dlgInput(message = "Tipo de Agente:\n                   1 - Comercializador\n                   2 - Generador\n                   3 - Comercializador y generador:")$res
    OpeT = dlgInput(message = "Tiempo de operacion: ")$res
    Ren = dlgInput(message = "Rentabilidad (ROA en \%): ")$res
    Deu = dlgInput(message = "Endeudamiento (CP en \%): ")$res
    Expcom = dlgInput(message = "Exposicion a bolsa en compra: ")$res
    Date_ = Sys.Date()
    fAgente = as.factor(as.numeric(Agen))
    T_Ope = as.numeric(OpeT)
    Rent = as.numeric(Ren)
    End = as.numeric(Deu)
    Exp_Compra = as.numeric(Expcom)
    Dats = data.frame(ASIC, fAgente, T_Ope, Rent, End, Exp_Compra)
    Models = RegLog()
    log_odds = predict(Models, Dats)
    Probability = exp(log_odds)/(1 + exp(log_odds))
    Prob = percent(Probability, 2)
    print(paste("Probabilidad de default ", ASIC, ":", Prob))
  }
\keyword{ ~DefaultMEM }

