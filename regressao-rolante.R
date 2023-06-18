

library(tidyverse)
library(janitor)

#teste 2

roda_regressao = function(bog_rf, ibov_rf, smb, hml, wml){

  dados <- tibble(
    bog_rf = bog_rf,
    ibov_rf = ibov_rf,
    smb = smb,
    hml = hml,
    wml = wml
  )

  modelo = lm(formula = bog_rf ~ ibov_rf + smb + hml + wml, data = dados)

  broom::tidy(modelo) %>%
    select(
      term,
      estimate
    ) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate
    )


}

dados <- read_csv2("dados.txt") %>%
  clean_names() %>%
  mutate(
    data = make_date(ano, mes, 1)
  )


results = slider::pslide_dfr(
  .l = list(dados$bog_rf, dados$ibov_rf, dados$smb, dados$hml, dados$wml),
  .f = roda_regressao,
  .before = 12
)


coeficientes <- dados %>%
  select(
    data
  ) %>%
  bind_cols(
    results
  )


print(coeficientes)
