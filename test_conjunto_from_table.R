test_conjunto_from_table <- function(tabla, alpha = 0.05) {
  
  # separar modelos
  unres <- tabla |> filter(model == "unrestricted")
  res   <- tabla |> filter(model == "restricted")
  
  # extraer valores
  SSR_u  <- unres$deviance
  SSR_r  <- res$deviance
  df_u   <- unres$df.residual
  df_r   <- res$df.residual
  
  # número de restricciones Q
  q <- df_r - df_u
  
  # F conjunto
  F_test <- ((SSR_r - SSR_u) / q) / (SSR_u / df_u)
  
  # p-value
  p_value <- pf(F_test, q, df_u, lower.tail = FALSE)
  
  # decisión
  decision <- ifelse(p_value < alpha, 
                     "RECHAZO H0: las restricciones NO son válidas",
                     "NO rechazo H0: las restricciones son válidas")
  
  tibble(
    F_test = F_test,
    df_num = q,
    df_den = df_u,
    p_value = p_value,
    decision = decision
  )
}
