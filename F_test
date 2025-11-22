compare_models <- function(df, dep, indep_res, indep_unres)
{
  linear_reg() |> 
    fit_xy(
      y = df |> select(all_of(dep)),
      x = df |> select(all_of(indep_unres))
    ) |> 
      glance() |> 
      mutate(model = "unrestricted")->mod_unres
  
  #modelo restringido
  linear_reg() |> 
    fit_xy(
      y = df |> select(all_of(dep)),
      x = df |> select(all_of(indep_res))
    ) |> 
      glance() |> 
      mutate(model = "restricted")-> mod_res
  bind_rows(mod_unres,mod_res) |> 
    select(model,everything()
)
}
