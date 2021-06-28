lendo <- function(caminho, planilha){
  caminho |> read_excel(sheet = planilha, na = "-") |>
    mutate(D4 = ifelse(D4 =="x",NA,D4)) |>
    select("NívelCálcio","Repetição","Altura",
           "AlturaAlbumen",
           "D1","D2","D3","D4","DiaOvo","Esp1",
           "Esp2","Esp3","Esp4","FFT_R","FREQ_MED",
           "Freq1","Freq2","Freq3","Freq4",
           "PesoCasca","PesoOvo")
}
