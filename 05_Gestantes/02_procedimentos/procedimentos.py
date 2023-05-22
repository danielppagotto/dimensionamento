

= Table.AddColumn(#"Tipo Alterado1", "Personalizar", each Text.Combine({Date.ToText([mes_ano], "MM"), "-", Date.ToText([mes_ano], "yyyy")}), type text)

# usar esse aqui	
= Table.AddColumn(#"Colunas Removidas", "Personalizar", each Text.Combine({Date.ToText([mes_ano], "yyyy"), "-", Date.ToText([mes_ano], "MM")}), type text)

= Json.Document(
  Web.Contents(
    Text.Combine({
      "http://200.137.215.27:5025/calcula_procedimentos?mes_ano=",[ano_mes],"&nascidos_vivos=", Number.ToText([total_nasc])})
  )
)




= Table.Group(#"Colunas Renomeadas2", {"codibge", "macrorregiao_pad.2", "api.codigo_sigtap", "api.procedimento", "api.mes_procedimento"}, {{"total_procedimentos", each List.Sum([api.quantidade]), type number}})