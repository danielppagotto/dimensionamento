SELECT 
	m.cod_municipio,
    CASE
		WHEN pf.CBO LIKE '225%' OR pf.CBO LIKE '2231%' THEN 'Médico'
		WHEN pf.CBO LIKE '2234%' THEN 'Farmacêutico'
		WHEN pf.CBO LIKE '2235%' THEN 'Enfermeiro'
		WHEN pf.CBO LIKE '2236%' THEN 'Fisioterapeuta'
		WHEN pf.CBO LIKE '2237%' THEN 'Nutricionista'
		WHEN pf.CBO = '515105' THEN 'Agente Comunitário de Saúde'
		WHEN pf.CBO = '322205' OR pf.CBO = '322230' THEN 'Técnico/Auxiliar de Enfermagem'
		WHEN pf.CBO = '251605' THEN 'Assistente Social'
		WHEN pf.CBO = '221105' THEN 'Biólogo'
		WHEN pf.CBO = '221205' THEN 'Biomédico'
		WHEN pf.CBO LIKE '2241%' THEN 'Profissional da Educação Física'
		WHEN pf.CBO LIKE '2238%' THEN 'Fonoaudiólogo'
		WHEN pf.CBO = '223305' THEN 'Médico Veterinário'
		WHEN pf.CBO LIKE '2232%' THEN 'Cirurgiões-dentistas'
		WHEN pf.CBO = '251510' THEN 'Psicólogo Clínico'
		WHEN pf.CBO = '223905' THEN 'Terapeuta Ocupacional'
		WHEN pf.CBO = '324115' THEN 'Técnico em radiologia e imagenologia'
		WHEN pf.CBO = '322405' THEN 'Técnico em saúde bucal'
    END AS categoria,
    CASE
        WHEN o.titulo IS NULL THEN 
                                    CASE
										WHEN pf.CBO LIKE '2232%' THEN 'Cirurgiões-dentistas'
									END
        ELSE o.titulo
    END AS especialidade,
    SUM(HORAOUTR+HORAHOSP+HORA_AMB) AS FTE
FROM Dados.cnes.PF pf
LEFT JOIN
    Dados.cbo2002."ocupacao.parquet" o ON pf.cbo = TO_CHAR(o.codigo, '#')
LEFT JOIN
    "Analytics Layer".Territorial."Municípios - Hierarquia Completa" m on CAST(m.cod_municipio AS VARCHAR) = pf.CODUFMUN
WHERE
    SUBSTR(pf.COMPETEN, 0, 4) = 2021 AND
	SUBSTR(pf.COMPETEN, 5, 2) = 12 AND
    (
		(pf.CBO LIKE '2232%') -- Cirurgiões-dentistas
    )
GROUP BY
    m.cod_municipio,
    categoria,
    especialidade