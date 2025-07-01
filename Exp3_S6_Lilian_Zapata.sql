/*
Formativa semana 6
Lilian Zapata
Los grupos son individuales
*/

-- CREA O REEMPLAZA EL PROCEDIMIENTO GENERAR_PAGO_CERO
CREATE OR REPLACE PROCEDURE generar_pago_cero (
    -- PARAMETROS
    p_periodo_abril IN NUMBER,                             -- PARAMETRO PARA ABRIL
    p_periodo_mayo IN NUMBER                               -- PARAMETRO PARA MAYO
) IS
    -- VARIABLES
    c_multa_2uf CONSTANT NUMBER := 2 * 30000;              -- CTE PARA LA MULTA, CONSIDERANDO UN VALOR DE 30000
    c_multa_4uf CONSTANT NUMBER := 4 * 30000;              -- CTE PARA LA MULTA DE 4 UF, MISMO VALOR DE ARRIBA
    v_fecha_corte VARCHAR2(20);
BEGIN
    v_fecha_corte := TO_CHAR(SYSDATE - 1, 'DD/MM/YYYY');   -- CONSIDERA DIA DE AYER PARA LA CONSULTA

    --ELIMINA GASTO COMUN PAGO CERO 
    DELETE FROM GASTO_COMUN_PAGO_CERO 
    WHERE anno_mes_pcgc = p_periodo_mayo;

    -- INSERTA LOS DATOS EN LA TABLA GASTO COMUN PAGO CERO 
    INSERT INTO GASTO_COMUN_PAGO_CERO (
        -- COLUMNAS DE LA TABLA
        anno_mes_pcgc,              -- PERIODO DE MAYO
        id_edif,                    -- IDE EDIFICIO
        nombre_edif,                -- NOMBRE EDIFICIO
        run_administrador,          -- RUN ADMINISTRADOR
        nombre_admnistrador,        -- NOMBRE ADMINISTRADOR
        nro_depto,                  -- NRO DEPTO
        run_responsable_pago_gc,    -- RUN DE QUIEN PAGA
        nombre_responsable_pago_gc, -- NOMBRE DE QUIEN PAGA
        valor_multa_pago_cero,      -- VALOR DE LA MULTA
        observacion                 -- DESCRIPCION DE LA MULTA
    )
    SELECT
        -- VALORES QUE SE INSERTAN EN LAS COLUMNAS
        p_periodo_mayo,             -- SELECCIONA EL PARAMETRO DE MAYO
        base.id_edif,               -- SELECCIONA EL ID DEL EDIFICIO
        base.nombre_edif,           -- SELECCIONA EL NOMBRE DEL EDIFICIO
        base.run_admin,             -- SELECCIONA RUN DEL ADMINISTRADOR
        base.nombre_admin,          -- SELECCIONA EL NOMBRE DEL ADMINISTRADOR
        base.nro_depto,             -- SELECCIONA EL DPTO
        base.run_responsable,       -- SELECCIONA EL RUN DE QUIEN PAGA
        base.nombre_responsable,    -- SELECCION EL NOMBRE DE QUIEN PAGA
        -- CASE PARA VER DE CUANTO ES LA MULTA
        CASE base.uf_multa WHEN 4 THEN c_multa_4uf ELSE c_multa_2uf END,
        -- CASE PARA VER LA INFORMACION DE LA MULTA
        CASE base.uf_multa 
            WHEN 4 THEN 'Multa 4 UF. Se realizara el corte de combustible y agua a contar del ' || v_fecha_corte
            ELSE 'Multa 2 UF. Se realizará el corte de combustible y agua'
        END
    FROM (
        SELECT
            ed.id_edif,                                                  -- ID DE EDIFICIO
            ed.nombre_edif,                                              -- NOMBRE DEL EDIDICIO
            ad.numrun_adm || '-' || ad.dvrun_adm AS RUN_ADMIN,            -- RUN ADMINISTRADOR CONCATENADO
            ad.pnombre_adm || ' ' || NVL(ad.snombre_adm, '') || ' ' || ad.appaterno_adm || ' ' || NVL(ad.apmaterno_adm, '') AS NOMBRE_ADMIN, -- NOMBRE DE ADMINISTRADOR
            gc.nro_depto,                                               -- NUMERO DE DEPARTAMENTO
            rpgc.numrun_rpgc || '-' || rpgc.dvrun_rpgc AS RUN_RESPONSABLE,    -- RUND E QUIEN REALIZA EL PAGO
            rpgc.pnombre_rpgc || ' ' || NVL(rpgc.snombre_rpgc, '') || ' ' || rpgc.appaterno_rpgc || ' ' || NVL(rpgc.apmaterno_rpgc, '') AS NOMBRE_RESPONSABLE, -- NOMBRE DE QUIEN HACE EL PAGO
            -- CASE PATA VER SI HAY UN GGCC DE ABRILQ QUE NO SE PAGO PAA VER LA MULTA
            CASE
                WHEN EXISTS (
                    SELECT 1 FROM GASTO_COMUN GC_ANT                          -- SE VI EXISTEN PAGOS ANTERIORES
                    WHERE gc_ant.anno_mes_pcgc < p_periodo_abril              -- SOLO SE CONSIDERAN LOS PAGOS ANTERIORES A ABRIL
                      AND gc_ant.id_edif = gc.id_edif                         -- COMPARA QUE EL ID DEL EDIFICIO SEA EL MISMO
                      AND gc_ant.nro_depto = gc.nro_depto                     -- COMPARA QUE LOS NROS DE DPTOS SEAN LOS MISMOS
                      AND gc_ant.numrun_rpgc = gc.numrun_rpgc                 -- COMPARA QUE LOS RUN SEAN LOS MISMOS DE QUIEN PAGA
                      AND NOT EXISTS (                                        -- VE SI ES QUE NO EXISTE ALGUN PAGO ANTERIOR
                          SELECT 1 FROM PAGO_GASTO_COMUN PGC_ANT              -- SE VE SI ESQUE HAY ALGUN PAGO NO ANOTADO
                          WHERE pgc_ant.anno_mes_pcgc = gc_ant.anno_mes_pcgc  -- COMPARA EL PERIODO CON EL PAGO DEL GGCC
                            AND pgc_ant.id_edif = gc_ant.id_edif              -- COMPARA QUE LOS ID DE LOS EDIFICIOS SEAN LOS MISMOS EN LAS TABLAS
                            AND pgc_ant.nro_depto = gc_ant.nro_depto          -- COMPARA QUE EL NRO DE DPTO SEA EL MISMO EN LAS TABLAS
                      )
                ) THEN 4 ELSE 2
            END AS UF_MULTA
        FROM GASTO_COMUN gc
        JOIN EDIFICIO ed ON gc.id_edif = ed.id_edif                              -- UNION DE TABLAS POR ID DE EDIFICIO
        JOIN ADMINISTRADOR ad ON ed.numrun_adm = ad.numrun_adm                    -- UNION DE TABLAS POR RUN DE ADMINISTRADOR
        JOIN RESPONSABLE_PAGO_GASTO_COMUN rpgc ON gc.numrun_rpgc = rpgc.numrun_rpgc  -- UNION DE TABLAS POR RUN DE QUIEN PAGA
        WHERE gc.anno_mes_pcgc = p_periodo_abril                               -- SE VE LOS PAGO DE ABRIL
          AND NOT EXISTS (    -- VER SI NO EXISTE ALGUN PAGO
                SELECT 1 FROM PAGO_GASTO_COMUN pgc            -- VER LOS PAGOS DE LA TABLA
                WHERE pgc.anno_mes_pcgc = gc.anno_mes_pcgc    -- COMPARA PERIDO DE PAGO CON MES ACTUAL
                  AND pgc.id_edif = gc.id_edif                -- COMAPRACION DE LOS ID DE EDIFICIOS DE DISTINTAS TABLAS
                  AND pgc.nro_depto = gc.nro_depto            -- COMPARACION DE LOS NRO DE DPTO DE DISTINTAS TABLAS
          ) -- FILTRO PARA VER QUIEN NO HA PAGADO
    ) base;

    COMMIT;  -- CONFIRMA LOS CAMBIOS
EXCEPTION
    WHEN OTHERS THEN
        ROLLBACK; -- SI HAY ERROR DESHACE LOS CAMBIOS QUE SE REALIZARON
        -- ERROR PERSONALIZADO
        RAISE_APPLICATION_ERROR(-20001, 'Error al generar el reporte de pago cero: ' || SQLERRM);
END;
/


-- CONSULTA PARA VER LOS QUE NO HAN PAGADO
SELECT 
    gcpc.anno_mes_pcgc,                 -- PERIDO 
    gcpc.id_edif,                       -- ID DEL EDIFICIO
    gcpc.nombre_edif,                   -- NOMBRE DEL EDIFICIO
    gcpc.run_administrador,             -- RUN ADMINISTRADOR
    gcpc.nombre_admnistrador,           -- NOMBRE DEL ADMINISTRADOR
    gcpc.nro_depto,                     -- DPTO 
    gcpc.run_responsable_pago_gc,       -- RUN DE QUIEN PAGA
    gcpc.nombre_responsable_pago_gc,    -- NOMBRE DE QUIEN PAGA
    CASE   -- CASE PARA VER LA INFORMACION DE LA MULTA
        WHEN gcpc.valor_multa_pago_cero = 120000 THEN 
            'Multa 4 UF. Se realizara el corte de combustible y agua a contar del ' || TO_CHAR(SYSDATE - 1, 'DD/MM/YYYY')  -- FORMATEO Y ANUNCIO DE LA FECHA DE CORTE
        ELSE 
            'Multa 2 UF. Se realizará el corte de combustible y agua'
    END AS OBSERVACION
FROM 
    GASTO_COMUN_PAGO_CERO gcpc
ORDER BY 
    gcpc.anno_mes_pcgc ASC,  -- ORDENAR POR PERIODO ASC
    gcpc.nombre_edif ASC,    -- ORDENAR POR NOMBBE ED ASC
    gcpc.nro_depto ASC;      -- ORDENAR POR DPTO ASC
    
   
-- CONSULTA PARA CONCOCER LOS DPTOS PAGO CERO 
SELECT 
    anno_mes_pcgc,                                           -- FECHA PROCEDIMIENTO    
    id_edif,                                                 -- ID EDIFICIO
    nro_depto,                                               -- NUMERP DPTO
    TO_CHAR(fecha_desde_gc, 'DD/MM/YYYY') AS FECHA_DESDE_GC, -- FORMATEO FECHA INICIO GC
    TO_CHAR(fecha_hasta_gc, 'DD/MM/YYYY') AS FECHA_HASTA_GC, -- FORMATEO FECHA MAX GC
    multa_gc                                                 -- CANTIDAD MULTA
FROM 
    GASTO_COMUN  -- TABLA DE DONDE SE VE LA INFO
ORDER BY 
    anno_mes_pcgc ASC,   -- ORDEN DE PERIODO ASC
    id_edif ASC,         -- ORDEN ID EDIFICIO ASC
    nro_depto ASC;       -- ORDEN POR DPTO ASC