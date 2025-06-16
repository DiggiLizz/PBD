/*
FORMATIVA SEMANA 4
LILIAN ZAPATA
15/6/25
*/


-- VER USUARIO CREADO
SHOW USER;

-- CASO 1
-- VARIABLE BIND
define b_anio = 2025;

DECLARE
  --CONSTANTE PARA PUNTOS NORMALES POR CADA 100MIL
  c_puntos_normales CONSTANT NUMBER := 250;

  --VARRAY PARA PUNTOS EXTRAS POR TRAMOS
  TYPE puntos_extras_array IS VARRAY(3) OF NUMBER;
  v_puntos_extras puntos_extras_array := puntos_extras_array(300, 550, 700);

  -- VARIABLE DE TRAMOS
  v_tramo1_inf NUMBER := 500000;
  v_tramo1_sup NUMBER := 700000;
  v_tramo2_sup NUMBER := 900000;

  -- CURSOR PARA GUARDAR LOS DATOS DE TRANSACCIONES
  CURSOR c_transacciones(p_anio NUMBER) IS
    SELECT cli.numrun, cli.dvrun, cli.cod_tipo_cliente,
           trj.nro_tarjeta, tran.nro_transaccion,
           tran.fecha_transaccion, tpt.nombre_tptran_tarjeta AS tipo_transaccion,
           tran.monto_transaccion
    FROM CLIENTE cli
    JOIN TARJETA_CLIENTE trj ON cli.numrun = trj.numrun                                    -- UNION DE TABLA TARJETA CLIENRE CON CLIENTE MEDIANTE NUMERO RUN
    JOIN TRANSACCION_TARJETA_CLIENTE tran ON trj.nro_tarjeta = tran.nro_tarjeta            -- UNION TABLAS TRANSACCION_TARJETA CLIENTE CON TARJETA MEDIANTE EL NUMERO DE TARJ
    JOIN TIPO_TRANSACCION_TARJETA tpt ON tran.cod_tptran_tarjeta = tpt.cod_tptran_tarjeta  -- UNION TABLAS TIPO TRANSACCION TARJETA CON TRANSACCION MEDIANTE COD TIPO TRANSACCION TARJETA
    WHERE EXTRACT(YEAR FROM tran.fecha_transaccion) = p_anio;                              -- DONDE SE SACA EL AÑO DE LA TRANSACCION

  --VARIABLE QUE RECORRE EL CURSOR
  v_reg c_transacciones%ROWTYPE;

  --VARIABLES
  v_puntos NUMBER;
  v_puntos_totales NUMBER := 0;
  v_mes_anno VARCHAR2(6);

BEGIN
  --TRUNCAR LAS TABLAS PARA COMENZAR CON TABLA LIMPIA
  EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_PUNTOS_TARJETA_CATB';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_PUNTOS_TARJETA_CATB';

  --RECORRER EL RESGISTRO 
  FOR v_reg IN c_transacciones(&b_anio - 1) LOOP

    --CONOCER MES Y AÑO
    v_mes_anno := TO_CHAR(v_reg.fecha_transaccion, 'MMYYYY');

    --CALCULO PARA LOS PUNTOS NORMALES
    v_puntos := TRUNC(v_reg.monto_transaccion / 100000) * c_puntos_normales;

    --CALCULO CON IF PARA LOS PUNTOS SEGUN EL TIPO DE CLIENTE
    IF v_reg.cod_tipo_cliente IN (30, 40) THEN
      v_puntos := v_puntos + CASE
        WHEN v_reg.monto_transaccion BETWEEN v_tramo1_inf AND v_tramo1_sup THEN
          TRUNC(v_reg.monto_transaccion / 100000) * v_puntos_extras(1)
        WHEN v_reg.monto_transaccion > v_tramo1_sup AND v_reg.monto_transaccion <= v_tramo2_sup THEN
          TRUNC(v_reg.monto_transaccion / 100000) * v_puntos_extras(2)
        WHEN v_reg.monto_transaccion > v_tramo2_sup THEN
          TRUNC(v_reg.monto_transaccion / 100000) * v_puntos_extras(3)
        ELSE 0
      END;
    END IF;

    --INSER TPARA LA TABLA DETALLE_PUNTOS_TARJETA_CATB
    INSERT INTO DETALLE_PUNTOS_TARJETA_CATB(
        -- COLUMNAS DE LA TABLA
        numrun, 
        dvrun, 
        nro_tarjeta, 
        nro_transaccion, 
        fecha_transaccion,
        tipo_transaccion, 
        monto_transaccion, 
        puntos_allthebest)
    VALUES   -- VALORES DE LA TABLA
      (v_reg.numrun, 
       v_reg.dvrun, 
       v_reg.nro_tarjeta, 
       v_reg.nro_transaccion,
       v_reg.fecha_transaccion, 
       v_reg.tipo_transaccion, 
       v_reg.monto_transaccion, 
       v_puntos);

    BEGIN
      UPDATE RESUMEN_PUNTOS_TARJETA_CATB    -- ACTIALIZACION DE LA TABLA MEDIANTE CASE
      SET 
        monto_total_compras = CASE WHEN v_reg.tipo_transaccion = 'Compras Tiendas Retail o Asociadas'
                                   THEN monto_total_compras + v_reg.monto_transaccion ELSE monto_total_compras END,
        total_puntos_compras = CASE WHEN v_reg.tipo_transaccion = 'Compras Tiendas Retail o Asociadas'
                                   THEN total_puntos_compras + v_puntos ELSE total_puntos_compras END,
        monto_total_avances = CASE WHEN v_reg.tipo_transaccion = 'Avance en Efectivo'
                                   THEN monto_total_avances + v_reg.monto_transaccion ELSE monto_total_avances END,
        total_puntos_avances = CASE WHEN v_reg.tipo_transaccion = 'Avance en Efectivo'
                                   THEN total_puntos_avances + v_puntos ELSE total_puntos_avances END,
        monto_total_savances = CASE WHEN v_reg.tipo_transaccion = 'Súper Avance en Efectivo'
                                   THEN monto_total_savances + v_reg.monto_transaccion ELSE monto_total_savances END,
        total_puntos_savances = CASE WHEN v_reg.tipo_transaccion = 'Súper Avance en Efectivo'
                                   THEN total_puntos_savances + v_puntos ELSE total_puntos_savances END
      WHERE mes_anno = v_mes_anno;  -- VER SI YA EXISTE ESTA FILA SEGUN EL MES DEL AÑO

      IF SQL%ROWCOUNT = 0 THEN    -- SI NO SE ENCUENTRA SE CREA CON VALORES SEGUN LA TRANSACCION
        INSERT INTO RESUMEN_PUNTOS_TARJETA_CATB
        VALUES (
          v_mes_anno,
          CASE WHEN v_reg.tipo_transaccion = 'Compras Tiendas Retail o Asociadas' THEN v_reg.monto_transaccion ELSE 0 END,
          CASE WHEN v_reg.tipo_transaccion = 'Compras Tiendas Retail o Asociadas' THEN v_puntos ELSE 0 END,
          CASE WHEN v_reg.tipo_transaccion = 'Avance en Efectivo' THEN v_reg.monto_transaccion ELSE 0 END,
          CASE WHEN v_reg.tipo_transaccion = 'Avance en Efectivo' THEN v_puntos ELSE 0 END,
          CASE WHEN v_reg.tipo_transaccion = 'Súper Avance en Efectivo' THEN v_reg.monto_transaccion ELSE 0 END,
          CASE WHEN v_reg.tipo_transaccion = 'Súper Avance en Efectivo' THEN v_puntos ELSE 0 END
        );
      END IF;
    END;

  END LOOP;

  -- Confirmar la transaccion
  COMMIT;

END;
/

--CONSULTA DE LA TABLA SEGUN REQUISITO DE GUIA
SELECT 
  numrun AS "NUMRUN",
  dvrun AS "DV",
  nro_tarjeta AS "NRO_TARJETA",
  nro_transaccion AS "NRO_TRANSACCION",
  TO_CHAR(fecha_transaccion, 'YYYY/MM/DD') AS "FECHA_TRANSACCION",
  tipo_transaccion AS "TIPO_TRANSACCION",
  monto_transaccion AS "MONTO_TRANSACCION",
  puntos_allthebest AS "PUNTOS_ALLTHEBEST"
FROM DETALLE_PUNTOS_TARJETA_CATB
ORDER BY fecha_transaccion, numrun, nro_transaccion;


-- CONSULTA PARA VER EL RESUMEN MENSUAL DE LAS TRANSACCIONES Y PUNTOS
SELECT *
FROM RESUMEN_PUNTOS_TARJETA_CATB
ORDER BY mes_anno;
    
    
------------------------------------------------------------------------------------------------------
--CASO 2
DECLARE
    --VARIABLE DE AÑO ACTUAL
    v_anio NUMBER := EXTRACT(YEAR FROM SYSDATE);

    --CURSOR
    CURSOR c_transacciones IS
    SELECT cli.numrun, 
           cli.dvrun, 
           trjcli.nro_tarjeta, 
           trantrjcli.nro_transaccion, 
           trantrjcli.fecha_transaccion,
           ttran.nombre_tptran_tarjeta AS tipo_transaccion,
           trantrjcli.monto_total_transaccion
    FROM CLIENTE cli
    JOIN TARJETA_CLIENTE trjcli ON cli.numrun = trjcli.numrun                                       -- UNION DE TABLAS MEDIATE EL NUMERO DE RUN
    JOIN TRANSACCION_TARJETA_CLIENTE trantrjcli ON trjcli.nro_tarjeta = trantrjcli.nro_tarjeta      -- UNION DE TABLAS MEDIANTE EL NUMERO DE TARJEYA
    JOIN TIPO_TRANSACCION_TARJETA ttran ON trantrjcli.cod_tptran_tarjeta = ttran.cod_tptran_tarjeta -- UNION DE TABLAS MEDIANTE EL COD TRANSAC DE LA TARJETA
    WHERE ttran.nombre_tptran_tarjeta IN ('Avance en Efectivo', 'Súper Avance en Efectivo')
    AND EXTRACT(YEAR FROM trantrjcli.fecha_transaccion) = v_anio
    --ORDEN
    ORDER BY trantrjcli.fecha_transaccion, cli.numrun;

    -- VARIABLES
    v_rec c_transacciones%ROWTYPE;
    v_aporte NUMBER;

BEGIN
    --TRUNCAR TABLAS PARA COMENZAR CON TABLAS LIMPIAS
    EXECUTE IMMEDIATE 'TRUNCATE TABLE DETALLE_APORTE_SBIF';
    EXECUTE IMMEDIATE 'TRUNCATE TABLE RESUMEN_APORTE_SBIF';

    FOR v_rec IN c_transacciones LOOP
        -- SELECT PARA VER EL %
        SELECT porc_aporte_sbif
        INTO v_aporte
        FROM TRAMO_APORTE_SBIF
        WHERE v_rec.monto_total_transaccion BETWEEN tramo_inf_av_sav AND tramo_sup_av_sav;

        -- Insertar en tabla de detalle
        INSERT INTO DETALLE_APORTE_SBIF (
            numrun, dvrun, nro_tarjeta, nro_transaccion, fecha_transaccion,
            tipo_transaccion, monto_transaccion, aporte_sbif
        ) VALUES (
            v_rec.numrun, v_rec.dvrun, v_rec.nro_tarjeta, v_rec.nro_transaccion,
            v_rec.fecha_transaccion, v_rec.tipo_transaccion, v_rec.monto_total_transaccion,
            ROUND(v_rec.monto_total_transaccion * v_aporte / 100)
        );
    END LOOP;

    FOR mes IN (SELECT DISTINCT TO_CHAR(fecha_transaccion, 'MMYYYY') AS mes_anno FROM DETALLE_APORTE_SBIF ORDER BY 1) LOOP
        FOR resumen IN (
            SELECT tipo_transaccion, SUM(monto_transaccion) AS total_monto
            FROM DETALLE_APORTE_SBIF
            WHERE TO_CHAR(fecha_transaccion, 'MMYYYY') = mes.mes_anno
            GROUP BY tipo_transaccion
            ORDER BY tipo_transaccion
        ) LOOP
            --VARIABLES
            DECLARE
                v_tipo_transaccion DETALLE_APORTE_SBIF.tipo_transaccion%TYPE := resumen.tipo_transaccion;
                v_mes_anno VARCHAR2(6) := mes.mes_anno;
                v_total_monto NUMBER := resumen.total_monto;
                v_total_aporte NUMBER;
            BEGIN
                SELECT SUM(aporte_sbif)
                INTO v_total_aporte
                FROM DETALLE_APORTE_SBIF
                WHERE TO_CHAR(fecha_transaccion, 'MMYYYY') = v_mes_anno
                AND tipo_transaccion = v_tipo_transaccion;

                --INSERTAR EN LA TABLA RESUMEN
                INSERT INTO RESUMEN_APORTE_SBIF (
                    -- COLUMNAS DE LAS TABLAS
                    mes_anno, 
                    tipo_transaccion, 
                    monto_total_transacciones, 
                    aporte_total_abif
                ) VALUES (  -- VALORES DE LAS COLUMNAS
                    v_mes_anno, 
                    v_tipo_transaccion, 
                    v_total_monto, 
                    v_total_aporte);
            END;
        END LOOP;
    END LOOP;

    COMMIT;
END;
/
-- CONSULTA SEGUN IMAGEN DE REFERENCIA DE LA GUIA
SELECT
    numrun,
    dvrun,
    nro_tarjeta,
    nro_transaccion,
    TO_CHAR(fecha_transaccion, 'DD/MM/YYYY') AS "FECHA_TRANSACCION",
    tipo_transaccion,
    monto_transaccion AS "MONTO_TOTAL_TRANSACCION",
    aporte_sbif
FROM
    DETALLE_APORTE_SBIF
WHERE
    tipo_transaccion IN ('Avance en Efectivo', 'Súper Avance en Efectivo')
ORDER BY
    fecha_transaccion,
    numrun;
    
-- VER QUE VALORES HAY EN LA COLUMNA TIPO TRANSACCION, YA QUE NO SALE SUPER AVACE COMO EN LA GUIA
SELECT DISTINCT tipo_transaccion FROM DETALLE_APORTE_SBIF;

--CONULTA TABLA RESUMEN APORTE SBIF SEGUN IMAGEN GUIA
SELECT
    mes_anno,
    tipo_transaccion,
    monto_total_transacciones,
    aporte_total_abif
FROM
    RESUMEN_APORTE_SBIF
ORDER BY
    mes_anno,
    tipo_transaccion;