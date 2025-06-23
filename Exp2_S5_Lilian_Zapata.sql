/* SEMANA 5 PROGRAMACION BASE DE DATOS
LILIAN ZAPATA 22-6-25
*/

--CASO 1  SE HAN BORRADO LAS TABLAS PARA DEJAR SOLO LA CONSULTA

DECLARE
  -- CREACION DEL REGISTRO DE DETALLE USANDO EL MISMO TIPO DE DATOS
  TYPE r_detalle IS RECORD (
        numrun cliente.numrun%TYPE,                                               -- NUMERO DE RUN DEL CLIENTE
        dvrun cliente.dvrun%TYPE,                                                 -- DV DEL RUN DEL CLIENTE
        nro_tarjeta tarjeta_cliente.nro_tarjeta%TYPE,                             -- NUMERO DE TARJETA DEL CLIENTE
        nro_transaccion transaccion_tarjeta_cliente.nro_transaccion%TYPE,         -- NUMERO DE TRANSACCION
        fecha_transaccion transaccion_tarjeta_cliente.fecha_transaccion%TYPE,     -- FECHA DE LA TRANSACCION
        tipo_transaccion tipo_transaccion_tarjeta.nombre_tptran_tarjeta%TYPE,     -- NOMBRE DEL TIPO DE TRANSACCION
        monto_total transaccion_tarjeta_cliente.monto_total_transaccion%TYPE,     -- MONTO DE LA TRANSACCION
        tasa tipo_transaccion_tarjeta.tasaint_tptran_tarjeta%TYPE                 -- TASA DE INTERES
  );

  -- CREACION DE REGISTRO PARA RESUMEN USANDO EL MISMO TIPO DE DATOS
  TYPE r_resumen IS RECORD (
    mes_anno RESUMEN_APORTE_SBIF.mes_anno%TYPE,                                 -- MES Y AÑO DE LA TRANSACCION
    tipo_transaccion RESUMEN_APORTE_SBIF.tipo_transaccion%TYPE,                 -- TIPO DE TRANSACCION
    total_transacciones RESUMEN_APORTE_SBIF.monto_total_transacciones%TYPE      -- SUMA DE LAS TRANSACCIONES
  );

  -- VARIABLES TIPO RECORD
  v_det r_detalle;
  v_res r_resumen;

  -- VARIABLES
  v_aporte NUMBER;
  v_total_aporte NUMBER;

  -- CURSOR PARA LAS TRASNSACCIONES SOLICITADAS
  CURSOR c_detalle IS
    SELECT 
        -- COLUMNAS
        cli.numrun, 
        cli.dvrun, 
        tcli.nro_tarjeta, 
        tr.nro_transaccion, 
        tr.fecha_transaccion,
        ttt.nombre_tptran_tarjeta, 
        tr.monto_total_transaccion, 
        ttt.tasaint_tptran_tarjeta
    FROM cliente cli
    JOIN tarjeta_cliente tcli ON cli.numrun = tcli.numrun     -- UINION DE TABLAS MEDIENTE EL NUM DE RUN DEL CLIENTE
    JOIN transaccion_tarjeta_cliente tr ON tcli.nro_tarjeta = tr.nro_tarjeta  -- UNION DE TABLAS MEDIANTE EL NUMERO DE TARJETA
    JOIN tipo_transaccion_tarjeta ttt ON tr.cod_tptran_tarjeta = ttt.cod_tptran_tarjeta  -- UNION DE TABLAS MEDIANTE EL COD_TPTRANS_TARJETA
    WHERE ttt.nombre_tptran_tarjeta IN ('Avance en Efectivo', 'Súper Avance en Efectivo') -- SE BUSCA LOS AVANCES DE EFECTIVO Y SUPER AVANCE EN EFECTIVO
      AND EXTRACT(YEAR FROM tr.fecha_transaccion) = EXTRACT(YEAR FROM SYSDATE);

  -- CURSOR PARA EL RESUMEN MENSUAL 
  CURSOR c_resumen(p_tipo VARCHAR2) IS
    SELECT TO_CHAR(tr.fecha_transaccion, 'MMYYYY') AS mes_anno,  -- SE SACA MES Y AÑO
           ttt.nombre_tptran_tarjeta,    -- NOMBRE DE TIPO DE TRANSACCIOM
           SUM(tr.monto_total_transaccion)    -- SE SUMAN LAS TRAMSACCIONES
    FROM transaccion_tarjeta_cliente tr
    JOIN tipo_transaccion_tarjeta ttt ON tr.cod_tptran_tarjeta = ttt.cod_tptran_tarjeta  -- UNION MEDIANTE EL COD DE TIPO DE TRANSACCIOM
    WHERE ttt.nombre_tptran_tarjeta = p_tipo
      AND EXTRACT(YEAR FROM tr.fecha_transaccion) = EXTRACT(YEAR FROM SYSDATE)  -- SACR EL AÑO DE LA TRANSACCION
    GROUP BY TO_CHAR(tr.fecha_transaccion, 'MMYYYY'), ttt.nombre_tptran_tarjeta -- SE FORMATE SEGUN MES Y AÑO
    ORDER BY mes_anno;   -- ORDENAR SEGUN EÑ MES DEL AÑO

BEGIN
  -- TRUNCADO DE LAS TABLAS PARA PODER EMPEZAR DE CERO
  EXECUTE IMMEDIATE 'TRUNCATE TABLE detalle_aporte_sbif';
  EXECUTE IMMEDIATE 'TRUNCATE TABLE resumen_aporte_sbif';

  -- PROCESAR DETALLE DE APORTES
  OPEN c_detalle;
  LOOP
    FETCH c_detalle INTO v_det;
    EXIT WHEN c_detalle%NOTFOUND;   -- SE SALE DEL CICLO CUANDO YA NO SE ENCUENTRAN MAS DATOS

    -- CALCULO DEL MONTO CON INTERES
    v_det.monto_total := ROUND(v_det.monto_total + (v_det.monto_total * v_det.tasa));

    -- OBTENER EL PORCENTAJE DE APORTE SEGUN TRAMO
    SELECT porc_aporte_sbif INTO v_aporte
    FROM tramo_aporte_sbif
    WHERE v_det.monto_total BETWEEN tramo_inf_av_sav AND tramo_sup_av_sav;

    -- CALCULAR APORTE REDONDEADO
    v_aporte := ROUND((v_det.monto_total * v_aporte) / 100);

    -- INSERTAR DATOS EN TABLA DETALLE_APORTE_SBIF
    INSERT INTO detalle_aporte_sbif (
        -- COLUMNAS
        numrun, 
        dvrun, 
        nro_tarjeta, 
        nro_transaccion,
        fecha_transaccion, 
        tipo_transaccion, 
        monto_transaccion, 
        aporte_sbif
    ) VALUES (
        -- VALORES QUE SE INSERTAN
        v_det.numrun, 
        v_det.dvrun, 
        v_det.nro_tarjeta, 
        v_det.nro_transaccion,
        v_det.fecha_transaccion, 
        v_det.tipo_transaccion, 
        v_det.monto_total, 
        v_aporte
    );
  END LOOP;
  CLOSE c_detalle;

  -- Procesar resumen mensual de aportes por tipo
  FOR tipo_row IN (SELECT DISTINCT nombre_tptran_tarjeta FROM tipo_transaccion_tarjeta
                   WHERE nombre_tptran_tarjeta IN ('Avance en Efectivo', 'Súper Avance en Efectivo')) LOOP  -- SE VEN LOS TIPOS DE AVANCE EN EFECTIVO

    OPEN c_resumen(tipo_row.nombre_tptran_tarjeta);
    LOOP
      FETCH c_resumen INTO v_res; -- SE GUARDA C RESUMEN EN EL V RES
      EXIT WHEN c_resumen%NOTFOUND; -- SI LLEGA AL FINAL SE CIERRA EL CICLO

      -- CALCULAR APORTE TOTAL AGRUPADO POR MES Y TIPO
      SELECT SUM(ROUND(d.monto_transaccion * t.porc_aporte_sbif / 100))
      INTO v_total_aporte
      FROM detalle_aporte_sbif d
      JOIN tramo_aporte_sbif t ON d.monto_transaccion BETWEEN t.tramo_inf_av_sav AND t.tramo_sup_av_sav -- UNION DE TABLAS SEGUN EL MONTO Y TRAMO INF Y SUP
      WHERE TO_CHAR(d.fecha_transaccion, 'MMYYYY') = v_res.mes_anno  -- FORMATEO DE LA FECHA SEGUN MES Y AÑO
        AND d.tipo_transaccion = v_res.tipo_transaccion;

      -- INSERTAR RN LA TABLA RESUMEN
      INSERT INTO resumen_aporte_sbif (
          -- COLUMNAS  
          mes_anno, 
          tipo_transaccion, 
          monto_total_transacciones, 
          aporte_total_abif
      ) VALUES (
          -- VALORES
          v_res.mes_anno, 
          v_res.tipo_transaccion, 
          v_res.total_transacciones, 
          v_total_aporte
      );
    END LOOP;
    CLOSE c_resumen;
  END LOOP;

  COMMIT; -- CONFIRMAR CAMBIOS
  
-- USO DE EXECPIONES
EXCEPTION
  WHEN OTHERS THEN
    DBMS_OUTPUT.PUT_LINE('Error: ' || SQLERRM); -- DEVUELVE EL ERROR
    ROLLBACK; -- SI NO HAY ERROR, VOLVER
END;
/


-- TABLA RESUMEN_APORTE_SBIF 
SELECT 
  mes_anno AS "MES_ANNO",
  tipo_transaccion AS "TIPO_TRANSACCION",
  monto_total_transacciones AS "MONTO_TOTAL_TRANSACCIONES",
  aporte_total_abif AS "APORTE_TOTAL_ABIF"
FROM resumen_aporte_sbif
ORDER BY mes_anno, tipo_transaccion; -- ORDENA SEGUN FECHA Y TIPO DE TRANSACCION


-- TABLA DETALLE_APORTE_SBIF 
SELECT 
  numrun,  -- NUMERO DE RJN
  dvrun,   -- DV DEL RUN
  nro_tarjeta, -- NUMERO DE TARJETA
  nro_transaccion, -- NUMERO DE LA TRANSACCION
  TO_CHAR(fecha_transaccion, 'DD/MM/YYYY') AS FECHA_TRANSACCION, -- FORMATE DE LA FECHA
  tipo_transaccion,  -- TIPO DE TRANSACCION
  monto_transaccion AS MONTO_TOTAL_TRANSACCION, -- MONTO DE LA TRANSACCION
  aporte_sbif
FROM detalle_aporte_sbif
WHERE EXTRACT(YEAR FROM fecha_transaccion) = EXTRACT(YEAR FROM SYSDATE)
ORDER BY fecha_transaccion, numrun; -- ORDENAR SEEGUN FECHA Y NUMERO DE RUN

-- los supr avance en efectico no se ven en la tabla ya que son del 2021, y piden otro año