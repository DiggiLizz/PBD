/*
SUMATIVA 3
PROGRAMACION BASE DE DATOS
LILIAN ZAPATA
*/
-- SE CARGARON LAS TABLAS Y SE ELIMINARON DEL SCRIPT
-- PAQUETE PARA EL CALCULO DE REMUNERACIONES Y ERRORES
CREATE OR REPLACE PACKAGE pkg_remuneraciones IS
    -- FUNCION PARA CALCULAR EL BONO POR AÑOS DE SERVICIO
    FUNCTION f_calc_bonif_annos(
        p_fec_ingreso DATE, -- FECHA DE INGRESO
        p_fecha_proc DATE   -- FECHA DEL PROCESO
    ) RETURN NUMBER;        -- DEVUELVE EL VALOR DEL BONO
        -- VARIABLE
        g_pct_movilizacion NUMBER;
END pkg_remuneraciones;
/

-- CUERPO DEL PAQUETE
CREATE OR REPLACE PACKAGE BODY pkg_remuneraciones IS

    FUNCTION f_calc_bonif_annos(
        p_fec_ingreso DATE,
        p_fecha_proc  DATE
    ) RETURN NUMBER IS
        v_annos        NUMBER;                         -- VARIABLE DE LOS AÑOS TRABAJADOS
        v_pct_bonif    NUMBER := 0;                    -- % DEL BONO SEGUN LOS TRMAOS
        v_sueldo_base  empleado.sueldo_base_emp%TYPE;  -- SUELDO BASE USANDO EL MISMO TIPO DE DATO DE LA TABLA EMPLEADO
        v_err_msg      VARCHAR2(2000);                 -- MENSAJE ERROR
    BEGIN
        -- CALCULO DE LOS AÑOS TRABAJAODS
        v_annos := FLOOR(MONTHS_BETWEEN(p_fecha_proc, p_fec_ingreso) / 12);
    
        -- SELECT PARA VER EL % DE BONIFICAICON
        SELECT porc_bonif
            INTO v_pct_bonif
            FROM PORC_BONIF_ANNOS_CONTRATO
         WHERE v_annos BETWEEN annos_inferior AND annos_superior;
    
        -- SELECT PARA EL SUELDO BASE
        SELECT sueldo_base_emp
            INTO v_sueldo_base
            FROM empleado
         WHERE fecing_emp = p_fec_ingreso;
    
        -- REDONDEO DEL BONO
        RETURN ROUND(v_pct_bonif * v_sueldo_base);
  -- EXCEPCION PARA CAPTURAR ERRORES
    EXCEPTION
        WHEN NO_DATA_FOUND THEN   -- SI NO SE ENCUENTRA LA INFORMACION
            v_err_msg := SQLERRM;
            INSERT INTO ERROR_PROCESO_REMUN(correl_error, rutina_error, descrip_error)
                VALUES(SEQ_ERROR.NEXTVAL, 'f_calc_bonif_annos', v_err_msg);
        RETURN 0;    -- SI NO ENCUENTRA ERROR DEVUELVE CERO (SIEMPRE DEBE DEVOLVER ALGO)

        WHEN OTHERS THEN  -- CAPTURA OTRO ERROR
            v_err_msg := SQLERRM;
            INSERT INTO ERROR_PROCESO_REMUN(correl_error, rutina_error, descrip_error)
            VALUES(SEQ_ERROR.NEXTVAL, 'f_calc_bonif_annos', v_err_msg);
        RETURN 0;
    END f_calc_bonif_annos;

END pkg_remuneraciones;               
/  




-- FUNCIONES
-- CALCULO DE LAS CARGAS FAMILIARES
CREATE OR REPLACE FUNCTION f_valor_carga_familiar(
    p_numrut_emp   IN NUMBER,   -- RUN DEL EMPLEADO
    p_valor_carga  IN NUMBER    -- VALOR DE LA CARGA
) RETURN NUMBER IS
    v_cargas NUMBER;            -- CARGAS FAMILAIRES
BEGIN
    SELECT COUNT(*)   -- CUENTA LAS CARGAS FAMILIARES Y LAS MULTIPLICA POR EL VALOR DE LA CARGA
        INTO v_cargas
        FROM CARGA_FAMILIAR
    WHERE numrut_emp = p_numrut_emp;
    RETURN ROUND(v_cargas * p_valor_carga); -- SE RETORNA EL VALOR CARGA X EL TOTAL DE CARGAS
END f_valor_carga_familiar;
/

-- CALCULO DE LA COMISION DE VENTAS POR AÑOS
CREATE OR REPLACE FUNCTION f_valor_comision_ventas(
    p_numrut_emp IN NUMBER,   -- RUN EMPLEADO
    p_mes        IN NUMBER,  -- MES DE LA BOLETA
    p_anno       IN NUMBER    -- AÑO DE LA BOLETA
    ) RETURN NUMBER IS
    v_total NUMBER;     -- SE ACUMULAN LAS COMISONES
BEGIN
    SELECT NVL(SUM(c.valor_comision), 0)      -- CALCULO PARA SUMAR LAS COMISIONES, NI NO HAY RETORNA CERO
        INTO v_total
        FROM BOLETA b
        JOIN COMISION_VENTA c ON b.nro_boleta = c.nro_boleta
    WHERE b.numrut_emp = p_numrut_emp
         AND EXTRACT(MONTH FROM b.fecha_boleta) = p_mes
         AND EXTRACT(YEAR  FROM b.fecha_boleta) = p_anno;
    RETURN ROUND(v_total);   -- DEVUELVE EL TOTAL REDONDEADO
END f_valor_comision_ventas;
/

-- TRIGGER
-- PARA LANZAR BOLETA
CREATE OR REPLACE TRIGGER trg_bol_ins  -- CREA O REEMPLZA EL LANZADOR PARA INSERTAR
AFTER INSERT ON BOLETA
FOR EACH ROW
BEGIN
    INSERT INTO COMISION_VENTA( -- INSERTAR LOS DATOS EN LA TABLA
        nro_boleta, 
        valor_comision)    -- INSERTA EL VALOR EN LA TABLA
    -- VALORES
    VALUES(
        :NEW.nro_boleta, 
        ROUND(:NEW.monto_boleta * 0.15)); -- CALCULO DEL 15% DE LA BOLETA NUEVA
END;
/

-- ACTUALIZAR UNA BOLETA
CREATE OR REPLACE TRIGGER trg_bol_upd    -- CREA O REEMPLAZA EL LANZADOR PARA ACTUALIZAR
AFTER UPDATE OF monto_boleta ON BOLETA
FOR EACH ROW
BEGIN
  IF :NEW.monto_boleta > :OLD.monto_boleta THEN
    UPDATE COMISION_VENTA  -- ACTUALIZA EL CALCULO
       SET valor_comision = ROUND(:NEW.monto_boleta * 0.15)  -- CALCULA EL % Y LO ACTUALIZA
     WHERE nro_boleta = :OLD.nro_boleta;
  END IF;
END;
/

-- ELIMINAR UNA BOLETA
CREATE OR REPLACE TRIGGER trg_bol_del       -- CREA O RREMPLAZA EL LANZADOR DE ELIMINACION
AFTER DELETE ON BOLETA
FOR EACH ROW
BEGIN
  DELETE FROM COMISION_VENTA 
  WHERE nro_boleta = :OLD.nro_boleta;  -- ELIMINA LA COMISON CALCULADA
END;
/

-- CALCLULO DE LOS HABERES 
CREATE OR REPLACE PROCEDURE prc_calc_haberes (
  p_fecha_proc       IN DATE,   -- FECHA DE PROCESO 
  p_pct_movilizacion IN NUMBER, -- % DE MOVILIZACIÓN a APLICAR
  p_valor_carga      IN NUMBER, -- VALOR CARGA FAMILIAR
  p_valor_colacion   IN NUMBER  -- COLACION FIJO
) IS
  v_err_msg VARCHAR2(2000);     -- PARA CAPTURAR MENSAJES DE ERROR
BEGIN
  -- VARIABLE DE MOVILIZACION
  pkg_remuneraciones.g_pct_movilizacion := p_pct_movilizacion;

  -- RECORRE A TODOS LOS EMPLEADOS
  FOR emp_rec IN (
    SELECT 
        numrut_emp, 
        sueldo_base_emp, 
        fecing_emp
    FROM empleado
     ORDER BY numrut_emp
  ) LOOP
    -- VARIABLES PARA LOS EMPLEADOS
    DECLARE
      v_annos       NUMBER;  -- AÑOS DE SERVICIO
      v_val_annos   NUMBER;  -- MONTO BONIFICACIÓN AÑOS
      v_val_cargas  NUMBER;  -- MONTO CARGAS FAMILIARES
      v_val_movil   NUMBER;  -- MONTO MOVILIZACION
      v_val_col     NUMBER;  -- MONTO COLACION
      v_val_com     NUMBER;  -- MONTO COMISIONES DE VENTAS
      v_total       NUMBER;  -- TOTAL HABERES
    BEGIN
      -- CALCULO AÑOS TRABAJAODOS
      v_annos := FLOOR(MONTHS_BETWEEN(p_fecha_proc, emp_rec.fecing_emp) / 12);

      -- LLAMAR FUNCION CALCULO DE ANTIGUEDAD
      v_val_annos := pkg_remuneraciones.f_calc_bonif_annos(
                       emp_rec.fecing_emp,
                       p_fecha_proc
                     );

      -- CALCULO DE LAS CARGAS FAMILIARES
      v_val_cargas := f_valor_carga_familiar(
                        emp_rec.numrut_emp,
                        p_valor_carga
                      );

      -- CALCULO DE LA MOVILIZACION
      v_val_movil := ROUND(emp_rec.sueldo_base_emp * p_pct_movilizacion / 100);

      -- COLACION REDONDEADO
      v_val_col := ROUND(p_valor_colacion);

      -- CALCULO COMISON DEL MES
      v_val_com := f_valor_comision_ventas(
                     emp_rec.numrut_emp,
                     EXTRACT(MONTH FROM p_fecha_proc),  -- EXTRAE MES DEL PRECESO
                     EXTRACT(YEAR  FROM p_fecha_proc)  -- ECTRAE AÑO DEL PROCESO
                   );

      -- SUMA PARA SABER EL TOTAL
      v_total := emp_rec.sueldo_base_emp
               + v_val_annos
               + v_val_cargas
               + v_val_movil
               + v_val_col
               + v_val_com;

      -- INSERCION EN LA TABLA DE HABER_CAL_MES
      INSERT INTO HABER_CALC_MES(
            numrut_emp, 
            mes_proceso, 
            anno_proceso,
            annos_trabajados, 
            valor_sueldo_base,
            valor_asig_annos, 
            valor_cargas_fam,
            valor_movilizacion, 
            valor_colacion,
            valor_com_ventas, 
            valor_tot_haberes
      ) VALUES (   -- VALORES
            emp_rec.numrut_emp,
            EXTRACT(MONTH FROM p_fecha_proc),
            EXTRACT(YEAR  FROM p_fecha_proc),
            v_annos,
            emp_rec.sueldo_base_emp,
            v_val_annos,
            v_val_cargas,
            v_val_movil,
            v_val_col,
            v_val_com,
            v_total
      );

    EXCEPTION
      WHEN OTHERS THEN
        v_err_msg := SQLERRM;
        -- INSERTAR LOS ERRORES EN LA TABLA
        INSERT INTO ERROR_PROCESO_REMUN(
            correl_error,  -- CORRELATIVO DEL ERROR
            rutina_error,  
            descrip_error  -- QUE DICE EL ERROR
        ) VALUES(  -- VALORES QUE SE INSERTAN
          SEQ_ERROR.NEXTVAL,
          'prc_calc_haberes',
          v_err_msg
        );
    END;
  END LOOP;

  COMMIT;  -- CONFIRMAR LOS CAMBIOS
END prc_calc_haberes;
/


------------------------------------------------------------------------------------------
-- PRUEBA DE LOS TRIGGER
BEGIN
  -- LIMPIAR LOS DATOS ANTERIORES
  DELETE FROM COMISION_VENTA WHERE nro_boleta = 28;
  DELETE FROM BOLETA         WHERE nro_boleta = 28;
  COMMIT;  -- CONFIRMA LOS CAMBIOS

  -- INSERTAR BOLETA 28
  INSERT INTO BOLETA(
    nro_boleta,   -- NUMERO DE BILETA
    fecha_boleta, -- FECHA DE LA BOLETA
    monto_boleta, -- MONTO DE LA BOLETA
    id_cliente,   -- IDE DEL CLIENTE
    numrut_emp    -- RUN EMPLEAOD
  ) VALUES ( -- VALORES A INSERTAR
    28,
    TO_DATE('10/07/2024','DD/MM/YYYY'),
    258999,
    3000,
    12456905
  );

  -- ACTUALIZAR UNA BOLETA
  UPDATE BOLETA
     SET monto_boleta = 558590
   WHERE nro_boleta = 24;

  -- ELIMINAR UNA BOLETA
  DELETE FROM BOLETA
        WHERE nro_boleta = 22;

  COMMIT;  -- finalmente confirma insert, update y delete
END;
/


-- PRUEBA DE LOS HABERES
BEGIN
  prc_calc_haberes(
    TO_DATE('01/07/2024','DD/MM/YYYY'),
    35,     -- % movilización
    4500,   -- valor carga familiar
    62000   -- valor colación
  );
END;
/

-----------------------------------------------------------------------------------------------
-- CONSULTA DE LAS SEGUN LA REFERENCIA
-- BOLETAS DE JULIO
SELECT 
    b.nro_boleta,    -- NUMERO DE BOLETA
    TO_CHAR(b.fecha_boleta,'DD/MM/YYYY')    AS FECHA_BOLETA,
    b.monto_boleta,  -- MONTO
    b.id_cliente,    -- ID CLIENTE
    b.numrut_emp     -- RUN EMPLEADO
FROM boleta b
WHERE TRUNC(b.fecha_boleta,'MM') = TO_DATE('01/07/2024','DD/MM/YYYY')
ORDER BY b.nro_boleta;


-- COMISIONES DE ESAS BOLETAS
SELECT 
    c.nro_boleta,
    c.valor_comision
FROM comision_venta c
WHERE c.nro_boleta IN (
    SELECT b.nro_boleta
      FROM boleta b
     WHERE TRUNC(b.fecha_boleta,'MM') = TO_DATE('01/07/2024','DD/MM/YYYY')
)
ORDER BY c.nro_boleta;


-- CONSULTA DE LOS HABERES DE JULIO
SELECT 
  numrut_emp         AS NUMRUT_EMP,
  mes_proceso        AS MES_PROCESO,
  anno_proceso       AS ANNO_PROCESO,
  annos_trabajados   AS ANNOS_TRABAJADOS,
  valor_sueldo_base  AS VALOR_SUELDO_BASE,
  valor_asig_annos   AS VALOR_ASIG_ANNOS,
  valor_cargas_fam   AS VALOR_CARGAS_FAM,
  valor_movilizacion AS VALOR_MOVILIZACION,
  valor_colacion     AS VALOR_COLACION,
  valor_com_ventas   AS VALOR_COM_VENTAS,
  valor_tot_haberes  AS VALOR_TOT_HABERES
FROM haber_calc_mes
WHERE mes_proceso  = 7
  AND anno_proceso = 2024
ORDER BY numrut_emp;


-----------------------------------------------------------------------------------------------
















