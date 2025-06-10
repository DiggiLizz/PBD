/* FORMATIVA SEMANA 3
LILIAN ZAPATA
PD: NO PUEDO TRABAJAR CON NADIE, YA QUE LAS INSCRIPCIONES ESTAN CERRADAS EN GRUPOS DE 1
9-6-25
*/

SHOW USER;


-- CASO 1

-- VARIABLE BIND PARA PARAMETRIZAR AÑO
VAR b_anio NUMBER;
EXEC :b_anio := EXTRACT(YEAR FROM SYSDATE);

-- BLOQUE PARA TRUNCAR LA TABLA
BEGIN
    -- TRUNCAR LA TABLA AL INICIO SIEMPRE
    EXECUTE IMMEDIATE 'TRUNCATE TABLE pago_moroso';
END;
/

-- BLOQUE PRINCIPAL PARA PROCESAR DATOS DE PACIENTES MOROSOS
DECLARE
  -- CURSOR EXPLICITO CON NUEVOS ALIAS
  CURSOR cr_morosos IS
    SELECT
    -- COLUMNAS DE LA TABLA
      px.pac_run,
      px.dv_run,
      px.pnombre || ' ' || px.snombre || ' ' || px.apaterno || ' ' || px.amaterno AS nombre_completo,
      at.ate_id,
      pat.fecha_venc_pago,
      pat.fecha_pago,
      at.esp_id,
      esp.nombre AS "NOMBRE_ESPECIALIDAD",
      TRUNC(MONTHS_BETWEEN(SYSDATE, px.fecha_nacimiento) / 12) AS "EDAD"  --CONOCER LA EDAD 
    FROM atencion at
    JOIN paciente px ON px.pac_run = at.pac_run            -- UNION DE LA TABLA PACIENTE CON ATEMCION MEDIANTE RUN
    JOIN pago_atencion pat ON at.ate_id = pat.ate_id       -- UNION DE LA TABLA PAGO ATENCION CON ATENCION MEDIATE ATENCION ID
    JOIN especialidad esp ON at.esp_id = esp.esp_id        -- UNION DE LA TABLA ESPECILIADAD Y ATECION MEDIATE EL ESPECIALIDAD ID
    WHERE EXTRACT(YEAR FROM pat.fecha_pago) = :b_anio - 1  -- SELECCION PAGO AÑOS ANTERIOR A LA CONSULTA
      AND pat.fecha_pago > pat.fecha_venc_pago             -- SELECCOION DE PAGOS MOROSOS
    ORDER BY pat.fecha_venc_pago, px.apaterno;             -- ORDEN DE VISUALIZACION

  -- VARRAY CON VALORES DE MULTAS
  TYPE tp_multas IS VARRAY(7) OF NUMBER;
  v_multas tp_multas := tp_multas(1200, 1300, 1700, 1900, 1100, 2000, 2300);

  -- VARIABLES
  v_descuento NUMBER := 0;
  v_dias_mora NUMBER;
  v_multa_diaria NUMBER;
  v_total_multa NUMBER;

BEGIN
  -- BUCLE PARA RECORRER PACIENTES MOROSOS
  FOR r IN cr_morosos LOOP
    -- CALCULO DE DIAS DE MOROSIDAD
    v_dias_mora := r.fecha_pago - r.fecha_venc_pago;

    -- ASIGNACION DE MULTA SEGIN ESPECIALIDAD USANDO CASE
    v_multa_diaria := CASE
      WHEN r.esp_id IN (100, 300) THEN v_multas(1)
      WHEN r.esp_id = 200 THEN v_multas(2)
      WHEN r.esp_id IN (400, 900) THEN v_multas(3)
      WHEN r.esp_id IN (500, 600) THEN v_multas(4)
      WHEN r.esp_id = 700 THEN v_multas(5)
      WHEN r.esp_id = 1100 THEN v_multas(6)
      WHEN r.esp_id IN (1400, 1800) THEN v_multas(7)
      ELSE 0
    END;

    -- DESCUENTO POR TERCERA EDAD
    SELECT NVL(MAX(porcentaje_descto), 0)
    INTO v_descuento
    FROM porc_descto_3ra_edad
    WHERE r.edad BETWEEN anno_ini AND anno_ter;

    -- CALCULO DE MULTA TOTAL CON DESCUENTO
    v_total_multa := v_dias_mora * v_multa_diaria * (1 - (v_descuento / 100));

    -- INSERCION EN LA TABLA 
    INSERT INTO pago_moroso (
    -- COLUMNAS QUE SE INSERTAN
      pac_run, 
      pac_dv_run, 
      pac_nombre, 
      ate_id,
      fecha_venc_pago, 
      fecha_pago, 
      dias_morosidad,
      especialidad_atencion, 
      monto_multa
    ) VALUES (  -- VALORES DE LAS COLUMNAS
      r.pac_run, 
      r.dv_run, 
      r.nombre_completo, 
      r.ate_id,
      r.fecha_venc_pago, 
      r.fecha_pago, 
      v_dias_mora,
      r.nombre_especialidad, 
      ROUND(v_total_multa)
    );
  END LOOP;

  COMMIT;  -- GUARFAR LOS CAMBIOS
END;
/

--SELECT PARA VER LOS DATOS DE LA CONSULTA ANONIMA
SELECT * FROM pago_moroso;
-----------------------------------------------
--CASE 2

-- VARIABLE BIND
VAR b_anio NUMBER;
EXEC :b_anio := EXTRACT(YEAR FROM SYSDATE);

-- ELIMINAR LA TABLA Y SUS CONSTRAINTS 
BEGIN
  EXECUTE IMMEDIATE 'DROP TABLE MEDICO_SERVICIO_COMUNIDAD CASCADE CONSTRAINTS';
  -- CREACION DE TABLA NUEVA
  EXECUTE IMMEDIATE ' 
    CREATE TABLE MEDICO_SERVICIO_COMUNIDAD ( 
      id_med_scomun NUMBER GENERATED ALWAYS AS IDENTITY PRIMARY KEY,  
      unidad VARCHAR2(50),                                            
      run_medico VARCHAR2(15),                                        
      nombre_medico VARCHAR2(50),                                     
      correo_institucional VARCHAR2(40),                              
      total_aten_medicas NUMBER,                                      
      destinacion VARCHAR2(50)                                        
    )';
END;
/

-- BLOQUE PRINCIPAL
DECLARE
  -- CURSOR EXPLICITO
  CURSOR cr_medicos IS
    SELECT  -- me arrojba error si ponia los alias con comillas en mayuscula
      med.med_run,                                                                
      LPAD(med.med_run, 8, '0') || '-' || med.dv_run AS run_medico,
      med.pnombre || ' ' || med.apaterno || ' ' || med.amaterno AS nombre_medico,
      uni.nombre AS unidad,
      COUNT(at.ate_id) AS total_atenciones
    FROM medico med
    JOIN unidad uni ON med.uni_id = uni.uni_id        -- UNION DE LA TABLA UNIDAD CON MEDICO POR UNIDAD ID
    LEFT JOIN atencion at ON med.med_run = at.med_run -- UNION DE TABLA ATENCION CON MEDICO POR MEDIO DEL RUN DEL MEDICO
      AND EXTRACT(YEAR FROM at.fecha_atencion) = :b_anio - 1 -- SE VE LOS DEL AÑO PASADO
    --ORDEN DE LOS DATOS
    GROUP BY med.med_run, med.dv_run, med.pnombre, med.apaterno, med.amaterno, uni.nombre
    ORDER BY uni.nombre, med.apaterno;

  -- ARREGLO VARRAY DE DESTINOS
  TYPE tp_varray_destinos IS VARRAY(5) OF VARCHAR2(50);
  destinos tp_varray_destinos := tp_varray_destinos(
    'Servicio de Atencion Primaria de Urgencia (SAPU)',
    'Centros de Salud Familiar (CESFAM)',
    'Consultorios Generales',
    'Hospitales del area de la Salud Publica',
    'Sin asignacion'
  );

  -- VARIABLES AUXILIARES
  v_destino VARCHAR2(50);
  v_correo VARCHAR2(40);

BEGIN
  -- BUCLE PRINCIPAL
  FOR r IN cr_medicos LOOP
    -- ASIGNACION DE DESTINO SEGUN UNIDAD Y CANTIDAD DE ATENCIONES
    v_destino := CASE
      WHEN r.unidad = 'PSIQUIATRIA Y SALUD MENTAL' THEN destinos(2)
      WHEN r.unidad IN ('CARDIOLOGIA', 'ONCOLOGICA', 'PACIENTE CRONICO') THEN destinos(4)
      WHEN r.unidad IN ('ATENCION URGENCIA', 'TRAUMATOLOGIA ADULTO', 'CIRUGIA', 'CIRUGIA PLASTICA') AND r.total_atenciones <= 3 THEN destinos(1)
      WHEN r.unidad IN ('ATENCION URGENCIA', 'TRAUMATOLOGIA ADULTO', 'CIRUGIA', 'CIRUGIA PLASTICA') AND r.total_atenciones > 3 THEN destinos(4)
      WHEN r.unidad IN ('ATENCION AMBULATORIA', 'ATENCION ADULTO') THEN destinos(1)
      ELSE destinos(5)
    END;

    -- GENERACION DE CORREO INSTITUCIONAL
    v_correo := LOWER(
      SUBSTR(REPLACE(r.unidad, ' ', ''), 1, 2) ||
      SUBSTR(REPLACE(r.nombre_medico, ' ', ''), LENGTH(REPLACE(r.nombre_medico, ' ', '')) - 1, 2) ||
      SUBSTR(TO_CHAR(r.med_run), -3)
    ) || '@ketekura.cl';

    -- INSERCION DE DATOS
    INSERT INTO MEDICO_SERVICIO_COMUNIDAD (
    -- COLUMNAS DE LA TABLA
      unidad, 
      run_medico, 
      nombre_medico, 
      correo_institucional,
      total_aten_medicas, 
      destinacion
    ) VALUES ( -- VALORES DE LAS COLUMNAS
      r.unidad, 
      r.run_medico, 
      r.nombre_medico, 
      v_correo,
      r.total_atenciones, 
      v_destino
    );
  END LOOP;

  COMMIT;
END;
/

-- SELECT PARA VER LA TABLA
SELECT * FROM medico_servicio_comunidad;