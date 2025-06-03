/*
SUMATIVA NUMERO 1
LILIAN ZAPATA
PROGRAMACION BASE DE DATOS
2/6/25
*/

-- SE CARGARON Y BORRARON LAS TABLAS, SOLO PARA VER LA PROGRAMACION

-- MOSTRAR USUARIO
SHOW USER;

----------------------------------------

-- VARIABLES BIND REQUERIDAS Y ASIGNACION DE SU VALOR
VAR b_anio NUMBER;
EXEC :b_anio := EXTRACT(YEAR FROM SYSDATE);

VAR b_maria_pinto NUMBER;
EXEC :b_maria_pinto := 20000;

VAR b_curacavi NUMBER;
EXEC :b_curacavi := 25000;

VAR b_talagante NUMBER;
EXEC :b_talagante := 30000;

VAR b_el_monte NUMBER;
EXEC :b_el_monte := 35000;

VAR b_buin NUMBER;
EXEC :b_buin := 40000;

-- DECLARACION DE LAS VARIABLES
DECLARE                                     
    v_id_emp EMPLEADO.id_emp%TYPE;          -- VARIABLE ID SEGUN EL TIPO DE DATOS DE ID EMPLEADO DE LA TABLA EMPLEADOS
    v_run EMPLEADO.numrun_emp%TYPE;         -- VARIABLE RUN SEGUN EL TIPO DE DATOS DE NUMRUN DE LA TABLA EMPLEADO
    v_dv EMPLEADO.dvrun_emp%TYPE;           -- VARIABLE DV SEGUN EL TIPO DE DATOS DE DV DE LA TABLA EMPLEADO
    v_nombre VARCHAR2(200);
    v_comuna COMUNA.nombre_comuna%TYPE;     -- VARIABLE COMUNA SEGUN EL TIPO DE DATOS DE COMUNA DE LA TABLA COMUNA
    v_sueldo EMPLEADO.sueldo_base%TYPE;     -- VARIABLE SUELDO SEGUN EL TIPO DE DATOS DE SUELDO BASE DE LA TABLA EMPLEADO
    v_porc_movil NUMBER;
    v_valor_movil NUMBER;
    v_valor_extra NUMBER;
    v_valor_total NUMBER;
    v_contador NUMBER := 0;
    v_total_empleados NUMBER;
    
BEGIN
    -- TRUNCAR LA TABLA PARA COMENZAR CON LA TABAL DESDE CERO
    EXECUTE IMMEDIATE 'TRUNCATE TABLE PROY_MOVILIZACION';

    -- BUCLE PARA RECORRER A TODOS LOS EMPLEADOS
    FOR registro IN (
        -- DATOS NECESARIOS DE LOS EMPLEADOS
        SELECT                                  
            emp.id_emp, 
            emp.numrun_emp, 
            emp.dvrun_emp,
            emp.pnombre_emp, 
            emp.snombre_emp, 
            emp.appaterno_emp, 
            emp.apmaterno_emp,
            emp.sueldo_base, 
            com.nombre_comuna
        FROM EMPLEADO emp
        -- JOIN DE LA TABLA COMUNA CON LA TABLA EMPLEADO MEDIANTE EL ID DE COMUNA
        JOIN COMUNA com ON emp.id_comuna = com.id_comuna 
        -- ORDEN EN QUE SE VEN LOS DATOS
        ORDER BY emp.id_emp                      
    )
    
    LOOP                    
        v_id_emp := registro.id_emp;
        v_run := registro.numrun_emp;
        v_dv := registro.dvrun_emp;
        
        -- CONCATENACION DEL NOMBRE CON LETRA CAPITAL
        v_nombre := INITCAP(NVL(registro.pnombre_emp, '')) || ' ' ||
                    INITCAP(NVL(registro.snombre_emp, '')) || ' ' ||
                    INITCAP(NVL(registro.appaterno_emp, '')) || ' ' ||
                    INITCAP(NVL(registro.apmaterno_emp, ''));
        v_comuna := registro.nombre_comuna;
        v_sueldo := registro.sueldo_base;
        
        -- CALCULO DE POCENTAJE DE MOVILIZACION MORMAL
        v_porc_movil := TRUNC(v_sueldo / 100000);
        
        -- CALCULO SUELDO BASE POR EL PORCENTAJE DE MOVILIZACION
        v_valor_movil := TRUNC(v_sueldo * (v_porc_movil / 100));

        --CASE PARA VER LOS VALORES DE LAS COMUNAS EXTRAS
        v_valor_extra := CASE v_comuna
            WHEN 'María Pinto' THEN :b_maria_pinto
            WHEN 'Curacaví' THEN :b_curacavi
            WHEN 'Talagante' THEN :b_talagante
            WHEN 'El Monte' THEN :b_el_monte
            WHEN 'Buin' THEN :b_buin
            ELSE 0
        END;
        
        -- SUMA TOTAL DE VALOR NORMAL MAS EL VALOR EXTRA DE MOVILIZACION
        v_valor_total := v_valor_movil + v_valor_extra;

        -- INSERCION A LA TABLA PROY MOVILIZACION
        INSERT INTO PROY_MOVILIZACION (
            anno_proceso, 
            id_emp, 
            numrun_emp, 
            dvrun_emp, 
            nombre_empleado,
            nombre_comuna, 
            sueldo_base, 
            porc_movil_normal,
            valor_movil_normal, 
            valor_movil_extra, 
            valor_total_movil
        ) VALUES (     -- VALORES QUE SE INSERTAN
            :b_anio, 
            v_id_emp, 
            v_run, 
            v_dv, 
            v_nombre,
            v_comuna, 
            v_sueldo, 
            v_porc_movil,
            v_valor_movil, 
            v_valor_extra, 
            v_valor_total
        );
        
        -- SE SUMA 1 AL CONTADOR INICIADO EN CERO
        v_contador := v_contador + 1;
        
    END LOOP;  -- CIERRE DEL LOOP
    
    -- VALIDACION, SE CUENTAN TODOS LOS EMPLEADOS
    SELECT COUNT(*) INTO v_total_empleados FROM EMPLEADO;

    IF v_contador = v_total_empleados THEN
        COMMIT;         -- SI ESTA CORRECTO SE GUARDA
    ELSE
        ROLLBACK;       -- SI NO ESTA CORRECTO SE REVIERTE
    END IF;

    DBMS_OUTPUT.PUT_LINE('Total empleados procesados: ' || v_contador);
END;
/

-- CONSULTA DE VALIDACION PARA VER LA TABLA
SELECT * FROM PROY_MOVILIZACION ORDER BY id_emp;