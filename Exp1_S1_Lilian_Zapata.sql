-- SUMATICA 1
-- PROGRAMACION BASE DE DATOS
-- LILIAN ZAPATA

--CASO 1
CREATE OR REPLACE PROCEDURE calcular_beneficio (
    --PARAMETROS
    p_rut IN NUMBER,           -- NUMERO DE RUT SIN DIGITO VERIFICADOR
    p_dv IN VARCHAR2,          --  DIGITO VERIFICADOR DEL RUT
    p_peso_normal IN NUMBER,   -- VALOR BASE PARA CALCULO DE BENEFICIO
    p_limite1 IN NUMBER,       -- MULTIPLICADOR PARA BONO BAJO 1 MILLON
    p_limite2 IN NUMBER,       -- MULTIPLICADOR PARA BONO HASTA 3 MILLONES
    p_limite3 IN NUMBER        -- MULTIPLICADOR PARA BONO SOBRE 3 MILLONES
) AS
    -- VARIABLES
    v_nro_cliente NUMBER;              -- PARA GUARDAR EL NUMERO DE CLIENTE
    v_run VARCHAR2(10);                -- PARA GUARDAR EL RUN FORMATEADO
    v_nombre VARCHAR2(100);            -- PARA GUARDAR EL NOMBRE COMPLETO
    v_tipo_cliente VARCHAR2(100);      -- PARA GUARDAR EL TIPO DE CLIENTE
    v_total_cred NUMBER;               -- PARA GUARDAR TOTAL DE CREDITOS DEL CLIENTE
    v_cant_cienmil NUMBER;             -- PARA GUARDAR CUANTOS TRAMOS DE 100MIL TIENE
    v_beneficio NUMBER;                -- BENEFICIO CALCULADO SIN BONO
    v_bono_adicional NUMBER := 0;      -- BONO ADICIONAL CALCULADO SEGUN TIPO DE CLIENTE
    v_pesos_todsum NUMBER;             -- TOTAL SUMADO DEL BENEFICIO Y BONO ADICIONAL
BEGIN
    -- OBTENER INFORMACION DEL CLIENTE, NOMBRE, TIPO Y SUMA DE CREDITOS DEL AÑO ANTERIOR
    SELECT
        cli.nro_cliente,
        -- CONCATENACION DEL NOMBRE
        (TO_CHAR(cli.numrun) || '-' || cli.dvrun),
        (cli.pnombre || NVL2(cli.snombre, ' ' || cli.snombre, '') || cli.appaterno
             || NVL2(cli.apmaterno, ' ' || cli.apmaterno, '')),
        ticli.nombre_tipo_cliente,
        NVL(SUM(crcli.monto_solicitado), 0)   -- MANEJO DE NULOS
    INTO v_nro_cliente, v_run, v_nombre, v_tipo_cliente, v_total_cred
    FROM cliente cli                                                    -- DESDE TABLA CLIENTE
    JOIN tipo_cliente ticli ON cli.cod_tipo_cliente = ticli.cod_tipo_cliente  -- UNIR TIPLO CLIENTE CON CLIENTE, MEDIANTE EL COD DE TIPO CLIENTE
    LEFT JOIN credito_cliente crcli ON cli.nro_cliente = crcli.nro_cliente   --UNION DE LA TABLA CREDITO CLIENTE CON CLIENTE, SEGUN EL NUERO DE CLIENTE
        AND EXTRACT(YEAR FROM crcli.fecha_solic_cred) = EXTRACT(YEAR FROM SYSDATE) - 1 -- VER AÑO ANTERIOR SEGUN FECHA ACTUAL
    WHERE cli.numrun = p_rut AND cli.dvrun = p_dv  -- SE VE SEGUN EL NUMERO DE RUN Y SU DV
    GROUP BY  -- ORDENAR SEGUN LAS SIGUIENTES COLUMNAS
        cli.nro_cliente, 
        cli.numrun, 
        cli.dvrun,
        cli.pnombre, 
        cli.snombre, 
        cli.appaterno, 
        cli.apmaterno,
        ticli.nombre_tipo_cliente;

    -- CALCULAR CUANTOS TRAMOS DE 100 MIL HAY Y EL BENEFICIO TOTAL SIN BONO
    v_cant_cienmil := v_total_cred / 100000;
    v_beneficio := v_cant_cienmil * p_peso_normal;

    -- SI EL CLIENTE ES INDEPENDIENTE, CALCULAR BONO ADICIONAL SEGUN MONTO TOTAL
    IF UPPER(v_tipo_cliente) = UPPER('Trabajadores independientes') THEN
        IF v_total_cred <= 1000000 THEN 
            v_bono_adicional := v_cant_cienmil * p_limite1; -- BONO BAJO 1 MILLON
        ELSIF v_total_cred <= 3000000 THEN  
            v_bono_adicional := v_cant_cienmil * p_limite2; -- BONO ENTRE 1 Y 3 MILLONES
        ELSE 
            v_bono_adicional := v_cant_cienmil * p_limite3; -- BONO SOBRE 3 MILLONES
        END IF;
    END IF;

    -- SUMAR BENEFICIO Y BONO
    v_pesos_todsum := v_beneficio + v_bono_adicional;

    -- INSERTAR LOS DATOS CALCULADOS EN LA TABLA CLIENTE_TODOSUMA
    INSERT INTO CLIENTE_TODOSUMA 
    VALUES (v_nro_cliente, v_run, v_nombre, v_tipo_cliente, v_total_cred, v_pesos_todsum);
    
    -- MOSTRAR CONFIRMACION POR CONSOLA
    DBMS_OUTPUT.PUT_LINE('Insertado: ' || v_run || ' - ' || v_pesos_todsum);

END;
/

-- BLOQUE ANONIMO PARA LLAMAR EL PROCEDIMIENTO VARIAS VECES CON DISTINTOS RUN SOLICITADOS
BEGIN
    calcular_beneficio(22176845, '2', 1200, 100, 300, 550);
    calcular_beneficio(18858542, '6', 1200, 100, 300, 550);
    calcular_beneficio(21300628, '2', 1200, 100, 300, 550);
    calcular_beneficio(22558061, '8', 1200, 100, 300, 550);
END;
/

SHOW USER;



-- MUESTRA LA TABLA CON LOS DATOS DE LAS CONSULTAS
SELECT * FROM CLIENTE_TODOSUMA;


TRUNCATE TABLE CLIENTE_TODOSUMA;
-- PARA BORRAR
--DELETE FROM CLIENTE_TODOSUMA;
--COMMIT;

---------------------------------------------------------------------------

-- CASE 2 CICLOS FOR

DECLARE -- INICIAR EL BLOQUE ANONIMO

    -- DEFINICION DE TIPO DE REGISTRO PARA GUARDAR DATOS DE CLIENTES Y CREDITOS
    TYPE t_cliente IS RECORD (
        nro_cliente        NUMBER,           -- NUMERO DEL CLIENTE
        nro_solic_credito  NUMBER,           -- NUMERO DE SOLICITUD DE CREDITO
        cuotas_postergar   NUMBER            -- CANTIDAD DE CUOTAS A POSTERGAR
    );

    -- DEFINICION DE UNA TABLA  PARA ALMACENAR CLIENTES
    TYPE t_clientes IS TABLE OF t_cliente INDEX BY PLS_INTEGER;
    v_clientes t_clientes; -- VARIABLE DE TABLA

    -- VARIABLES 
    v_tipo_credito         VARCHAR2(30);  -- TIPO DE CREDITO 
    v_max_cuota            NUMBER;        -- ULTIMO NUMERO DE CUOTA EXISTENTE
    v_fecha_max            DATE;          -- FECHA DE LA ULTIMA CUOTA EXISTENTE
    v_valor_cuota          NUMBER;        -- VALOR DE LA ULTIMA CUOTA
    v_nueva_cuota          NUMBER;        -- NUMERO DE LA NUEVA CUOTA POSTERGADA
    v_fecha_nueva          DATE;          -- FECHA DE VENCIMIENTO DE LA NUEVA CUOTA
    v_valor_nueva_cuota    NUMBER;        -- VALOR DE LA NUEVA CUOTA CALCULADA
    v_creditos_anteriores  NUMBER;        -- CANTIDAD DE CREDITOS DEL AÑO ANTERIOR

BEGIN
    -- ASIGNAR CLIENTES A PROCESAR
    v_clientes(1) := t_cliente(5, 2001, 2);   --  SEBASTIAN
    v_clientes(2) := t_cliente(67, 3004, 1);  --  KAREN
    v_clientes(3) := t_cliente(13, 2004, 1);  --  JULIAN

    -- RECORRER CADA CLIENTE EN LA TABLA
    FOR i IN 1 .. v_clientes.COUNT LOOP

        -- OBTENER EL TIPO DE CREDITO PARA LA SOLICITUD ACTUAL
        SELECT cr.nombre_credito
        INTO v_tipo_credito
        FROM credito_cliente crcli
        JOIN credito cr ON crcli.cod_credito = cr.cod_credito
        WHERE crcli.nro_solic_credito = v_clientes(i).nro_solic_credito;

        -- OBTENER LA ULTIMA CUOTA Y SU FECHA PARA ESE CREDITO
        SELECT MAX(nro_cuota), MAX(fecha_venc_cuota)
        INTO v_max_cuota, v_fecha_max
        FROM cuota_credito_cliente
        WHERE nro_solic_credito = v_clientes(i).nro_solic_credito;

        -- OBTENER EL VALOR DE LA ULTIMA CUOTA EXISTENTE
        SELECT valor_cuota
        INTO v_valor_cuota
        FROM cuota_credito_cliente
        WHERE nro_solic_credito = v_clientes(i).nro_solic_credito AND nro_cuota = v_max_cuota;

        -- REVISAR SI EL CLIENTE TUVO MÁS DE UN CREDITO EN EL AÑO ANTERIOR
        SELECT COUNT(*)
        INTO v_creditos_anteriores
        FROM credito_cliente
        WHERE nro_cliente = v_clientes(i).nro_cliente
        AND EXTRACT(YEAR FROM fecha_solic_cred) = EXTRACT(YEAR FROM SYSDATE) - 1;

        -- SI TUVO MAS DE UN CRÉDITO, CONDONAR LA ULTIMA CUOTA
        IF v_creditos_anteriores > 1 THEN
            UPDATE cuota_credito_cliente
            SET fecha_pago_cuota = fecha_venc_cuota,  -- PONER FECHA DE PAGO IGUAL A FECHA DE VENCIMIENTO
                monto_pagado = valor_cuota            -- PONER MONTO PAGADO IGUAL AL VALOR DE LA CUOTA
            WHERE nro_solic_credito = v_clientes(i).nro_solic_credito
            AND nro_cuota = v_max_cuota;
        END IF;

        -- AGREGAR NUEVAS CUOTAS POSTERGADAS
        FOR j IN 1 .. v_clientes(i).cuotas_postergar LOOP
            v_nueva_cuota := v_max_cuota + j;               -- NUEVO NUMERO DE CUOTA
            v_fecha_nueva := ADD_MONTHS(v_fecha_max, j);   -- NUEVA FECHA DE VENCIMIENTO (MES SIGUIENTE)

            -- CALCULAR EL VALOR DE LA NUEVA CUOTA SEGUN EL TIPO DE CRÉDITO
            IF v_tipo_credito = 'Crédito Hipotecario' THEN
                IF v_clientes(i).cuotas_postergar = 1 THEN
                    v_valor_nueva_cuota := v_valor_cuota;  -- SIN INTERES SI POSTERGA SOLO 1
                ELSE
                    v_valor_nueva_cuota := v_valor_cuota * 1.005; -- 0.5% DE INTERES SI SON 2 CUOTAS
                END IF;
            ELSIF v_tipo_credito = 'Crédito de Consumo' THEN
                v_valor_nueva_cuota := v_valor_cuota * 1.01;  -- 1% DE INTERES
            ELSIF v_tipo_credito = 'Crédito Automotriz' THEN
                v_valor_nueva_cuota := v_valor_cuota * 1.02;  -- 2% DE INTERES
            ELSE
                v_valor_nueva_cuota := v_valor_cuota;         -- CUALQUIER OTRO INTERES
            END IF;

            -- INSERTAR LA NUEVA CUOTA EN LA TABLA
            INSERT INTO cuota_credito_cliente (
                nro_solic_credito, nro_cuota, fecha_venc_cuota, valor_cuota,
                fecha_pago_cuota, monto_pagado, saldo_por_pagar, cod_forma_pago)
            VALUES (
                v_clientes(i).nro_solic_credito,
                v_nueva_cuota,
                v_fecha_nueva,
                ROUND(v_valor_nueva_cuota),
                NULL, NULL, NULL, NULL); -- DEJAR CAMPOS DE PAGO VACIOS
        END LOOP; -- FIN FOR INTERNO (CUOTAS)
    END LOOP; -- FIN FOR DE CLIENTES

    COMMIT; -- CONFIRMAR CAMBIOS
END;
/

SELECT * FROM CUOTA_CREDITO_CLIENTE ORDER BY nro_solic_credito, nro_cuota;


