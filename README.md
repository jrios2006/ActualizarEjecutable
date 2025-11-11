# Actualizador de Ejecutables para Windows (Delphi XE7)

> **Herramienta para cerrar, reemplazar y relanzar ejecutables en ejecución**  
> Compatible con **Delphi XE7** – Windows 32/64 bits

---

## Descripción

Este proyecto implementa un **actualizador seguro** de aplicaciones Windows escrito en **Delphi XE7**. Permite:

- Detectar si un ejecutable está en ejecución.
- Cerrarlo de forma segura (con confirmación opcional).
- Reemplazar el archivo `.exe` **incluso si está bloqueado**.
- Ejecutar la nueva versión automáticamente.

Ideal para:
- Actualizaciones automáticas
- Instaladores
- Sistemas de despliegue continuo

---

## Características

| Funcionalidad | Estado |
|-------------|--------|
| Detección de procesos por nombre | Done |
| Cierre forzado seguro | Done |
| Reemplazo de EXE en uso | Done |
| Confirmaciones interactivas | Done |
| Modo silencioso (sin diálogos) | Done |
| Reutilizable como unidad | Done |
| Documentación completa | Done |

---

## Estructura del Proyecto

ActualizadorDelphiXE7/
│
├── UpdaterUtils.pas        ← Unidad principal con lógica reutilizable
├── Unit1.pas               ← Formulario de prueba (opcional)
├── Unit1.dfm               ← Diseño del formulario
├── Project1.dpr            ← Archivo de proyecto principal
└── README.md               ← Documentación básica para implemtar en otros proyectos


---

## Requisitos

- **Delphi XE7** (o superior)
- Windows 7/8/10/11
- Ejecutar como **administrador** (para cerrar procesos)

---

## Cómo Usar

### 1. Desde Línea de Comandos (modo updater)

```bash
Actualizador.exe "C:\ruta\app.exe" "C:\ruta\app_nuevo.exe"
```

El programa:
* Cierra app.exe si está en ejecución
* Reemplaza el archivo
* Ejecuta la nueva versión
* Se cierra automáticamente

### 2. Desde el Botón de Prueba (interfaz gráfica)

1. Abre el proyecto en Delphi XE7
2. Compila y ejecuta (F9)
3. Pulsa "Probar Actualización"
4. Usa las rutas de ejemplo:

```bash
1_OLD.exe → 1.exe
```

### Uso Programático (en tu código)

```delphi
uses UpdaterUtils;

// Modo interactivo
UpdateExecutable('C:\miapp.exe', 'C:\miapp_nueva.exe');

// Modo silencioso (ideal para instaladores)
UpdateExecutable('C:\miapp.exe', 'C:\miapp_nueva.exe', False, False);
```

---

## Funciones Principales (UpdaterUtils.pas)

Función,Descripción
ProcessExists,Verifica si un EXE está en ejecución
TerminateProcessByPID,Cierra un proceso por PID
ReplaceExecutable,Reemplaza EXE en uso (técnica .old)
UpdateExecutable,Proceso completo con/sin confirmaciones

Técnica de Reemplazo Seguro

1. Renombra app.exe → app.exe.old (Windows lo permite aunque esté en uso)
2. Copia app_nuevo.exe → app.exe
3. Cierra el proceso original
4. Elimina app.exe.old

No se pierde el archivo original si falla el proceso

Compilación

1. Abre Project1.dpr en Delphi XE7
2. Presiona F9 o Build → Compile
3. El ejecutable se genera en:

```text
Win32\Debug\Actualizador.exe
```

Ejemplo de Uso en Instalador

```pascal
// Al final de la instalación
ShellExecute(0, 'open', 'Actualizador.exe',
  PChar('"C:\MiApp\MiApp.exe" "C:\Temp\MiApp_v2.exe"'),
  nil, SW_HIDE);
```

---

Notas

1. Ejecuta como administrador para cerrar procesos.
2. Usa rutas completas en parámetros.
3. Compatible con aplicaciones VCL y Console.
