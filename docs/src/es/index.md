<table>
  <tr>
    <td><a href="https://regex.sorokin.engineer/">English</a></td>
    <td><a href="https://regex.sorokin.engineer/ru/">Русский</a></td>
    <td><a href="https://regex.sorokin.engineer/de/">Deutsch</a></td>
    <td><a href="https://regex.sorokin.engineer/bg/">Български</a></td>
    <td><a href="https://regex.sorokin.engineer/fr/">Français</a></td>
    <td>Español</td>
  </tr>
</table>

# Introducción

La biblioteca TRegExpr implementa [expresiones regulares](regular_expressions.md).

Las expresiones regulares son una herramienta fácil de usar y poderosa para búsquedas y sustituciones sofisticadas, así como para la verificación de texto basada en plantillas.

Son especialmente útiles para la validación de entradas de usuarios en formularios - para validar direcciones de correo electrónico, etc.

También puedes extraer números de teléfono, códigos postales, etc., de páginas web o documentos, buscar patrones complejos en archivos de registro y todo lo que puedas imaginar. Las reglas (plantillas) pueden cambiarse sin necesidad de recompilar tu programa.

TRegExpr está implementado en Pascal puro. Está incluido en [Lazarus (Free Pascal)](http://wiki.freepascal.org/Regexpr): [paquete](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). Pero también existe como una biblioteca separada y puede ser compilada por Delphi 2-7, Borland C++ Builder 3-6.

[Cómo fue recibida la biblioteca](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html).

# Inicio rápido

Para usar la biblioteca, simplemente añade [las fuentes](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas) a tu proyecto y usa la clase [TRegExpr](tregexpr.md).

En el [FAQ](faq.md) puedes aprender de los problemas de otros usuarios.

La aplicación de Windows lista para ejecutar [REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip) te ayudará a aprender y depurar expresiones regulares.

Si ves algún problema, por favor [crea un informe de bug](https://github.com/andgineer/TRegExpr/issues).

# Traducciones

La documentación ha sido traducida al [inglés](https://regex.sorokin.engineer/) y al [ruso](https://regexpr.sorokin.engineer/ru/).

Hay traducciones incompletas a varios otros idiomas. Si quieres ayudar a completarlas, [contáctame](https://github.com/andgineer).

# Gratitud

Muchas características sugeridas y un montón de errores fueron encontrados (e incluso corregidos) por los contribuyentes de TRegExpr.

No puedo listarlos a todos aquí, pero aprecio todos los informes de errores, sugerencias de características y preguntas que recibo de ustedes.

- [Alexey Torgashin](https://github.com/Alexey-T) - principal contribuyente desde 2019, por ejemplo,
- grupos nombrados, grupos sin captura, afirmaciones, búsqueda hacia atrás y mucho más
- Guido Muehlwitz - encontró y corrigió un feo error en el procesamiento de cadenas grandes
- Stephan Klimek - pruebas en C++Builder y sugerencia/implementación de muchas características
- Steve Mudford - implementó el parámetro Offset
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) - traducción al alemán, sugerencias útiles
- Yury Finkel - implementó soporte Unicode, encontró y corrigió algunos errores
- Ralf Junker - implementó algunas características, muchas sugerencias de optimización
- Simeon Lilov - traducción al búlgaro
- Filip Jirsák y Matthew Winter - ayuda en la implementación del modo no ávido
- Kit Eason - muchos ejemplos para la sección de ayuda de introducción
- Juergen Schroth - búsqueda de errores y sugerencias útiles
- Martin Ledoux - traducción al francés
- Diego Calp, Argentina - traducción al español
