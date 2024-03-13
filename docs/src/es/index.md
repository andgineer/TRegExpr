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

Las expresiones regulares son fáciles de usar y una herramienta poderosa para búsquedas y 
sustituciones sofisticadas, así como para la verificación de textos basada en plantillas.

Son especialmente útiles para la validación de entradas de usuarios en formularios - 
para validar direcciones de correo electrónico, etc.

También puedes extraer números de teléfono, códigos postales, etc., de páginas web o 
documentos, buscar patrones complejos en archivos de registros y todo lo que puedas 
imaginar. Las reglas (plantillas) pueden cambiarse sin necesidad de recompilar tu programa.

TRegExpr está implementado en Pascal puro. 
Está incluido en el proyecto [Lazarus (Free Pascal)](http://wiki.freepascal.org/Regexpr): 
[package](https://github.com/fpc/FPCSource/tree/main/packages/regexpr). 
Pero también existe como una biblioteca separada y puede ser compilada con Delphi 2-7, 
Borland C++ Builder 3-6.

[Que buena fue la biblioteca](https://sorokin.engineer/posts/en/regexpstudio_site_is_lunched.html)

# Inicio rápido

To use the library just add [the sources](https://github.com/andgineer/TRegExpr/blob/master/src/regexpr.pas)
to you project and use the class [TRegExpr](tregexpr.md).

En el [FAQ](faq.md) puedes aprender de los problemas de otros
usuarios.

Ready to run Windows application
[REStudio](https://github.com/andgineer/TRegExpr/releases/download/0.952b/restudio.zip)
will help you learn and debug regular expressions.

If you see any problems, please [create the bug](https://github.com/andgineer/TRegExpr/issues).

# Traducciones

La documentación ha sido traducida al
[inglés](https://regex.sorokin.engineer/) y al
[ruso](https://regexpr.sorokin.engineer/ru/).

Hay traducciones incompletas en varios otros idiomas. Si quieres ayudar a completarlas,
[contáctame](https://github.com/andgineer).

# Gratitud

Many features suggested and a lot of bugs founded (and even fixed) by
TRegExpr’s contributors.

I cannot list here all of them, but I do appreciate all bug-reports,
features suggestions and questions that I am receiving from you.

- [Alexey Torgashin](https://github.com/Alexey-T) - main contributor since 2019, e.g. 
- named groups, non-capturing groups, assertions, backward search and much more
- Guido Muehlwitz: error encontrado y arreglado en el procesamiento de
  cadenas grandes
- Stephan Klimek - testing in C++Builder and suggesting/implementing
  many features
- Steve Mudford - implementó el parámetro Offset
- Martin Baur ([www.mindpower.com](http://www.mindpower.com)) -German
  translation, usefull suggestions
- Yury Finkel - implemented Unicode support, found and fixed some bugs
- Ralf Junker - implemented some features, many optimization suggestions
- Simeon Lilov - Traducción Búlgaro
- Filip Jirsбk and Matthew Winter - help in implementation non-greedy
  mode
- Kit Eason - many examples for introduction help section
- Juergen Schroth - caza de errores y sugerencias útiles
- Martin Ledoux - Traducción al francés
- Diego Calp, Argentina - Traducción al Español
