El sistema de preguntes i respostes ha de permetre als usuaris les següents funcionalitats:

    1.Consultar la llista de temes oberts sobre els que es poden fer qüestions i respostes.
    Cada tema ha de tenir els següets atributs:
        1.el títol (text d'una sola línia),
        2.l'usuari que manté el tema (leader del tema)
        3.i la descripció del tema (text de múltiples línies).
    2.Si l'usuari s'ha autentificat i és el webmaster, afegir nous temes.
    En aquest cas, s'ha d'obrir un formulari que permeti introduir l'usuari que mantindrà el tema (el leader del tema), el títol i la descripció.
    3.Veure les preguntes i respostes realitzades sobre un tema determinat. Cada pregunta pot tenir vàries respostes associades.
    Cada pregunta ha de tenir els següets atributs:
        1.l'usuari que ha fet la pregunta,
        2.la data (dia i hora) en que s'ha fet la pregunta,
        3.l'assumpte de la pregunta (text d'una sola línia)
        4.i el contingut de la pregunta (text de múltiples línies).
    Cada resposta ha de tenir els següents atributs:
        1.l'usuari que ha fet la resposta,
        2.la data (dia i hora) en que s'ha fet la resposta
        3.i el contingut de la resposta (text de múltiples línies).
    4.Si l'usuari s'ha autentificat, afegir noves preguntes a un determinat tema.
    En aquest cas, s'ha d'obrir un formulari que permeti introduir l'assumpte i el contingut de la pregunta.
    5.Si l'usuari s'ha autentificat, afegir noves respostes a una determinada pregunta.
    En aquest cas, s'ha d'obrir un formulari que permeti introduir el contingut de la resposta.
    6.Si l'usuari s'ha autentificat i és el leader d'un determinat tema, modificar el tema.
    En aquest cas, s'ha d'obrir un formulari que permeti introduir el nou títol i/o la nova descripció.
    El leader d'un tema també ha de poder eliminar les preguntes o respostes que cregui que són inadeqüades per al tema.
    Si elimina una pregunta, automàticament quedaràn eliminades totes les seves respostes.

La autentificació de l'usuari es realitza amb un subsistema apart, de manera que un cop l'usuari s'hagi autentificat en el portal es mantindrà l'identificador de l'usuari durant tota la sessió
