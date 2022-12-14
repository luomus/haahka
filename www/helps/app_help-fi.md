Tämä verkkosovellus on tehty [R-kielellä](https://cran.r-project.org/index.html) ja erityisesti [Shiny-paketin](https://shiny.rstudio.com) avulla. Sovelluksen lähdekoodi on avoimesti lisenssöity (MIT-lisenssi) ja se löytyy [GitLab-palvelusta](https://gitlab.com/tringa-ry/halias-browser).

### Ohjeita

#### Tarkasteltavan lajin valitseminen

Halutun lajin voi valita seuraavin tavoin:

1. Valitsemalla oikea laji pudotusvalikosta
2. Klikkaamalla hiirellä kerran lajin valintakentää, painamalla kerran 
   askelpalautinta ("backspace") ja alkamalla sitten kirjoittaa lajin nimeä
   (joko valitun kielen mukainen tai tieteellinen nimi). Valikko ehdottaa
   automaattisesti kirjoitetun perusteella sopivaa nimeä.

#### Linkki tiettyyn lajiin

Sovellus luo automaattisesti selaimen osoitekentässä olevaa URL-linkin parametrit, jotka toimivat tunnisteina valitulle lajille ja kielelle. Kopioimalla osoitekentässä olevan URL-linkin voit siis viitata suoraan haluamaasi lajiin (ja kieleen). Esim. seuraava linkki: 

[https://haahka.halias.fi/?\_inputs\_&language="fi"&species="Somateria mollissima"](https://haahka.halias.fi/?_inputs_&language="fi"&species="Somateria mollissima")
vie suoraan haahkasta kertovalle suomenkieliseslle sivulle.

### Palaute

Löysitkö ohjelmasta bugin? Eikö jokin ominaisuus toimi toivotulla tavalla? Tuliko sinulle mieleen jokin parannusehdotus? Ei hätää, tapoja osallistua sovelluksen kehittämiseen on monia:

1. Toteuta korjaus tai uusi omainaisuus itse ja jätä [GitLab-palvelussa](https://gitlab.com/tringa-ry/halias-browser) pull request.
2. [Avaa uusi huomio](https://gitlab.com/tringa-ry/halias-browser/issues/new?issue%5Bassignee_id%5D=&issue%5Bmilestone_id%5D=) (“issue”) GitLab-palvelussa ja kuvaa havaitesemasi bugi tai toive uudesta ominaisuudesta siellä.
3. Lähetä meiliä osoitteeseen <halias@halias.fi>
