import scala.io.Source

def ranking(): List[(Int, Int)] = {
  val src = Source.fromFile("test.txt")
  val linijki = src.getLines.toList.tail

  val odpowiedzi = linijki
    .map(
      linijka => linijka   // każdą linijkę zamieniamy na ją samą z następującymi modyfikacjami:
        .strip   // "czyści" linijkę (usuwa wszystkie znaki spacji, nowych linii, tabulacji itd. z KOŃCÓW stringa
        .split(" ")   // dzielimy linijkę po spacjach
        .tail   // usuwamy pierwszy element (czyli tak naprawdę pierwszą kolumnę każdej linijki)
        .map(liczba => liczba.toInt)   // zamieniamy każdą z liczb na ich prawdziwe reprezentacje (np. string "23" na liczbę 23)
    )
  val polowaPoprawnychOdpowiedzi = (odpowiedzi.length.toDouble / 2.toDouble).ceil
  val iloscPytan = odpowiedzi.head.length   // bierzemy ilość odpowiedzi dla pierwszego uczestnika, aby uzyskać ilość pytań, na które odpowiada KAŻDY
  val sekwencjaIndeksow = (0 until iloscPytan).toList   // generujemy sekwencję od 0 do iloscPytan *WYŁĄCZNIE*
  val ilosciPoprawnychOdpowiedzi = sekwencjaIndeksow
    .map(   // liczymy dla każdego pytania (od 0 do 24, bo po indeksach, a nie po ludzku) ilość poprawnych odpowiedzi dla tego pytania
      indeksPytania => odpowiedzi.map(rzad => rzad(indeksPytania)).sum
    )

  val sekwencjaNumerowPytan = (1 to iloscPytan).toList
  val pytaniaPosortowanePoIlosciPoprawnychOdpowiedzi = (sekwencjaNumerowPytan zip ilosciPoprawnychOdpowiedzi)
    .filter(para => para._2 >= polowaPoprawnychOdpowiedzi)  // filtrujemy tak, żeby dostać tylko te pytania, na które odpowiedziało >=50% uczestników
    .groupBy(para => para._2)  // grupujemy pary (nrPytania, iloscOdpowiedzi) przez iloscOdpowiedzi
    /*    /\             /\
    W wyniku groupBy robi coś takiego:
      40 -> List((13, 40), (15, 40))
      38 -> List((9, 38))
    Dla takich pytań:
      nr 13 - 40 poprawnych odpowiedzi
      nr 15 - 40 poprawnych odpowiedzi
      nr 9 - 38 poprawnych odpowiedzi
    */
    .toList  // zamieniamy na listę, żeby móc posortować...
    .sortBy(para => para._1)  // ...posortować po ilości poprawnych odpowiedzi (czyli po kluczach, które zrobił groupBy)
    .reverse  // odwracamy żeby mieć kolejność malejącą
    .map(para => para._2)  // mapujemy następująco
    /*
    jeśli groupBy wyżej zwrócił taką mapę:
      40 -> List((13, 40), (15, 40))
      38 -> List((9, 38))
    to toList na mapie zwrócił to:
      List(
        ( 40, List((13, 40), (15, 40)) )
        ( 38, List((9, 38)) )
      )
    a więc powyższy map zwraca to:
      List(
        List((13, 40), (15, 40)),
        List((9, 38))
      )
    */
    .map(podlista => podlista.map(para => para._1)) // zamieniamy podlisty na takie, żeby zawierały tylko numer pytania
    /*
    jeśli wyżej było to:
      List(
        List((13, 40), (15, 40)),
        List((9, 38))
      )
    to ten map zwróci to:
      List(
        List(13, 15),
        List(9)
      )
    */

  val sekwencjaNumerowNaPodium = (1 to pytaniaPosortowanePoIlosciPoprawnychOdpowiedzi.length).toList
  val posortowanePytaniaZNumeramiNaPodium = (sekwencjaNumerowNaPodium zip pytaniaPosortowanePoIlosciPoprawnychOdpowiedzi)

  /**
   * Powyższy zip zrobi coś takiego dla poprzednich przykładowych danych:
   * List(
   *    List(13, 15),
   *    List(9)
   * )
   *
   * ||
   * ||
   * \/
   *
   * List(
   *    (1, List(13, 15)),
   *    (2, List(9))
   * )
   */

  posortowanePytaniaZNumeramiNaPodium.foldLeft(Nil: List[List[(Int, Int)]])(
    (acc, elem) => {
      val numerNaPodium = elem._1  // np. 1, 2
      val numeryPytanNaTenNumerNaPodium = elem._2  // np. List(13, 15), List(9)
      val toCoTrafiaDoAkumulatora = numeryPytanNaTenNumerNaPodium.map(numerPytania => (numerNaPodium, numerPytania))

      toCoTrafiaDoAkumulatora :: acc
    }
  )
  .reverse
  .flatten
}

@main
def zadanie_01(): Unit = {
  ranking()
}