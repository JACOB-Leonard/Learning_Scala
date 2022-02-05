object stat:

/*Exercice 2*/
	def afficherTab(tab: Array[Int]): Unit=
		for vale <- tab do
			println(vale)

/*Exercice 3*/

	def indice(tab: Array[Int], v: Int): Int=
		var i = 0
		for vale <- tab do
			if vale == v then
				return i
			i+= 1
		return -1
		
/*Exercice 4*/
	def effectifTotal(tEffectifs: Array[Int] ): Int=
		var somme = 0
		for vale <- tEffectifs do
			somme+=vale
			
		return somme

/*Exercice 5*/		
	def effectif(tVAleurs: Array[Int], tEffectifs: Array[Int],v: Int): Int=
		var ind = indice(tVAleurs, v)
		
		if ind != -1 then
			return tEffectifs(ind)
		else
			return -2
			
/*Exercice 6*/			
	def effectifCumuleCroissant(tValeurs: Array[Int], tEffectifs: Array[Int],v: Int): Int=
		var ind = indice(tValeurs, v)
		if ind == -1 then
			return ind
		else
			var somme = 0
			for n <- 0 to ind do
				somme += tEffectifs(n)
			return somme
			
/*Exercice 7*/			
	def moyenne(tValeurs: Array[Int], tEffectifs: Array[Int]): Double=
			var moyenne = 0
			for valTab <- tValeurs do
				moyenne = valTab * tEffectifs(indice(tValeurs, valTab))
			return moyenne/effectifTotal(tEffectifs)
			
/*Exercice 8*/			
	def nIEmeValeur(tValeurs: Array[Int], tEffectifs: Array[Int],n: Int): Int=
		var effectifs = new Array[Int](effectifTotal(tEffectifs))
		var nb = 0
		for i <- 0 to (tValeurs.length -1) do
			for j <- 0 until tEffectifs(i) do
				effectifs(nb + j) = tValeurs(i)
			nb += tEffectifs(i)
		return effectifs(n-1)
	
/*Exercice 9*/
	def mediane(tValeurs: Array[Int], tEffectifs: Array[Int],n: Int): Double=
		var effectifs = new Array[Int](effectifTotal(tEffectifs))
		var nb = 0
		for i <- 0 to (tValeurs.length -1) do
			for j <- 0 until tEffectifs(i) do
				effectifs(nb + j) = tValeurs(i)
			nb += tEffectifs(i)
		if effectifs.length % 2 == 0 then
			return (effectifs(effectifs.length/2)-1)+effectifs(effectifs.length/2).toFloat/2
		else
			return effectifs((effectifs.length * 0.25).toInt -1)
	
/*Exercice 10*/

	def premierQuartile(tValeurs: Array[Int], tEffectifs: Array[Int],n: Int): Int=
		var effectifs = new Array[Int](effectifTotal(tEffectifs))
		var nb = 0
		for i <- 0 to (tValeurs.length -1) do
			for j <- 0 until tEffectifs(i) do
				effectifs(nb + j) = tValeurs(i)
			nb += tEffectifs(i)
		return effectifs((effectifs.length * 0.25).toInt -1)
			
	def main(args:Array[String]): Unit=
		val temps = Array(-5, -4, -3, -1, 0, 1, 2, 3, 4, 5)
		val effectif = Array(3, 2, 2, 3, 2, 3, 4, 5, 2, 3, 1)
		//afficherTab(temps)
		//println(indice(temps, 3))
		//println(effectif(temps, effectif, 0))
		//println(effectifTotal(temps))
		println(effectifCumuleCroissant(temps, effectif, 0))
		println(moyenne(temps, effectif))
		println(nIEmeValeur(temps, effectif, 15))
  
