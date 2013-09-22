package grammar

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class SymbolsMutable {

	var nameToSymbol: Map[String, Symbol] = Map.empty

	var keyToSymbol: Map[Int, Symbol] = Map.empty

	var counter = 0

	def getSymbol(key: Int): Option[Symbol] = keyToSymbol.get(key)

	def getSymbol(name: String): Option[Symbol] = nameToSymbol.get(name)

	def getOrCreateSymbol(name: String): Symbol = {
		val current = getSymbol(name)
		if (current == Option.empty) {
			counter += 1
			val sym = new Symbol(counter, name)
			nameToSymbol = nameToSymbol + (name -> sym)
			keyToSymbol = keyToSymbol + (counter -> sym)
			sym
		} else {
			current.get
		}
	}

	def getOrCreateSymbols(tokens: List[String]): List[Symbol] =
		tokens.foldRight(List[Symbol]())((v,l) => getOrCreateSymbol(v) :: l)

	def size = keyToSymbol.size
}
