package grammar

import utils.Keyable
import java.util

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
class SymbolsMutable {

	var nameToSymbol: Map[String, GSym] = Map.empty

	var keyToSymbol: Map[Int, GSym] = Map.empty

	var counter = 0

	def getSymbol(key: Int): Option[GSym] = keyToSymbol.get(key)

	def getSymbol(name: String): Option[GSym] = nameToSymbol.get(name)

    def getSymbolsDictionary: java.util.HashMap[Integer, Keyable[Int]] = {
        val map = new util.HashMap[Integer, Keyable[Int]]()
        for (gsym <- keyToSymbol.values) {
            map.put(gsym.getKey, gsym)
        }
        map
    }

	def getOrCreateSymbol(name: String): GSym = {
		val current = getSymbol(name)
		if (current == Option.empty) {
			counter += 1
			val sym = new GSym(counter, name)
			nameToSymbol = nameToSymbol + (name -> sym)
			keyToSymbol = keyToSymbol + (counter -> sym)
			sym
		} else {
			current.get
		}
	}

	def getOrCreateSymbols(tokens: List[String]): List[GSym] =
		tokens.foldRight(List[GSym]())((v,l) => getOrCreateSymbol(v) :: l)

	def size = keyToSymbol.size
}
