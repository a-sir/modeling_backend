package util

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
object Collections {

	def toList[T](collection: java.util.Collection[T]): List[T] = {
		var list: List[T] = List.empty
		val iterator = collection.iterator
		while (iterator.hasNext) {
			list = list ::: List(iterator.next())
		}
		list
	}

}
