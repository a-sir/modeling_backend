package grammar

import utils.Keyable

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
case class GSym(key: Int, name: String) extends Keyable[Integer]{
    override def getKey: Integer = key
}