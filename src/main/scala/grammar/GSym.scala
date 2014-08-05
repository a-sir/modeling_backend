package grammar

import play.api.libs.json.{JsString, JsNumber, Json, JsObject}
import utils.Keyable

/**
 * @author A.Sirenko
 *          Date: 9/8/13
 */
case class GSym(key: Int, name: String) extends Keyable[Int]{
  override def getKey: Int = key
  override def toString = name

  def toJson: JsObject = Json.obj("key" -> key, "name" -> name)
}

object GSym {

  def fromJson(obj: JsObject): GSym = new GSym(
    obj.\("key").as[JsNumber].value.intValue(),
    obj.\("name").as[JsString].value
  )

}