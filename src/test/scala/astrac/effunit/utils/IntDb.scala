package astrac.effunit
package utils

import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

object IntDb {
  case class Connection(id: Int)

  sealed trait DbCommand
  case class DbSet(key: String, value: Int) extends DbCommand
  case class DbGet(key: String) extends DbCommand
}

class IntDb {
  import IntDb._

  val internalMap: MutableMap[String, Int] = MutableMap.empty
  val commands: MutableMap[Connection, Vector[DbCommand]] = MutableMap.empty
  var connections: MutableSet[Connection] = MutableSet.empty
  var connectionCounter = 0

  def connect(): Connection = {
    val c = Connection(connectionCounter)
    connections.add(c)
    connectionCounter = connectionCounter + 1
    c
  }

  def close(conn: Connection): Unit = {
    connections.remove(conn)
    ()
  }

  def set(conn: Connection, key: String, value: Int): Unit = {
    if (!connections.contains(conn))
      throw new RuntimeException("Invalid connection")

    internalMap.update(key, value)
    commands.update(
      conn,
      commands.getOrElse(conn, Vector.empty) :+ DbSet(key, value)
    )
  }

  def get(conn: Connection, key: String): Option[Int] = {
    if (!connections.contains(conn))
      throw new RuntimeException("Invalid connection")

    commands.update(
      conn,
      commands.getOrElse(conn, Vector.empty) :+ DbGet(key)
    )
    internalMap.get(key)
  }
}

