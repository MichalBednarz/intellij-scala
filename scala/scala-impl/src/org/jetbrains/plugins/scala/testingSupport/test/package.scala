package org.jetbrains.plugins.scala.testingSupport

package object test {

  def ensure(bool: Boolean): Option[Unit] =
    if (bool) Some(()) else None
}
